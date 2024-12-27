open Lexer
open Ast

type parser_error = { msg : string; line : int; col : int }

exception UnexpectedToken of parser_error

let print_parser_error = function
  | { msg; line; col } -> Printf.eprintf "%s - line %d column %d\n" msg line col

let parsing_error = 3

(** [expect_str_lit cur_token] @raises UnexpectedToken when parser expects one thing and receives another *)
let expect_str_lit (cur_token : Tokenizer.token) =
  match cur_token with
  | t -> (
      match t.token_type with
      | Tokenizer.StringLiteral s -> s
      | _ ->
          raise
            (UnexpectedToken
               {
                 msg = "expected string literal";
                 line = cur_token.line;
                 col = cur_token.col;
               }))

let expect_identifier (cur_token : Tokenizer.token) =
  match cur_token.token_type with
  | Tokenizer.Identifier id -> id
  | _ ->
      raise
        (UnexpectedToken
           {
             msg = "expected identifier";
             line = cur_token.line;
             col = cur_token.col;
           })

let get_identifier_opt cur_tokentype =
  match cur_tokentype with Tokenizer.Identifier id -> Some id | _ -> None

class parser tokenizer =
  object (self)
    val mutable cur_token : Tokenizer.token =
      try tokenizer#next_token
      with Lexer.Tokenizer.InvalidToken e ->
        Lexer.Tokenizer.print_tokenization_error e;
        exit parsing_error

    val mutable token_buffer = []

    method private next_token =
      if List.length token_buffer <> 0 then (
        cur_token <- token_buffer |> List.hd;
        token_buffer <- token_buffer |> List.tl)
      else cur_token <- tokenizer#next_token
    (** @raise InvalidToken if there is an invalid token encountered from tokenizer *)

    method private accept expected =
      List.exists
        (fun x -> Tokenizer.same_tokentype x cur_token.token_type)
        expected

    method private expect expected =
      let tmp = cur_token in
      if
        List.exists
          (fun x -> Tokenizer.same_tokentype x cur_token.token_type)
          expected
      then (
        self#next_token;
        tmp)
      else
        let _ = Printf.eprintf "Expected: " in
        List.iter (fun x -> Tokenizer.print_token_class x) expected;
        raise
          (UnexpectedToken
             {
               msg = "unexpected token";
               line = cur_token.line;
               col = cur_token.col;
             })

    method private lookahead steps =
      if steps <= 0 then cur_token
      else
        let rec loop () =
          if List.length token_buffer < steps then (
            token_buffer <- tokenizer#next_token :: token_buffer;
            loop ())
          else List.hd token_buffer
        in
        loop ()

    method private get_pos : Node.position = (cur_token.line, cur_token.col)

    method parse =
      try self#parse_program
      with UnexpectedToken e ->
        print_parser_error e;
        exit parsing_error

    method parse_program =
      let imports = self#parse_imports |> List.rev in
      let decls = self#parse_declarations |> List.rev in
      let _ = self#expect [ Tokenizer.EOF ] in
      Node.Program { imports; declarations = decls }

    method parse_imports =
      let rec get_imports imports =
        if self#accept [ Tokenizer.Import ] then (
          self#next_token;
          let import_str = expect_str_lit cur_token in
          let pos = self#get_pos in
          self#next_token;
          get_imports (Node.Import (import_str, pos) :: imports))
        else imports
      in
      get_imports []

    method parse_int_lit =
      match cur_token.token_type with
      | Tokenizer.IntLiteral i ->
          self#next_token;
          i
      | _ ->
          raise
            (UnexpectedToken
               {
                 msg = "expected int literal";
                 line = cur_token.line;
                 col = cur_token.col;
               })

    method parse_declarations =
      let rec aux acc =
        match cur_token.token_type with
        | Tokenizer.Let ->
            let vardecl = self#parse_var_decl in
            aux (vardecl :: acc)
        | Tokenizer.FnDecl ->
            let fn_decl = self#parse_fn_decl in
            aux (fn_decl :: acc)
        | _ -> acc
      in
      aux []

    method parse_var_decl =
      let var_decl_pos = self#get_pos in
      let _ = self#expect [ Let ] in
      let identifier = expect_identifier cur_token in
      self#next_token;

      (* parsing potential array declaration type *)
      let rec get_array_var acc =
        if self#accept [ Lsbr ] then (
          let arr_pos = self#get_pos in
          self#next_token;
          let size = self#parse_int_lit in
          let _ = self#expect [ Rsbr ] in
          get_array_var ((size, arr_pos) :: acc))
        else acc
      in

      let arr_sizes = get_array_var [] in
      let _ = self#expect [ Colon ] in
      let elem_type = ref self#parse_type in

      (* now that we have the element type check for array type decl *)
      let rec collect_array_type arr_sizes =
        match arr_sizes with
        | (size, pos) :: tl ->
            elem_type := ArrayType { typ = !elem_type; n_elems = size; pos };
            collect_array_type tl
        | [] -> ()
      in
      collect_array_type arr_sizes;

      (* checking for assignment *)
      if self#accept [ Assign ] then (
        self#next_token;
        (* assignment to anonymous function *)
        if self#accept [ FnDecl ] then
          let anon_fn = self#parse_anon_fn_decl in
          Node.VarDecl
            {
              name = identifier;
              typ = !elem_type;
              value = Some anon_fn;
              pos = var_decl_pos;
            }
        else
          let expr = self#parse_exp in
          Node.VarDecl
            {
              name = identifier;
              typ = !elem_type;
              value = Some expr;
              pos = var_decl_pos;
            })
      else
        Node.VarDecl
          {
            name = identifier;
            typ = !elem_type;
            value = None;
            pos = var_decl_pos;
          }

    method parse_fn_decl =
      let fn_decl_pos = self#get_pos in
      let _ = self#expect [ FnDecl ] in
      let name = expect_identifier cur_token in
      self#next_token;

      let _ = self#expect [ Lpar ] in

      (* parse parameters *)
      let incr_ret x =
        let tmp = !x in
        x := !x + 1;
        string_of_int tmp
      in
      let fn_param_placeholder = ref 0 in
      let rec parse_param_types t =
        match t with
        | Node.Fn fn ->
            Node.FnDecl
              {
                name = "_" ^ incr_ret fn_param_placeholder;
                params = List.map parse_param_types fn.param_types;
                block = None;
                typ = Node.Fn fn;
                pos = self#get_pos;
              }
        | ptyp ->
            Node.VarDecl
              {
                name = "_" ^ incr_ret fn_param_placeholder;
                typ = ptyp;
                value = None;
                pos = self#get_pos;
              }
      in

      let rec parse_params acc =
        if get_identifier_opt cur_token.token_type |> Option.is_some then (
          let param_name =
            get_identifier_opt cur_token.token_type |> Option.get
          in
          let param_pos = self#get_pos in
          self#next_token;
          let _ = self#expect [ Colon ] in
          let param_type = self#parse_type in
          let next_param =
            match param_type with
            | Node.Fn fn ->
                Node.FnDecl
                  {
                    name = param_name;
                    typ = param_type;
                    params = List.map parse_param_types fn.param_types;
                    block = None;
                    pos = param_pos;
                  }
            | _ ->
                Node.VarDecl
                  {
                    name = param_name;
                    typ = param_type;
                    value = None;
                    pos = param_pos;
                  }
          in

          (* see if comma coming and if so advance the buffer *)
          if self#accept [ Comma ] then
            if Tokenizer.is_identifier (self#lookahead 1).token_type then (
              self#next_token;
              parse_params (next_param :: acc))
            else
              raise
                (UnexpectedToken
                   {
                     msg =
                       "expected an identifier after comma in function \
                        declaration";
                     line = cur_token.line;
                     col = cur_token.col;
                   })
          else next_param :: acc)
        else acc
      in
      let params = parse_params [] |> List.rev in
      let _ = self#expect [ Rpar ] in
      let _ = self#expect [ ReturnType ] in
      let return_type = self#parse_type in
      let block = self#parse_block in
      let fn_decl =
        Node.FnDecl
          {
            name;
            params;
            block = Some block;
            typ = return_type;
            pos = fn_decl_pos;
          }
      in

      (* if block has a return stmt, include a reference to fn decl in return node *)
      (match block with
      | Node.Block x -> (
          match x.return_stmts with
          | Some return_stmts ->
              List.iter
                (fun ret_stmt ->
                  match ret_stmt with
                  | Node.ReturnStmt rx -> rx.fn_decl <- Some fn_decl
                  | _ -> ())
                return_stmts
          | _ -> ())
      | _ -> ());
      fn_decl

    method parse_anon_fn_decl =
      let fn_decl_pos = self#get_pos in
      let _ = self#expect [ FnDecl ] in
      let _ = self#expect [ Lpar ] in

      (* parse parameters *)
      let incr_ret x =
        let tmp = !x in
        x := !x + 1;
        string_of_int tmp
      in
      let fn_param_placeholder = ref 0 in
      let rec parse_param_types t =
        match t with
        | Node.Fn fn ->
            Node.FnDecl
              {
                name = "_" ^ incr_ret fn_param_placeholder;
                params = List.map parse_param_types fn.param_types;
                block = None;
                typ = Node.Fn fn;
                pos = self#get_pos;
              }
        | ptyp ->
            Node.VarDecl
              {
                name = "_" ^ incr_ret fn_param_placeholder;
                typ = ptyp;
                value = None;
                pos = self#get_pos;
              }
      in

      let rec parse_params acc =
        if get_identifier_opt cur_token.token_type |> Option.is_some then (
          let param_name =
            get_identifier_opt cur_token.token_type |> Option.get
          in
          let param_pos = self#get_pos in
          self#next_token;
          let _ = self#expect [ Colon ] in
          let param_type = self#parse_type in
          let next_param =
            match param_type with
            | Node.Fn fn ->
                Node.FnDecl
                  {
                    name = param_name;
                    typ = param_type;
                    params = List.map parse_param_types fn.param_types;
                    block = None;
                    pos = param_pos;
                  }
            | _ ->
                Node.VarDecl
                  {
                    name = param_name;
                    typ = param_type;
                    value = None;
                    pos = param_pos;
                  }
          in

          (* see if comma coming and if so advance the buffer *)
          if self#accept [ Comma ] then
            if Tokenizer.is_identifier (self#lookahead 1).token_type then (
              self#next_token;
              parse_params (next_param :: acc))
            else
              raise
                (UnexpectedToken
                   {
                     msg =
                       "expected an identifier after comma in function \
                        declaration";
                     line = cur_token.line;
                     col = cur_token.col;
                   })
          else next_param :: acc)
        else acc
      in
      let params = parse_params [] |> List.rev in
      let _ = self#expect [ Rpar ] in
      let _ = self#expect [ ReturnType ] in
      let return_type = self#parse_type in
      let block = self#parse_block in
      let fn_decl =
        Node.AnonFnDecl { params; block; typ = return_type; pos = fn_decl_pos }
      in

      (* if block has a return stmt, include a reference to fn decl in return node *)
      (match block with
      | Node.Block x -> (
          match x.return_stmts with
          | Some return_stmts ->
              List.iter
                (fun ret_stmt ->
                  match ret_stmt with
                  | Node.ReturnStmt rx -> rx.fn_decl <- Some fn_decl
                  | _ -> ())
                return_stmts
          | _ -> ())
      | _ -> ());
      fn_decl

    method parse_anonymous_fn_decl =
      let anon_pos = self#get_pos in
      let _ = self#expect [ FnDecl ] in
      let _ = self#expect [ Lpar ] in

      (* parse parameters *)
      let rec parse_params acc =
        if get_identifier_opt cur_token.token_type |> Option.is_some then (
          let param_name =
            get_identifier_opt cur_token.token_type |> Option.get
          in
          let param_pos = self#get_pos in
          self#next_token;
          let _ = self#expect [ Colon ] in
          let param_type = self#parse_type in

          (* see if comma coming and if so advance the buffer *)
          if self#accept [ Comma ] then
            if Tokenizer.is_identifier (self#lookahead 1).token_type then (
              self#next_token;
              parse_params
                (Node.VarDecl
                   {
                     name = param_name;
                     typ = param_type;
                     value = None;
                     pos = param_pos;
                   }
                :: acc))
            else
              raise
                (UnexpectedToken
                   {
                     msg =
                       "expected an identifier after comma in function \
                        declaration";
                     line = cur_token.line;
                     col = cur_token.col;
                   })
          else
            Node.VarDecl
              {
                name = param_name;
                typ = param_type;
                value = None;
                pos = param_pos;
              }
            :: acc)
        else acc
      in
      let params = parse_params [] in
      let _ = self#expect [ Rpar ] in
      let _ = self#expect [ ReturnType ] in
      let return_type = self#parse_type in
      let block = self#parse_block in
      let fn_decl =
        Node.AnonFnDecl { params; block; typ = return_type; pos = anon_pos }
      in

      (* if block has a return stmt, include a reference to fn decl in return node *)
      (match block with
      | Node.Block x -> (
          match x.return_stmts with
          | Some return_stmts ->
              List.iter
                (fun ret_stmt ->
                  match ret_stmt with
                  | Node.ReturnStmt rx -> rx.fn_decl <- Some fn_decl
                  | _ -> ())
                return_stmts
          | _ -> ())
      | _ -> ());

      fn_decl

    method parse_type : Node.t =
      if self#accept [ Tokenizer.Fn ] then (
        (* handle function type *)
        let fn_type_pos = self#get_pos in
        self#next_token;
        let _ = self#expect [ Tokenizer.Lpar ] in

        let rec get_param_types acc =
          if Tokenizer.is_type cur_token.token_type then
            let typ = self#parse_type in

            if
              self#accept [ Tokenizer.Comma ]
              && Tokenizer.is_type (self#lookahead 1).token_type
            then (
              self#next_token;
              get_param_types (typ :: acc))
            else if
              self#accept [ Tokenizer.Comma ]
              && not (Tokenizer.is_type (self#lookahead 1).token_type)
            then
              raise
                (UnexpectedToken
                   {
                     msg = "expected another type after comma in function type";
                     line = cur_token.line;
                     col = cur_token.col;
                   })
            else typ :: acc
          else acc
        in
        let param_types = get_param_types [] in
        let _ = self#expect [ Tokenizer.Rpar ] in
        let _ = self#expect [ Tokenizer.ReturnType ] in
        let return_type = self#parse_type in
        Node.Fn { param_types; return_type; pos = fn_type_pos })
      else
        let pos = self#get_pos in
        match cur_token.token_type with
        | Unit ->
            self#next_token;
            Node.Unit pos
        | Int ->
            self#next_token;
            Node.Int pos
        | Float ->
            self#next_token;
            Node.Float pos
        | Char ->
            self#next_token;
            Node.Char pos
        | String ->
            self#next_token;
            Node.String pos
        | Bool ->
            self#next_token;
            Node.Bool pos
        | _ ->
            raise
              (UnexpectedToken
                 {
                   msg = "expected a type";
                   line = cur_token.line;
                   col = cur_token.col;
                 })

    method parse_block =
      let pos = self#get_pos in
      let _ = self#expect [ Lbra ] in
      let stmts = self#parse_statements in
      let _ = self#expect [ Rbra ] in
      if not (stmts |> List.is_empty) then
        let return_stmts =
          List.filter
            (fun x -> match x with Node.ReturnStmt _ -> true | _ -> false)
            stmts
        in

        match return_stmts with
        | [] ->
            Node.Block { stmts; pos; return_stmts = None; typ = Node.Unit pos }
        | _lst ->
            Node.Block
              {
                stmts;
                pos;
                return_stmts = Some return_stmts;
                typ = Node.Unit pos;
              }
      else Node.Block { stmts; pos; return_stmts = None; typ = Node.Unit pos }

    method parse_statements =
      let rec parse_stmt (acc : Node.t list) : Node.t list =
        match cur_token.token_type with
        | Tokenizer.Lbra -> parse_stmt (self#parse_block :: acc)
        | Tokenizer.While ->
            let pos = self#get_pos in
            self#next_token;
            let loop_exp = self#parse_exp in
            let do_stmt = self#parse_statement in
            parse_stmt (Node.While { loop_exp; do_stmt; pos } :: acc)
        | Tokenizer.If ->
            let pos = self#get_pos in
            self#next_token;
            let cond_expr = self#parse_exp in
            let if_stmt = self#parse_statement in

            (* else block check *)
            if self#accept [ Else ] then (
              self#next_token;
              let else_stmt = self#parse_statement in
              parse_stmt
                (Node.If
                   { cond_expr; if_stmt; else_stmt_opt = Some else_stmt; pos }
                :: acc))
            else
              parse_stmt
                (Node.If { cond_expr; if_stmt; else_stmt_opt = None; pos }
                :: acc)
        | Tokenizer.Return ->
            let pos = self#get_pos in
            self#next_token;
            if
              Tokenizer.is_literal cur_token.token_type
              || self#accept [ Lpar; Minus; Plus; Lsbr ]
            then
              let return_stmt = self#parse_exp in
              parse_stmt
                (Node.ReturnStmt
                   { stmt_opt = Some return_stmt; pos; fn_decl = None }
                :: acc)
            else
              parse_stmt
                (Node.ReturnStmt { stmt_opt = None; pos; fn_decl = None } :: acc)
        | Tokenizer.Let ->
            let s = self#parse_var_decl in
            parse_stmt (s :: acc)
        | Tokenizer.FnDecl -> parse_stmt (self#parse_fn_decl :: acc)
        | Lpar | Identifier _ | IntLiteral _ | Minus | Plus | CharLiteral _
        | StringLiteral _ | FloatLiteral _ ->
            parse_stmt (self#parse_exp :: acc)
        | _ -> acc
      in
      parse_stmt [] |> List.rev

    method parse_statement =
      match cur_token.token_type with
      | Tokenizer.Lbra -> self#parse_block
      | Tokenizer.While ->
          let pos = self#get_pos in
          self#next_token;
          let loop_exp = self#parse_exp in
          let do_stmt = self#parse_statement in
          Node.While { loop_exp; do_stmt; pos }
      | Tokenizer.If ->
          let pos = self#get_pos in
          self#next_token;
          let cond_expr = self#parse_exp in
          let if_stmt = self#parse_statement in

          (* else block check *)
          if self#accept [ Else ] then (
            self#next_token;
            let else_stmt = self#parse_statement in
            Node.If { cond_expr; if_stmt; else_stmt_opt = Some else_stmt; pos })
          else Node.If { cond_expr; if_stmt; else_stmt_opt = None; pos }
      | Tokenizer.Return ->
          let pos = self#get_pos in
          self#next_token;
          if
            Tokenizer.is_literal cur_token.token_type
            || self#accept [ Lpar; Minus; Plus; Lsbr ]
          then
            let return_stmt = self#parse_statement in
            Node.ReturnStmt { stmt_opt = Some return_stmt; pos; fn_decl = None }
          else Node.ReturnStmt { stmt_opt = None; pos; fn_decl = None }
      | Tokenizer.Let -> self#parse_var_decl
      | Tokenizer.FnDecl -> self#parse_fn_decl
      | Lpar | Identifier _ | IntLiteral _ | Minus | Plus | CharLiteral _
      | BoolLiteral _ | StringLiteral _ | FloatLiteral _ ->
          self#parse_exp
      | _ ->
          raise
            (UnexpectedToken
               {
                 msg = "expected a statement";
                 line = cur_token.line;
                 col = cur_token.col;
               })

    method parse_exp : Node.t =
      let parse_e0 () =
        if self#accept [ Tokenizer.Lpar ] then (
          (* "(" <exp> ")" *)
          self#next_token;
          let expr = self#parse_exp in
          let _ = self#expect [ Tokenizer.Rpar ] in
          expr)
        else if
          get_identifier_opt cur_token.token_type |> Option.is_some
          && (self#lookahead 1).token_type = Tokenizer.Lpar
        then (
          (* function call *)
          let id = get_identifier_opt cur_token.token_type |> Option.get in
          let fn_call_pos = self#get_pos in
          self#next_token;
          let _ = self#expect [ Tokenizer.Lpar ] in

          (* parse arg list *)
          let params = ref [] in
          if self#accept [ Tokenizer.Rpar ] |> not then (
            let param_exp = self#parse_exp in
            params := param_exp :: !params;

            let rec loop_args () =
              if self#accept [ Tokenizer.Comma ] then (
                self#next_token;
                params := self#parse_exp :: !params;
                loop_args ())
              else ()
            in
            loop_args ();

            let _ = self#expect [ Tokenizer.Rpar ] in
            Node.FnCallExp
              {
                typ = None;
                name = id;
                args = !params;
                decl = None;
                pos = fn_call_pos;
              })
          else
            let _ = self#expect [ Tokenizer.Rpar ] in
            Node.FnCallExp
              {
                typ = None;
                name = id;
                args = !params;
                decl = None;
                pos = fn_call_pos;
              })
        else if Tokenizer.is_literal cur_token.token_type then (
          let pos = self#get_pos in
          match cur_token.token_type with
          | Identifier id ->
              self#next_token;
              Node.VarExp { name = id; decl = None; pos; typ = None }
          | StringLiteral s ->
              self#next_token;
              Node.StringLiteral { s; pos }
          | IntLiteral i ->
              self#next_token;
              Node.IntLiteral { i; pos }
          | FloatLiteral f ->
              self#next_token;
              Node.FloatLiteral { f; pos }
          | CharLiteral c ->
              self#next_token;
              Node.CharLiteral { c; pos }
          | BoolLiteral b ->
              self#next_token;
              Node.BoolLiteral { b; pos }
          | _ ->
              prerr_endline "unexpected match";
              exit parsing_error)
        else if self#accept [ Lsbr ] then (
          (* Array literal *)
          let pos = self#get_pos in
          self#next_token;

          let rec get_row row_acc =
            if not (self#accept [ SemiColon ]) then
              let expr = self#parse_exp in

              if self#accept [ Comma ] then (
                self#next_token;
                get_row (expr :: row_acc))
              else expr :: row_acc |> List.rev
            else row_acc |> List.rev
          in

          let rec get_arr rows_acc =
            let row = get_row [] in
            if List.is_empty row |> not && self#accept [ SemiColon ] then (
              self#next_token;
              get_arr (row :: rows_acc))
            else if not (List.is_empty row) then row :: rows_acc |> List.rev
            else rows_acc |> List.rev
          in

          let arr = get_arr [] in
          let _ = self#expect [ Rsbr ] in

          ArrayLiteral { arr; pos })
        else
          raise
            (UnexpectedToken
               {
                 msg = "unexpected token while parsing an expression";
                 line = cur_token.line;
                 col = cur_token.col;
               })
      in
      let parse_e1 () =
        let acc = ref (parse_e0 ()) in

        (* array access *)
        let rec array_loop () =
          if self#accept [ Tokenizer.Lsbr ] then (
            let pos = self#get_pos in
            self#next_token;
            let index_exp = self#parse_exp in
            let _ = self#expect [ Tokenizer.Rsbr ] in
            acc :=
              Node.ArrayAccessExp
                { typ = None; arr = !acc; index = index_exp; pos };
            array_loop ())
          else ()
        in
        array_loop ();
        !acc
      in
      let rec parse_e2 () =
        match cur_token.token_type with
        | Tokenizer.Plus ->
            let pos = self#get_pos in
            self#next_token;
            Node.BinOp
              {
                typ = None;
                op = Node.ADD;
                lhs = Node.IntLiteral { i = 0; pos };
                rhs = parse_e2 ();
                pos;
              }
        | Tokenizer.Minus ->
            let pos = self#get_pos in
            self#next_token;
            Node.BinOp
              {
                typ = None;
                op = Node.SUB;
                lhs = Node.IntLiteral { i = 0; pos };
                rhs = parse_e2 ();
                pos;
              }
        | Tokenizer.Lpar when (self#lookahead 1).token_type |> Tokenizer.is_type
          ->
            (* type cast expr *)
            let pos = self#get_pos in
            self#next_token;
            let new_type = self#parse_type in
            let _ = self#expect [ Rpar ] in
            let expr = parse_e2 () in
            TypeCastExp { typ = None; new_type; expr; pos }
        | _ -> parse_e1 ()
      in

      let parse_e3 () =
        let pos = self#get_pos in
        let lhs = parse_e2 () in

        let rec collect lhs =
          if self#accept [ Asterix; Div; Rem ] then
            let op =
              match (self#expect [ Asterix; Div; Rem ]).token_type with
              | Asterix -> Node.MUL
              | Div -> Node.DIV
              | Rem -> Node.MOD
              | _ ->
                  raise
                    (UnexpectedToken
                       {
                         msg = "unexpected token";
                         line = cur_token.line;
                         col = cur_token.col;
                       })
            in
            let rhs = parse_e2 () in
            collect (Node.BinOp { typ = None; op; lhs; rhs; pos })
          else lhs
        in
        collect lhs
      in
      let parse_e4 () =
        let pos = self#get_pos in

        let lhs = parse_e3 () in

        let rec collect lhs =
          if self#accept [ Plus; Minus ] then
            let op =
              match (self#expect [ Plus; Minus ]).token_type with
              | Plus -> Node.ADD
              | Minus -> Node.SUB
              | _ ->
                  raise
                    (UnexpectedToken
                       {
                         msg = "unexpected token";
                         line = cur_token.line;
                         col = cur_token.col;
                       })
            in
            let rhs = parse_e3 () in
            collect (Node.BinOp { typ = None; op; lhs; rhs; pos })
          else lhs
        in
        collect lhs
      in

      let parse_e5 () =
        let pos = self#get_pos in

        let lhs = parse_e4 () in

        let rec collect lhs =
          if self#accept [ Lt; Le; Gt; Ge ] then
            let op =
              match (self#expect [ Lt; Le; Gt; Ge ]).token_type with
              | Lt -> Node.LT
              | Le -> Node.LE
              | Gt -> Node.GT
              | Ge -> Node.GE
              | _ ->
                  raise
                    (UnexpectedToken
                       {
                         msg = "unexpected token";
                         line = cur_token.line;
                         col = cur_token.col;
                       })
            in
            let rhs = parse_e4 () in
            collect (Node.BinOp { typ = None; op; lhs; rhs; pos })
          else lhs
        in
        collect lhs
      in

      let parse_e6 () =
        let pos = self#get_pos in

        let lhs = parse_e5 () in

        let rec collect lhs =
          if self#accept [ Eq; Ne ] then
            let op =
              match (self#expect [ Eq; Ne ]).token_type with
              | Eq -> Node.EQ
              | Ne -> Node.NE
              | _ ->
                  raise
                    (UnexpectedToken
                       {
                         msg = "unexpected token";
                         line = cur_token.line;
                         col = cur_token.col;
                       })
            in
            let rhs = parse_e5 () in
            collect (Node.BinOp { typ = None; op; lhs; rhs; pos })
          else lhs
        in
        collect lhs
      in

      let parse_e7 () =
        let pos = self#get_pos in

        let lhs = parse_e6 () in

        let rec collect lhs =
          if self#accept [ LogAnd ] then
            let op =
              match (self#expect [ LogAnd ]).token_type with
              | LogAnd -> Node.AND
              | _ ->
                  raise
                    (UnexpectedToken
                       {
                         msg = "unexpected token";
                         line = cur_token.line;
                         col = cur_token.col;
                       })
            in
            let rhs = parse_e6 () in
            collect (Node.BinOp { typ = None; op; lhs; rhs; pos })
          else lhs
        in
        collect lhs
      in

      let parse_e8 () =
        let pos = self#get_pos in

        let lhs = parse_e7 () in

        let rec collect lhs =
          if self#accept [ LogAnd ] then
            let op =
              match (self#expect [ LogOr ]).token_type with
              | LogOr -> Node.OR
              | _ ->
                  raise
                    (UnexpectedToken
                       {
                         msg = "unexpected token";
                         line = cur_token.line;
                         col = cur_token.col;
                       })
            in
            let rhs = parse_e7 () in
            collect (Node.BinOp { typ = None; op; lhs; rhs; pos })
          else lhs
        in
        collect lhs
      in

      let pos = self#get_pos in
      let lhs = parse_e8 () in
      if self#accept [ Assign ] then (
        self#next_token;
        let rhs = self#parse_exp in
        Node.Assign { typ = None; lhs; rhs; pos })
      else lhs
  end
