let type_check_error = 5

exception BadTypeCast of string * Node.position
exception TypeError of string * Node.position
exception ParameterMismatch of string * Node.position
exception UnexpectedUnitType of string * Node.position
exception TypeMismatch of Node.t * Node.t * Node.position

class type_checker =
  object (self)
    inherit Node.ast_pass
    val mutable last_fndecl_type : Node.t option = None

    method visit (node : Node.t) =
      let rec get_type (node : Node.t) : Node.t =
        match node with
        | Node.Program { imports = _; declarations } ->
            List.iter self#visit declarations;
            Node.Unit (Node.get_position node)
        | Import _ -> Node.Unit (Node.get_position node)
        | Unit _ -> node
        | Int _ -> node
        | Float _ -> node
        | Char _ -> node
        | String _ -> node
        | Bool _ -> node
        | Fn _ -> node
        | ArrayType _ -> node
        | VarDecl x -> (
            (* if vd is assigned to anon fn then need to handle return value *)
            (match x.value with 
            | Some (Node.AnonFnDecl afd) -> (
                let tmp = last_fndecl_type in 
                last_fndecl_type <- Some afd.typ;
                List.iter self#visit afd.params;
                let _ = get_type afd.block in
                last_fndecl_type <- tmp;
            )
            | _ -> ());

            match x.typ with
            | Node.Unit _ -> raise (UnexpectedUnitType (x.name, x.pos))
            | _ -> x.typ)
        | FnDecl x ->
            last_fndecl_type <- Some x.typ;
            List.iter self#visit x.params;
            if x.block |> Option.is_some then
              let _ = get_type (x.block |> Option.get) in
              ()
            else ();
            x.typ
        | AnonFnDecl x ->
            List.iter self#visit x.params;
            let _ = get_type x.block in
            x.typ
        | ExprStmt _ -> get_type node
        | ReturnStmt x -> (
            if last_fndecl_type |> Option.is_none then
              raise
                (TypeError
                   ( "Return statement not enclosed within a function \
                      declaration.",
                     x.pos ));

            match x.stmt_opt with
            | Some stmt ->
                if
                  (* fn decl return type must match this return stmt type *)
                  Node.assert_type (get_type stmt)
                    (last_fndecl_type |> Option.get)
                then Node.Unit x.pos
                else
                  raise
                    (TypeMismatch
                       (get_type (x.fn_decl |> Option.get), get_type stmt, x.pos))
            | None -> (
                (* fn decl must be unit type *)
                match get_type (x.fn_decl |> Option.get) with
                | Node.Unit p -> Node.Unit p
                | act ->
                    raise
                      (TypeMismatch
                         (act, Node.Unit (Node.get_position node), x.pos))))
        | If x ->
            let cond_expr = get_type x.cond_expr in 
            let is_bool_cond_expr = (match cond_expr with
            | Bool _ -> true
            | _ -> false) in

            if is_bool_cond_expr then (
            if Option.is_some x.else_stmt_opt then (
                let _ = get_type x.if_stmt in 
                get_type (Option.get x.else_stmt_opt)
            ) else get_type x.if_stmt) else (
              raise (TypeMismatch (get_type x.if_stmt, Node.Bool x.pos, x.pos))
            )

            (*let is_bool_cond =
              match get_type x.cond_expr with Node.Bool _ -> true | _ -> false
            in
            if is_bool_cond then (
              let _ = get_type x.if_stmt in
              if x.else_stmt_opt |> Option.is_some then
                let _ = get_type (x.else_stmt_opt |> Option.get) in
                ()
              else ();
              Node.Unit x.pos)
            else (
                Node.print_ast_node (If x);
              raise (TypeMismatch (get_type x.if_stmt, Node.Bool x.pos, x.pos))
            )*)
        | While x ->
            let is_bool_cond =
              match get_type x.loop_exp with Node.Bool _ -> true | _ -> false
            in
            if is_bool_cond then
              let _ = get_type x.do_stmt in
              Node.Unit x.pos
            else
              raise (TypeMismatch (get_type x.loop_exp, Node.Bool x.pos, x.pos))
        | Block x ->
            List.iter
              (fun x ->
                let _ = get_type x in
                ())
              x.stmts;
            Node.Unit x.pos
        | Assign x ->
            let lhs_type = get_type x.lhs in
            let rhs_type = get_type x.rhs in

            (* make sure both sides are the same type *)
            if Node.assert_type lhs_type rhs_type then (
              (* store the type of the expression to the assign node *)
              x.typ <- Some lhs_type;
              lhs_type)
            else raise (TypeMismatch (rhs_type, lhs_type, x.pos))
        | IntLiteral x -> Node.Int x.pos
        | FloatLiteral x -> Node.Float x.pos
        | CharLiteral x -> Node.Char x.pos
        | StringLiteral x -> Node.String x.pos
        | BoolLiteral x -> Node.Bool x.pos
        | ArrayLiteral x ->
            if
              (* make sure each array element has the same type *)
              not (List.is_empty (List.nth x.arr 0))
            then (
              let flatarr = List.flatten x.arr in
              let elem_type = get_type (List.nth flatarr 0) in
              List.iter
                (fun elem ->
                  if
                    not
                      (Node.assert_type
                         (get_type (List.nth flatarr 0))
                         (get_type elem))
                  then raise (TypeMismatch (get_type elem, elem_type, x.pos)))
                flatarr;
              elem_type)
            else Node.Unit x.pos
        | VarExp x ->
            let elem_type = get_type (x.decl |> Option.get) in
            x.typ <- Some elem_type;
            elem_type
        | FnCallExp x ->
            (* checking number of arguments *)
            let fndecl_params =
              match x.decl |> Option.get with
              | Node.FnDecl fd ->
                  x.typ <- Some fd.typ;
                  fd.params
              | Node.AnonFnDecl afd ->
                  x.typ <- Some afd.typ;
                  afd.params
              | _ -> []
            in
            if List.length x.args <> List.length fndecl_params then
              raise (ParameterMismatch (x.name, x.pos))
            else
              let ensure_arg_types a b =
                if not (Node.assert_type (get_type a) (get_type b)) then
                  raise (TypeMismatch (get_type a, get_type b, x.pos))
              in
              List.iter2 ensure_arg_types x.args fndecl_params;

              x.typ |> Option.get
        | BinOp op -> (
            let lhstype = get_type op.lhs in
            let rhstype = get_type op.rhs in

            if not (Node.assert_type lhstype rhstype) then
              raise (TypeMismatch (rhstype, lhstype, op.pos))
            else
              match op.op with
              | EQ | NE | GT | GE | LT | LE | OR | AND ->
                  op.typ <- Some (Node.Bool op.pos);
                  Node.Bool op.pos
              | ADD | SUB| MUL | DIV | MOD ->
                  op.typ <- Some lhstype;
                  lhstype)
        | ArrayAccessExp x -> (
            match (get_type x.arr, get_type x.index) with
            | ArrayType _, Int _ ->
                let rec getarr_elemtype acc =
                  (* If the type of the array is also an array type then keep unfolding  *)
                  match acc with
                  | Node.ArrayType at -> getarr_elemtype at.typ
                  | _ -> acc
                in
                let elem_type = getarr_elemtype (get_type x.arr) in
                x.typ <- Some elem_type;
                elem_type
            | _, _ ->
                raise
                  (TypeError
                     ("Illegal array access attempt, <ArrayType>[<Int>]", x.pos))
            )
        | TypeCastExp tc -> (
            let expr_type = get_type tc.expr in
            match (tc.new_type, expr_type) with
            | _, _ when Node.assert_type tc.new_type expr_type ->
                (* cast to same type as is *)
                Printf.printf
                  "Unnecessary type cast to own type line %d column %d\n"
                  (fst tc.pos) (snd tc.pos);
                tc.typ <- Some expr_type;
                expr_type
            | Int i, Char _ ->
                tc.typ <- Some (Int i);
                Int i
            | Char c, Int _ ->
                tc.typ <- Some (Char c);
                Char c
            | String s, Char _ ->
                tc.typ <- Some (String s);
                String s
            | _, _ ->
                raise
                  (BadTypeCast
                     ( "Attempted type cast between invalid or uncastable types",
                       tc.pos )))
      in
      try
        let _ = get_type node in
        ()
      with
      | TypeMismatch (act, expected, pos) ->
          Printf.eprintf "type mismatch - found ";
          Node.print_ast_node act;
          Printf.eprintf " but expected ";
          Node.print_ast_node expected;
          Printf.eprintf "line %d column %d\n" (fst pos) (snd pos);
          exit type_check_error
      | UnexpectedUnitType (name, pos) ->
          Printf.eprintf
            "Unexpected unit type for variable %s on line %d column %d" name
            (fst pos) (snd pos);
          exit type_check_error
      | ParameterMismatch (name, pos) ->
          Printf.eprintf
            "Given parameters don't match \"%s\" function's declaration on \
             line %d column %d\n"
            name (fst pos) (snd pos);
          exit type_check_error
      | TypeError (msg, pos) ->
          Printf.eprintf "%s line %d column %d\n" msg (fst pos) (snd pos);
          exit type_check_error
      | BadTypeCast (msg, pos) ->
          Printf.eprintf "%s line %d column %d\n" msg (fst pos) (snd pos);
          exit type_check_error
      | e ->
          Printf.eprintf "Error occured in type checker\n";
          Printf.eprintf "%s\n" (Printexc.to_string e);
          exit type_check_error
  end
