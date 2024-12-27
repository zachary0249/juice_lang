type token_class =
  | Identifier of string
  | Assign
  (* delimeters*)
  | Lbra (* '{' // left brace *)
  | Rbra (* '}' // right brace *)
  | Lpar (* '(' // left parenthesis *)
  | Rpar (* ')' // right parenthesis *)
  | Lsbr (* '[' // left square brace *)
  | Rsbr (* ']' // right square brace *)
  | Comma (* ',' *)
  | Colon (* ':' *)
  | SemiColon (* ';' *)
  (* types *)
  | Int (* "Int" *)
  | Float (* "Decimal" *)
  | Char (* "Char" *)
  | Unit (* "Unit" *)
  | String (* "String" *)
  | Bool (* "Bool" *)
  | Fn (* "Fn" *)
  | ReturnType (* "->" *)
  (* keywords *)
  | If (* "if" *)
  | Else (* "else" *)
  | While (* "while" *)
  | Return (* "return" *)
  | Let (* "let" *)
  | Import (* "import" *)
  (* literals*)
  | StringLiteral of string
    (* ".*"  any sequence of characters enclosed within two double quotes *)
  | IntLiteral of int (* ('0'|...|'9')+ *)
  | FloatLiteral of float (* f64 *)
  | CharLiteral of
      char (* all normal chars and special characters like '\n', '\t', etc. *)
  | BoolLiteral of bool (* "true" | "false" *)
  (* logical operators*)
  | LogAnd (* "and" *)
  | LogOr (* "or" *)
  (*comparison*)
  | Eq (* "==" *)
  | Ne (* "!=" *)
  | Lt (* '<' *)
  | Gt (* '>' *)
  | Le (* "<=" *)
  | Ge (* ">=" *)
  (* operators*)
  | Plus (* '+' *)
  | Minus (* '-' *)
  | Asterix (* '*'  // multiplication *)
  | Div (* '/' *)
  | Rem (* '%' *)
  | FnDecl (* function declaration "fn" *)
  | EOF (* end of file token *)
  | Invalid

let print_token_class = function
  | Identifier id -> Printf.printf "Identifier(%s)\n" id
  | Assign -> Printf.printf "Assign\n"
  (* delimiters *)
  | Lbra -> Printf.printf "Lbra\n"
  | Rbra -> Printf.printf "Rbra\n"
  | Lpar -> Printf.printf "Lpar\n"
  | Rpar -> Printf.printf "Rpar\n"
  | Lsbr -> Printf.printf "Lsbr\n"
  | Rsbr -> Printf.printf "Rsbr\n"
  | Comma -> Printf.printf "Comma\n"
  | Colon -> Printf.printf "Colon\n"
  | SemiColon -> Printf.printf "SemiColon\n"
  (* types *)
  | Int -> Printf.printf "Int\n"
  | Float -> Printf.printf "Float\n"
  | Char -> Printf.printf "Char\n"
  | Unit -> Printf.printf "Unit\n"
  | String -> Printf.printf "String\n"
  | Bool -> Printf.printf "Bool\n"
  | Fn -> Printf.printf "Fn\n"
  | ReturnType -> Printf.printf "ReturnType\n"
  (* keywords *)
  | If -> Printf.printf "If\n"
  | Else -> Printf.printf "Else\n"
  | While -> Printf.printf "While\n"
  | Return -> Printf.printf "Return\n"
  | Let -> Printf.printf "Let\n"
  | Import -> Printf.printf "Import\n"
  (* literals *)
  | StringLiteral strlit -> Printf.printf "StringLiteral(%s)\n" strlit
  | IntLiteral intlit -> Printf.printf "IntLiteral(%d)\n" intlit
  | FloatLiteral _ -> Printf.printf "FloatLiteral\n"
  | CharLiteral _ -> Printf.printf "CharLiteral\n"
  | BoolLiteral _ -> Printf.printf "BoolLiteral\n"
  (* logical operators *)
  | LogAnd -> Printf.printf "LogAnd\n"
  | LogOr -> Printf.printf "LogOr\n"
  (* comparisons *)
  | Eq -> Printf.printf "Eq\n"
  | Ne -> Printf.printf "Ne\n"
  | Lt -> Printf.printf "Lt\n"
  | Gt -> Printf.printf "Gt\n"
  | Le -> Printf.printf "Le\n"
  | Ge -> Printf.printf "Ge\n"
  (* operators *)
  | Plus -> Printf.printf "Plus\n"
  | Minus -> Printf.printf "Minus\n"
  | Asterix -> Printf.printf "Asterix\n"
  | Div -> Printf.printf "Div\n"
  | Rem -> Printf.printf "Rem\n"
  | FnDecl -> Printf.printf "FnDecl\n"
  | EOF -> Printf.printf "EOF\n"
  | Invalid -> Printf.printf "Invalid\n"

type token = { token_type : token_class; line : int; col : int }

let same_tokentype t1 t2 =
  (* Compare token_type fields *)
  match (t1, t2) with
  | Identifier id1, Identifier id2 -> id1 = id2
  | StringLiteral s1, StringLiteral s2 -> s1 = s2
  | IntLiteral i1, IntLiteral i2 -> i1 = i2
  | FloatLiteral f1, FloatLiteral f2 -> f1 = f2
  | CharLiteral c1, CharLiteral c2 -> c1 = c2
  | BoolLiteral b1, BoolLiteral b2 -> b1 = b2
  | t1, t2 -> t1 = t2 (* All other token types are directly comparable *)

let same_token t1 t2 =
  (* Compare token_type fields *)
  (match (t1.token_type, t2.token_type) with
  | Identifier id1, Identifier id2 -> id1 = id2
  | StringLiteral s1, StringLiteral s2 -> s1 = s2
  | IntLiteral i1, IntLiteral i2 -> i1 = i2
  | FloatLiteral f1, FloatLiteral f2 -> f1 = f2
  | CharLiteral c1, CharLiteral c2 -> c1 = c2
  | BoolLiteral b1, BoolLiteral b2 -> b1 = b2
  | t1, t2 -> t1 = t2 (* All other token types are directly comparable *))
  (* Compare line and col fields *)
  && t1.line = t2.line
  && t1.col = t2.col

let is_literal = function
  | Identifier _ | StringLiteral _ | IntLiteral _ | FloatLiteral _
  | CharLiteral _ | BoolLiteral _ ->
      true
  | _ -> false

let is_type = function
  | Unit | Int | Float | Char | String | Bool -> true
  | _ -> false

let is_identifier = function Identifier _ -> true | _ -> false

let print_token tok =
  Printf.printf "Token(%d, %d) : " tok.line tok.col;
  print_token_class tok.token_type

let new_token tt line col = { token_type = tt; line; col }

type tokenizer_error = TokenizationError of string * int * int

let tokenization_error = 2

let print_tokenization_error = function
  | TokenizationError (msg, line, col) ->
      Printf.eprintf "%s - line %d column %d\n" msg line col

exception InvalidToken of tokenizer_error

let is_eof = function EOF -> true | _ -> false

(** [next_token] @raise InvalidToken if invalid token encountered *)
class tokenizer (scanner : Scanner.scanner) =
  object (self)
    method next_token =
      let is_whitespace = function
        | ' ' | '\t' | '\n' | '\r' | '\x0C' -> true
        | _ -> false
      in

      let parse_escape c =
        if c <> '\\' then c
        else
          match scanner#next with
          | Some '"' -> '\"'
          | Some '\'' -> '\''
          | Some 'n' -> '\n'
          | Some 't' -> '\t'
          | Some 'r' -> '\r'
          | Some 'b' -> '\b'
          | Some '\\' -> '\\'
          | None ->
              raise
                (InvalidToken
                   (TokenizationError
                      ("unclosed escape char", scanner#get_line, scanner#get_col)))
          | _ ->
              raise
                (InvalidToken
                   (TokenizationError
                      ("invalid escape char", scanner#get_line, scanner#get_col)))
      in

      let is_alphabetic = function
        | 'a' .. 'z' | 'A' .. 'Z' -> true
        | _ -> false
      in

      let is_alphanumeric = function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
        | _ -> false
      in

      let is_numeric = function '0' .. '9' -> true | _ -> false in

      let rec next () =
        match scanner#next with
        | None -> None
        | Some x when is_whitespace x -> next ()
        | Some '/' when scanner#has_next && scanner#peek |> Option.get = '/' ->
            let rec loop () =
              if scanner#has_next && scanner#peek |> Option.get <> '\n' then
                let _ = scanner#next in
                loop ()
            in
            loop ();

            if scanner#has_next && scanner#next |> Option.get = '\n' then
              next ()
            else None
        | Some '+' ->
            Some
              {
                token_type = Plus;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some '-' when scanner#has_next && scanner#peek |> Option.get <> '>' ->
            Some
              {
                token_type = Minus;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some '-' when scanner#has_next && scanner#peek |> Option.get = '>' ->
            let _ = scanner#next in

            Some
              {
                token_type = ReturnType;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some '{' ->
            Some
              {
                token_type = Lbra;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some '}' ->
            Some
              {
                token_type = Rbra;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some '[' ->
            Some
              {
                token_type = Lsbr;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some ']' ->
            Some
              {
                token_type = Rsbr;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some '(' ->
            Some
              {
                token_type = Lpar;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some ')' ->
            Some
              {
                token_type = Rpar;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some ',' ->
            Some
              {
                token_type = Comma;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some ':' ->
            Some
              {
                token_type = Colon;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some ';' ->
            Some
              {
                token_type = SemiColon;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some '=' when scanner#has_next && scanner#peek |> Option.get = '=' ->
            let _ = scanner#next in
            Some
              {
                token_type = Eq;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some '=' ->
            Some
              {
                token_type = Assign;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some '!' when scanner#has_next && scanner#peek |> Option.get = '=' ->
            Some
              {
                token_type = Ne;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some '<' when scanner#has_next && scanner#peek |> Option.get = '=' ->
            let _ = scanner#next in
            Some
              {
                token_type = Le;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some '<' ->
            Some
              {
                token_type = Lt;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some '>' when scanner#has_next && scanner#peek |> Option.get = '=' ->
            let _ = scanner#next in
            Some
              {
                token_type = Ge;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some '>' ->
            Some
              {
                token_type = Gt;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some '*' ->
            Some
              {
                token_type = Asterix;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some '/' ->
            Some
              {
                token_type = Div;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some '%' ->
            Some
              {
                token_type = Rem;
                line = scanner#get_line;
                col = scanner#get_col;
              }
        | Some '\'' -> (
            (* char literal *)
            match scanner#next with
            | Some x -> (
                let escaped = parse_escape x in

                match scanner#next with
                | Some '\'' ->
                    Some
                      {
                        token_type = CharLiteral escaped;
                        line = scanner#get_line;
                        col = scanner#get_col;
                      }
                | _ ->
                    raise
                      (InvalidToken
                         (TokenizationError
                            ( "unclosed character literal",
                              scanner#get_line,
                              scanner#get_col ))))
            | None ->
                raise
                  (InvalidToken
                     (TokenizationError
                        ( "unclosed character literal",
                          scanner#get_line,
                          scanner#get_col ))))
        | Some '"' ->
            (* string literal *)
            let built_str = Buffer.create 16 in
            let rec loop () =
              match scanner#next with
              | Some '"' ->
                  if Buffer.length built_str = 0 then
                    raise
                      (InvalidToken
                         (TokenizationError
                            ( "empty string literal",
                              scanner#get_line,
                              scanner#get_col )))
                  else
                    Some
                      {
                        token_type = StringLiteral (Buffer.contents built_str);
                        line = scanner#get_line;
                        col = scanner#get_col;
                      }
              | Some '\\' ->
                  if scanner#has_next then (
                    (* escape the character if applicable *)
                    let nxt = scanner#next |> Option.get in
                    Buffer.add_char built_str (parse_escape nxt);
                    loop ())
                  else loop ()
              | Some x ->
                  Buffer.add_char built_str x;
                  loop ()
              | None ->
                  raise
                    (InvalidToken
                       (TokenizationError
                          ( "unclosed string literal",
                            scanner#get_line,
                            scanner#get_col )))
            in
            loop ()
        | Some x when is_alphabetic x || x = '_' -> (
            (* identifier *)
            let identifier = Buffer.create 16 in
            Buffer.add_char identifier x;
            let rec loop () =
              let nxt = scanner#peek in
              if nxt |> Option.is_some && is_alphanumeric (nxt |> Option.get)
              then (
                Buffer.add_char identifier (scanner#next |> Option.get);
                loop ())
            in
            loop ();

            (* match for keywords with string *)
            match Buffer.contents identifier with
            | "Unit" ->
                Some
                  {
                    token_type = Unit;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | "Int" ->
                Some
                  {
                    token_type = Int;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | "Float" ->
                Some
                  {
                    token_type = Float;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | "Char" ->
                Some
                  {
                    token_type = Char;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | "String" ->
                Some
                  {
                    token_type = String;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | "Bool" ->
                Some
                  {
                    token_type = Bool;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | "Fn" ->
                Some
                  {
                    token_type = Fn;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | "if" ->
                Some
                  {
                    token_type = If;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | "else" ->
                Some
                  {
                    token_type = Else;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | "while" ->
                Some
                  {
                    token_type = While;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | "return" ->
                Some
                  {
                    token_type = Return;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | "and" ->
                Some
                  {
                    token_type = LogAnd;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | "or" ->
                Some
                  {
                    token_type = LogOr;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | "true" ->
                Some
                  {
                    token_type = BoolLiteral true;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | "false" ->
                Some
                  {
                    token_type = BoolLiteral false;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | "import" ->
                Some
                  {
                    token_type = Import;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | "let" ->
                Some
                  {
                    token_type = Let;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | "fn" ->
                Some
                  {
                    token_type = FnDecl;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  }
            | id ->
                Some
                  {
                    token_type = Identifier id;
                    line = scanner#get_line;
                    col = scanner#get_col;
                  })
        | Some x when is_numeric x -> (
            let number = Buffer.create 16 in
            Buffer.add_char number x;

            let rec loop () =
              if scanner#peek |> Option.is_some then
                match scanner#peek |> Option.get with
                | x when is_numeric x || x = '.' ->
                    Buffer.add_char number (scanner#next |> Option.get);
                    loop ()
                | _ -> ()
            in
            loop ();

            let acc = Buffer.contents number in
            if String.contains acc '.' then
              let flt = float_of_string_opt acc in
              match flt with
              | Some f ->
                  Some
                    {
                      token_type = FloatLiteral f;
                      line = scanner#get_line;
                      col = scanner#get_col;
                    }
              | None ->
                  raise
                    (InvalidToken
                       (TokenizationError
                          ( "invalid floating point number given",
                            scanner#get_line,
                            scanner#get_col )))
            else
              let num = int_of_string_opt acc in
              match num with
              | Some i ->
                  Some
                    {
                      token_type = IntLiteral i;
                      line = scanner#get_line;
                      col = scanner#get_col;
                    }
              | None ->
                  raise
                    (InvalidToken
                       (TokenizationError
                          ( "invalid integer number given",
                            scanner#get_line,
                            scanner#get_col ))))
        | Some _ ->
            raise
              (InvalidToken
                 (TokenizationError
                    ("invalid character", scanner#get_line, scanner#get_col)))
      in

      (* match the result from next and see if is valid token, none - EOF, or invalid *)
      try
        match next () with
        | Some t -> t
        | None ->
            { token_type = EOF; line = scanner#get_line; col = scanner#get_col }
      with InvalidToken e ->
        print_tokenization_error e;
        exit
          tokenization_error (* exit code 2 corresponds to tokenization error *)

    method collect =
      let rec aux acc =
        match self#next_token with
        | t when is_eof t.token_type -> List.rev acc
        | t -> aux (t :: acc)
      in
      aux []
  end
