type op = ADD | SUB | MUL | DIV | MOD | GT | LT | GE | LE | NE | EQ | OR | AND

type position = int * int
(** [position] is the (line, column) tuple indicating the position of the 
    AST node in the source code *)

let print_position = function 
    | (line, column) -> Printf.printf "(%d, %d)" line column

type t =
  | Program of { imports : t list; declarations : t list }
  | Import of string * position
  (* types *)
  | Unit of position
  | Int of position
  | Float of position
  | Char of position
  | String of position
  | Bool of position
  | Fn of { param_types : t list; return_type : t; pos : position }
  | ArrayType of { n_elems : int; typ : t; pos : position }
  (* DECLARATIONS *)
  | VarDecl of {
      name : string;
      typ : t;
      value : t option; (* expected an expression *)
      pos : position;
    }
  | FnDecl of {
      name : string;
      params : t list; (* expects vardecl variant here *)
      mutable block : t option;
      typ : t;
      pos : position;
    }
  | AnonFnDecl of {
      params : t list; (* expects vardecl variant here *)
      block : t;
      typ : t;
      pos : position;
    }
  (* STATEMENTS *)
  | ExprStmt of { expr : t; pos : position }
  | ReturnStmt of { stmt_opt : t option; mutable fn_decl: t option; pos : position }
    (* expects expression *)
  | If of {
      cond_expr : t;
      if_stmt : t;
      else_stmt_opt : t option;
      pos : position;
    }
  | While of { loop_exp : t; do_stmt : t; pos : position }
  | Block of { stmts : t list; return_stmts : t list option; pos : position ; typ: t}
  (* Expressions *)
  | Assign of { mutable typ : t option; lhs : t; rhs : t; pos : position }
  | IntLiteral of { i : int; pos : position }
  | FloatLiteral of { f : float; pos : position }
  | CharLiteral of { c : char; pos : position }
  | StringLiteral of { s : string; pos : position }
  | BoolLiteral of { b : bool; pos : position }
  | ArrayLiteral of { arr : t list list; pos : position }
  | VarExp of { name : string; mutable typ: t option; mutable decl : t option; pos : position }
    (* decl to be filled in during name analysis*)
  | FnCallExp of {
      mutable typ : t option;
      name : string;
      args : t list;
      mutable decl : t option;
      pos : position;
    }
  | BinOp of {mutable typ : t option; op : op; lhs : t; rhs : t; pos : position }
  | ArrayAccessExp of {mutable typ : t option; arr : t; index : t; pos : position }
  | TypeCastExp of {mutable typ : t option; new_type : t; expr : t; pos : position }


  let rec assert_type (x : t) (y : t) : bool =
    match (x, y) with
    | Unit _, Unit _ -> true  (* Unit has no value to compare, so it's always "equal" *)
    
    | Int _, Int _ -> true  (* Compare the integer values *)
    
    | Float _, Float _ -> true  (* Compare the float values *)
    
    | Char _, Char _ -> true  (* Compare the char values *)
    
    | String _, String _ -> true  (* Compare the string values *)
    
    | Bool _, Bool _ -> true  (* Compare the boolean values *)
  
    | Fn { param_types = p1; return_type = r1; _ }, Fn { param_types = p2; return_type = r2; _ } ->
      (* Compare the parameter types and return types recursively *)
      assert_type r1 r2 && List.length p1 = List.length p2 && 
      List.for_all2 assert_type p1 p2
    
    | ArrayType { n_elems = n1; typ = t1; _ }, ArrayType { n_elems = n2; typ = t2; _ } ->
      (* Compare number of elements and the type of elements *)
      n1 = n2 && assert_type t1 t2
    
    | _ , _ -> false  (* If the variants are different types, they are not equal *)


(* let get_stmt_type stmt = 
  match stmt with 
  | Block x -> x.typ
  | While x ->  *)

let get_position (x : t) : position =
  match x with
  | Program { imports = _; declarations = _ } ->
      (0, 0) (* Program doesn't carry position itself *)
  | Import (_, pos) -> pos
  | Unit pos -> pos
  | Int pos -> pos
  | Float pos -> pos
  | Char pos -> pos
  | String pos -> pos
  | Bool pos -> pos
  | Fn { pos; _ } -> pos
  | ArrayType { pos; _ } -> pos
  | VarDecl { pos; _ } -> pos
  | FnDecl { pos; _ } -> pos
  | AnonFnDecl { pos; _ } -> pos
  | ExprStmt { pos; _ } -> pos
  | ReturnStmt { pos; _ } -> pos
  | If { pos; _ } -> pos
  | While { pos; _ } -> pos
  | Block { pos; _ } -> pos
  | Assign { pos; _ } -> pos
  | IntLiteral { pos; _ } -> pos
  | FloatLiteral { pos; _ } -> pos
  | CharLiteral { pos; _ } -> pos
  | StringLiteral { pos; _ } -> pos
  | BoolLiteral { pos; _ } -> pos
  | ArrayLiteral { pos; _ } -> pos
  | VarExp { pos; _ } -> pos
  | FnCallExp { pos; _ } -> pos
  | BinOp { pos; _ } -> pos
  | ArrayAccessExp { pos; _ } -> pos
  | TypeCastExp { pos; _ } -> pos

let print_op (op : op) : unit =
  match op with
  | ADD -> Printf.printf "ADD"
  | SUB -> Printf.printf "SUB"
  | MUL -> Printf.printf "MUL"
  | DIV -> Printf.printf "DIV"
  | MOD -> Printf.printf "MOD"
  | GT -> Printf.printf "GT"
  | LT -> Printf.printf "LT"
  | GE -> Printf.printf "GE"
  | LE -> Printf.printf "LE"
  | NE -> Printf.printf "NE"
  | EQ -> Printf.printf "EQ"
  | OR -> Printf.printf "OR"
  | AND -> Printf.printf "AND"

let rec print_ast_node (t : t) : unit =
  match t with
  | Program { imports; declarations } ->
      Printf.printf "Program(";
      List.iter (fun imp -> print_ast_node imp) imports;
      List.iter
        (fun decl ->
          print_ast_node decl;
          Printf.printf ", ")
        declarations;
      Printf.printf ")\n"
  | Import (s, _) -> Printf.printf "Import(%s), " s
  | Unit _ -> Printf.printf "Unit"
  | Int _ -> Printf.printf "Int"
  | Float _ -> Printf.printf "Float"
  | Char _ -> Printf.printf "Char"
  | String _ -> Printf.printf "String"
  | Bool _ -> Printf.printf "Bool"
  | Fn { param_types; return_type; _ } ->
      Printf.printf "Fn(";
      print_ast_node return_type;
      Printf.printf ", ";
      List.iter
        (fun param_type ->
          print_ast_node param_type;
          Printf.printf ", ")
        param_types;
      Printf.printf ")"
  | ArrayType { n_elems; typ; _ } ->
      Printf.printf "ArrayType(";
      print_ast_node typ;
      Printf.printf ", %d)" n_elems
  | VarDecl { name; typ; value; _ } ->
      Printf.printf "VarDecl(%s, " name;
      print_ast_node typ;
      (match value with
      | Some v ->
          Printf.printf ", ";
          print_ast_node v
      | None -> ());
      Printf.printf ")"
  | FnDecl { name; params; block; typ; _ } ->
      Printf.printf "FnDecl(%s, " name;
      print_ast_node typ;
      Printf.printf ", ";
      List.iter
        (fun x ->
          print_ast_node x;
          Printf.printf ", ")
        params;
      if Option.is_some block then print_ast_node (Option.get block) else ();
      Printf.printf ")"
  | AnonFnDecl { params; block; typ; _ } ->
      Printf.printf "AnonFnDecl(";
      print_ast_node typ;
      Printf.printf ", ";
      List.iter
        (fun x ->
          print_ast_node x;
          Printf.printf ", ")
        params;
      print_ast_node block;
      Printf.printf ")"
  | ExprStmt { expr; _ } ->
      Printf.printf "ExprStmt(";
      print_ast_node expr;
      Printf.printf ")"
  | ReturnStmt { stmt_opt; _ } ->
      Printf.printf "ReturnStmt(";
      (match stmt_opt with Some stmt -> print_ast_node stmt | None -> ());
      Printf.printf ")"
  | If { cond_expr; if_stmt; else_stmt_opt; _ } ->
      Printf.printf "If(";
      print_ast_node cond_expr;
      Printf.printf ", ";
      print_ast_node if_stmt;
      (match else_stmt_opt with
      | Some stmt ->
          Printf.printf ", ";
          print_ast_node stmt
      | None -> ());
      Printf.printf ")"
  | While { loop_exp; do_stmt; _ } ->
      Printf.printf "While(";
      print_ast_node loop_exp;
      Printf.printf ", ";
      print_ast_node do_stmt;
      Printf.printf ")"
  | Block { stmts; _ } ->
      Printf.printf "Block(";
      List.iter
        (fun x ->
          print_ast_node x;
          Printf.printf ", ")
        stmts;
      Printf.printf ")"
  | Assign { typ; lhs; rhs; _ } ->
      Printf.printf "Assign(";
      (match typ with
      | Some t ->
          print_ast_node t;
          Printf.printf ", "
      | None -> ());
      print_ast_node lhs;
      Printf.printf ", ";
      print_ast_node rhs;
      Printf.printf ")"
  | IntLiteral { i; _ } -> Printf.printf "IntLiteral(%d)" i
  | FloatLiteral { f; _ } -> Printf.printf "FloatLiteral(%f)" f
  | CharLiteral { c; _ } -> Printf.printf "CharLiteral(%c)" c
  | StringLiteral { s; _ } -> Printf.printf "StringLiteral(%s)" s
  | BoolLiteral { b; _ } -> Printf.printf "BoolLiteral(%b)" b
  | ArrayLiteral { arr; _ } ->
      Printf.printf "ArrayLiteral([";
      List.iter
        (fun row ->
          (List.iter (fun node ->
               print_ast_node node;
               Printf.printf ", "))
            row;
          Printf.printf " ; ")
        arr;
      Printf.printf "]) "
  | VarExp { name; _ } -> Printf.printf "VarExp(%s), " name
  | FnCallExp { typ; name; args; _ } ->
      Printf.printf "FnCallExp(%s," name;
      (match typ with
      | Some t ->
          print_ast_node t;
          Printf.printf ", "
      | None -> ());
      List.iter
        (fun x ->
          print_ast_node x;
          Printf.printf ", ")
        args;
      Printf.printf ")"
  | BinOp { typ; op; lhs; rhs; _ } ->
      Printf.printf "BinOp(";
      print_op op;
      Printf.printf ", ";
      (match typ with
      | Some t ->
          print_ast_node t;
          Printf.printf ", "
      | None -> ());
      print_ast_node lhs;
      Printf.printf ", ";
      print_ast_node rhs;
      Printf.printf ")"
  | ArrayAccessExp { typ; arr; index; _ } ->
      Printf.printf "ArrayAccessExp(";
      (match typ with
      | Some t ->
          print_ast_node t;
          Printf.printf ", "
      | None -> ());
      print_ast_node arr;
      Printf.printf ", ";
      print_ast_node index;
      Printf.printf ")"
  | TypeCastExp { typ = _; new_type; expr; _ } ->
      Printf.printf "TypeCastExp(";
      print_ast_node new_type;
      Printf.printf ", ";
      print_ast_node expr;
      Printf.printf ")"

class virtual ast_pass =
  object
    method virtual visit : t -> unit
  end
