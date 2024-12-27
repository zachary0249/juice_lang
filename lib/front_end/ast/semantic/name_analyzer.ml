let name_analysis_error = 4

module Symbol = struct
  type t = { name : string; decl : Node.t }
end

module Scope = struct
  type t = { outer : t option; symbol_table : (string, Symbol.t) Hashtbl.t }

  let lookup_current t name = Hashtbl.find_opt t.symbol_table name

  let rec lookup t name =
    match lookup_current t name with
    | Some symbol -> Some symbol
    | None ->
        if t.outer |> Option.is_some then lookup (Option.get t.outer) name
        else None

  let put t (symbol : Symbol.t) = Hashtbl.add t.symbol_table symbol.name symbol
  let init ?(outer = None) () = { outer; symbol_table = Hashtbl.create 50 }
end

exception AlreadyInScope of string * Node.position
exception NotInScope of string * Node.position

(** [name_analyzer] inherits AST node pass, performs a traversal overriding the visit
    method that ensures scoping and visibility rules are respected *)
class name_analyzer =
  object
    inherit Node.ast_pass
    val mutable scope = Scope.init ()

    method visit node : unit =
      let rec aux node =
        match node with
        | Node.Program { imports; declarations } ->
            List.iter aux imports;
            List.iter aux declarations
        | Unit _ | Int _ | Float _ | Char _ | String _ | Bool _ | Fn _
        | ArrayType _ ->
            ()
        | VarDecl { name; typ; value; pos } ->
            let s = Scope.lookup_current scope name in
            if Option.is_some s then raise (AlreadyInScope (name, pos))
            else (
              (match value with
              | Some (Node.AnonFnDecl afd) ->
                  Scope.put scope { name; decl = Node.AnonFnDecl afd };
                  (* analyze this anon function and include its reference in scope *)
                  let old_scope = scope in
                  scope <- Scope.init () ~outer:(Some old_scope);
                  List.iter aux afd.params;
                  aux afd.block;
                  scope <- old_scope
              | _ -> Scope.put scope { name; decl = node });
              aux typ)
        | FnDecl { name; params; block; typ = _; pos } ->
            let _ =
              match Scope.lookup_current scope name with
              | Some _ -> raise (AlreadyInScope (name, pos))
              | None -> Scope.put scope { name; decl = node }
            in

            (* change the scope to this function scope *)
            let old_scope = scope in
            scope <- Scope.init () ~outer:(Some old_scope);
            List.iter aux params;
            if Option.is_some block then aux (Option.get block) else ();
            scope <- old_scope
        | ExprStmt { expr; _ } -> aux expr
        | ReturnStmt { stmt_opt; _ } ->
            if Option.is_some stmt_opt then aux (Option.get stmt_opt)
        | If { cond_expr; if_stmt; else_stmt_opt; _ } ->
            aux cond_expr;
            aux if_stmt;
            if Option.is_some else_stmt_opt then aux (Option.get else_stmt_opt)
        | While { loop_exp; do_stmt; _ } ->
            aux loop_exp;
            aux do_stmt
        | Block { stmts; _ } -> List.iter aux stmts
        | Assign { typ = _; lhs; rhs; _ } ->
            aux lhs;
            aux rhs
        | IntLiteral _ | FloatLiteral _ | CharLiteral _ | StringLiteral _
        | BoolLiteral _ ->
            ()
        | VarExp x -> (
            match Scope.lookup scope x.name with
            | Some varsymb -> x.decl <- Some varsymb.decl
            | None -> raise (NotInScope (x.name, x.pos)))
        | FnCallExp x -> (
            match Scope.lookup scope x.name with
            | Some fnsymb ->
                x.decl <- Some fnsymb.decl;
                List.iter aux x.args
            | None -> raise (NotInScope (x.name, x.pos)))
        | BinOp { typ = _; op = _; lhs; rhs; _ } ->
            aux lhs;
            aux rhs
        | ArrayAccessExp { typ = _; arr; index; _ } ->
            aux arr;
            aux index
        | TypeCastExp { typ = _; new_type; expr; _ } ->
            aux new_type;
            aux expr
        | _ -> ()
      in
      try aux node with
      | AlreadyInScope (name, pos) ->
          Printf.eprintf "symbol \"%s\" already in scope - line %d column %d\n"
            name (fst pos) (snd pos);
          exit name_analysis_error
      | NotInScope (name, pos) ->
          Printf.eprintf
            "symbol \"%s\" not found in scope - line %d column %d\n" name
            (fst pos) (snd pos);
          exit name_analysis_error
  end
