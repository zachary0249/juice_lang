

let lvalue_error_code = 6

exception LValueError of Node.t * Node.position

(** Ensures that values on the left hand size of expressions have verifiable memory addresses *)
class lvalue_analyzer = 
    object
        inherit Node.ast_pass

        method visit (node: Node.t) = 
            let rec aux node = 
                match node with
                | Node.Program x -> List.iter aux x.declarations
                | Node.ExprStmt x -> aux x.expr
                | Node.ReturnStmt x -> if Option.is_some x.stmt_opt then aux (Option.get x.stmt_opt)
                | Node.If x -> (
                    aux x.cond_expr;
                    aux x.if_stmt;
                    if Option.is_some x.else_stmt_opt then aux (Option.get x.else_stmt_opt)
                )
                | Node.While x -> (
                    aux x.loop_exp;
                    aux x.do_stmt;
                )
                | Node.Block x -> (
                    List.iter aux x.stmts;
                )
                | Assign {lhs; pos; _} -> (
                    (* lhs of assign can only be valid items that can be assigned to *)
                    match lhs with 
                    | Node.VarExp _ | Node.ArrayAccessExp _ -> ()
                    | _ -> raise (LValueError (lhs, pos))
                ) 
                    
                | _ -> ()
            in 
            try aux node with 
            LValueError (errnode, pos) -> (
                Printf.eprintf "invalid lvalue ";
                Node.print_ast_node errnode;
                Printf.eprintf " line %d column %d" (fst pos) (snd pos);
                exit lvalue_error_code 
            )
    end
