open Front_end.Ast
open Reg_alloc

type label = string

(* each section contains their code *)
type section =
  | Text of asm_stmt list
  | Data of asm_stmt list
  | Bss of asm_stmt list

(* these are the supported ASM instructions *)
and asm_stmt =
  | Db_String of label * string
  | Dq_Int of label * int
  | Resq of label * int (* reserves size for 64bit array *)
  | Resb of label * int
  | FnLabel of string
  | Section of section
  | Global of label
  | Mov of Register.t * label
  | Call of label (* push %rip, jmp to label fn *)
  | Push of Register.t (* add to top of stack *)
  | Inc of Register.t
  | Jmp of label
  | Je of label
  | Syscall
  | Pop of Register.t
  | Ret

let string_of_asm_stmt = function
  | Db_String (label, str) -> Printf.sprintf "%s db \"%s\", 0\n" label str
  | Dq_Int (lbl, i) -> Printf.sprintf "%s dq %d\n" lbl i
  | Resq (lbl, size) -> Printf.sprintf "%s resq %d\n" lbl size
  | _ -> failwith "print not yet implemented"

let string_of_section sect =
  let buff = Buffer.create 160 in
  match sect with
  | Text _stmts -> Buffer.contents buff
  | Data stmts ->
      Buffer.add_string buff "section .data\n";
      List.iter (fun x -> Buffer.add_string buff (string_of_asm_stmt x)) stmts;
      Buffer.contents buff
  | Bss stmts -> 
    Buffer.add_string buff "section .bss\n";
    List.iter (fun x -> Buffer.add_string buff (string_of_asm_stmt x)) stmts;
    Buffer.contents buff
    

let label_counter = ref 0

let create_label name =
  let counter = !label_counter in
  incr label_counter;
  let label_name = Printf.sprintf "label_%d_%s" counter name in
  (* note that any previous labels in table get burried until most recent is removed *)
  label_name

let rec size_of = function
  | Node.ArrayType { n_elems; typ; _ } -> n_elems * size_of typ
  | Node.Unit _ -> 0
  | _ -> 0

let alloc_global_mem ast =
  (* maps an ast node to its asm mem alloc *)
  let decls =
    match ast with
    | Node.Program { imports = _; declarations } -> declarations
    | _ -> failwith "expects program ast"
  in
  let global_decls =
    List.filter
      (fun x -> match x with Node.VarDecl _ -> true | _ -> false)
      decls
  in
  (* function that produces asm output while iterating through the global decls *)
  let alloc decl =
    match decl with 
    | Node.VarDecl { name; typ; value; _ } -> (
        if (* check if we have an assignment *)
           value |> Option.is_some then
          match typ with
          | Node.Int _ -> (
              (* we expect an int literal as val *)
              match value |> Option.get with
              | Node.IntLiteral { i; _ } ->
                  Data [ Dq_Int (create_label name, i) ]
              | _ -> failwith "int type vardecl expects int literal assignment")
          | Node.String _ -> (
              match value |> Option.get with
              | Node.StringLiteral sl ->
                  Data [ Db_String (create_label name, sl.s) ]
              | _ ->
                  failwith
                    "string type vardecl expects string literal assignment")
          | _ -> failwith "this code gen not implemented yet"
        else
          
          (* when the decl has not been assigned : bss section *)
          match typ with
          | Node.Int _ -> Bss [ Resq (create_label name, 1) ]
          | _ -> failwith "this code gen not implemented yet")
    | _ -> failwith "unexpected AST type in global memory allocation"
  in
  List.map alloc global_decls


(* let visit_fn_decl ast = 
  match ast with
  | Node.FnDecl {name; params; block; typ; _} -> 
    Global name :: 
    FnLabel name :: 

    ()
  | _ -> ()


let gen_program ast = 
  let global_sections = alloc_global_mem ast in 

  (* standard library functions *)
  let print_string =  *)