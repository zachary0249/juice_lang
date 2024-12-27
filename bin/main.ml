open Juice

let usage_msg =
  "juice -c <command> -f <sourcefile>\n\
  \  Error Codes:\n\
  \      * 0 - success\n\
  \      * 1 - error in command line arguments or I/O\n\
  \      * 2 - tokenization error\n\
  \      * 3 - parser error\n\
  \      * 4 - name analysis error\n\
  \      * 5 - type check error\n\
  \      * 6 - l-value analysis error\n\
  \      * 7 - n      "

let command_usage =
  "<command>:\n\
  \      * lexer - prints the tokens of the <sourcefile> to standard output\n\
  \      * parser - produces the AST (Abstract Syntax Tree) of the program\n\
  \        * codegen - produces the assembly instructions for the given source \
   file"

let command = ref ""
let source_file = ref ""
let anon_fun invalid = raise (Arg.Bad ("Invalid input: " ^ invalid))

let speclist =
  [
    ("-c", Arg.Set_string command, command_usage);
    ("-f", Arg.Set_string source_file, "Set source file path");
  ]

(** 
Error codes:
0 = success
1 = command line input failure
2 = tokenization error
3 = parser error
    *)
let () =
  (* parse the command line args once quantity constraints met *)
  Arg.parse speclist anon_fun usage_msg;

  (* Checking there are valid number of arguments *)
  if Array.length Sys.argv <= 4 then (
    prerr_endline (Arg.usage_string speclist "Not enough arguments supplied");
    exit 1);

  (* make sure that the file extension is for juice *)
  if not (Filename.extension !source_file = ".jce") then (
    prerr_endline
      (Arg.usage_string speclist "Source files must have \".jce\" extension");
    exit 1);

  let scanner = new Front_end.Lexer.Scanner.scanner !source_file in
  let tokenizer = new Front_end.Lexer.Tokenizer.tokenizer scanner in
  let parser = new Front_end.Parser.parser tokenizer in
  let name_analyzer = new Front_end.Ast.Semantic.Name_analyzer.name_analyzer in
  let type_checker = new Front_end.Ast.Semantic.Type_checker.type_checker in
  let lvalue_analyzer =
    new Front_end.Ast.Semantic.Lvalue_analyzer.lvalue_analyzer
  in

  match !command with
  | "lexer" ->
      let tokens = tokenizer#collect in
      List.iter (fun x -> Front_end.Lexer.Tokenizer.print_token x) tokens;

      print_endline "lexer successful!"
  | "parser" ->
      let ast = parser#parse in
      (*Ast.Node.print_ast_node ast;*)
      name_analyzer#visit ast;
      (*Ast.Node.print_ast_node ast;*)
      type_checker#visit ast;
      (*Ast.Node.print_ast_node ast;*)
      lvalue_analyzer#visit ast;

      print_endline "parser successful!"
  | "codegen" ->
      let ast = parser#parse in
      let sections = Back_end.X64.Code_generator.alloc_global_mem ast in
      List.iter
        (fun x ->
          print_string (Back_end.X64.Code_generator.string_of_section x))
        sections;
      ()
  | _ ->
      Arg.usage speclist ("Invalid <command> given: " ^ !command);
      exit 1
