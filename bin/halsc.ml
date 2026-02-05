(** HAL/S Compiler Driver

    halsc - compile HAL/S source files
*)

let usage = "Usage: halsc [options] <file.hal>\n\
             Options:"

let input_file = ref None
let dump_ast = ref false
let dump_tokens = ref false

let speclist = [
  ("-ast", Arg.Set dump_ast, "Dump the AST");
  ("-tokens", Arg.Set dump_tokens, "Dump tokens from lexer");
]

let set_input f =
  match !input_file with
  | None -> input_file := Some f
  | Some _ -> raise (Arg.Bad "Only one input file allowed")

let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <- {
    lexbuf.Lexing.lex_curr_p with
    Lexing.pos_fname = filename
  };
  try
    let ast = Hals.Parser.compilation_unit Hals.Lexer.token lexbuf in
    close_in ic;
    ast
  with
  | Hals.Lexer.Lexer_error (msg, pos) ->
    close_in ic;
    Printf.eprintf "%s:%d:%d: Lexer error: %s\n"
      pos.Lexing.pos_fname
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
      msg;
    exit 1
  | Hals.Parser.Error ->
    close_in ic;
    let pos = lexbuf.Lexing.lex_curr_p in
    Printf.eprintf "%s:%d:%d: Parse error\n"
      pos.Lexing.pos_fname
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
    exit 1

let dump_tokens_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let rec loop () =
    let tok = Hals.Lexer.token lexbuf in
    Printf.printf "%s\n" (match tok with
      | Hals.Parser.EOF -> "EOF"
      | Hals.Parser.INT_LIT n -> Printf.sprintf "INT_LIT(%d)" n
      | Hals.Parser.FLOAT_LIT f -> Printf.sprintf "FLOAT_LIT(%f)" f
      | Hals.Parser.CHAR_LIT s -> Printf.sprintf "CHAR_LIT(%s)" s
      | Hals.Parser.BIT_LIT s -> Printf.sprintf "BIT_LIT(%s)" s
      | Hals.Parser.IDENT s -> Printf.sprintf "IDENT(%s)" s
      | Hals.Parser.PROGRAM -> "PROGRAM"
      | Hals.Parser.TASK -> "TASK"
      | Hals.Parser.FUNCTION -> "FUNCTION"
      | Hals.Parser.PROCEDURE -> "PROCEDURE"
      | Hals.Parser.DECLARE -> "DECLARE"
      | Hals.Parser.DO -> "DO"
      | Hals.Parser.END -> "END"
      | Hals.Parser.IF -> "IF"
      | Hals.Parser.THEN -> "THEN"
      | Hals.Parser.ELSE -> "ELSE"
      | Hals.Parser.SEMI -> "SEMI"
      | Hals.Parser.PLUS -> "PLUS"
      | Hals.Parser.MINUS -> "MINUS"
      | Hals.Parser.STAR -> "STAR"
      | Hals.Parser.EQ -> "EQ"
      | Hals.Parser.LPAREN -> "LPAREN"
      | Hals.Parser.RPAREN -> "RPAREN"
      | _ -> "OTHER");
    if tok <> Hals.Parser.EOF then loop ()
  in
  loop ();
  close_in ic

let print_ast _ast =
  Printf.printf "AST parsed successfully!\n";
  Printf.printf "Blocks: %d\n" (List.length _ast.Hals.Ast.unit_blocks);
  List.iter (fun block ->
    Printf.printf "  Block: %s (%s)\n"
      block.Hals.Ast.block_name
      (match block.Hals.Ast.block_type with
       | Hals.Ast.Program -> "PROGRAM"
       | Hals.Ast.Task -> "TASK"
       | Hals.Ast.Function _ -> "FUNCTION"
       | Hals.Ast.Procedure -> "PROCEDURE"
       | Hals.Ast.Compool -> "COMPOOL"
       | Hals.Ast.Update -> "UPDATE");
    Printf.printf "    Declarations: %d\n" (List.length block.Hals.Ast.block_decls);
    Printf.printf "    Statements: %d\n" (List.length block.Hals.Ast.block_body)
  ) _ast.Hals.Ast.unit_blocks

let () =
  Arg.parse speclist set_input usage;
  match !input_file with
  | None ->
    Printf.eprintf "Error: No input file specified\n";
    Arg.usage speclist usage;
    exit 1
  | Some filename ->
    if !dump_tokens then
      dump_tokens_file filename
    else begin
      let ast = parse_file filename in
      if !dump_ast then
        print_ast ast
      else
        Printf.printf "Compilation successful.\n"
    end
