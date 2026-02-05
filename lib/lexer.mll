(** HAL/S Lexer

    HAL/S has a unique multi-line format where:
    - Main expression line (M-line) contains the primary code
    - Exponent line (E-line) above contains superscripts/exponents
    - Subscript line (S-line) below contains subscripts

    For simplicity, this initial lexer handles single-line HAL/S.
    Multi-line support will be added later.
*)

{
open Parser

exception Lexer_error of string * Lexing.position

let keywords = Hashtbl.create 64
let () = List.iter (fun (kw, tok) -> Hashtbl.add keywords kw tok) [
  (* Block keywords *)
  "PROGRAM", PROGRAM;
  "TASK", TASK;
  "FUNCTION", FUNCTION;
  "PROCEDURE", PROCEDURE;
  "COMPOOL", COMPOOL;
  "UPDATE", UPDATE;
  "CLOSE", CLOSE;
  "REENTRANT", REENTRANT;

  (* Declaration keywords *)
  "DECLARE", DECLARE;
  "STRUCTURE", STRUCTURE;
  "REPLACE", REPLACE;
  "ARRAY", ARRAY;
  "VECTOR", VECTOR;
  "MATRIX", MATRIX;
  "INTEGER", INTEGER;
  "SCALAR", SCALAR;
  "BOOLEAN", BOOLEAN;
  "BIT", BIT;
  "CHARACTER", CHARACTER;
  "EVENT", EVENT;
  "LABEL", LABEL_KW;
  "NAME", NAME;

  (* Attribute keywords *)
  "AUTOMATIC", AUTOMATIC;
  "STATIC", STATIC;
  "INITIAL", INITIAL;
  "CONSTANT", CONSTANT;
  "DENSE", DENSE;
  "ALIGNED", ALIGNED;
  "REMOTE", REMOTE;
  "RIGID", RIGID;
  "LATCHED", LATCHED;
  "SINGLE", SINGLE;
  "DOUBLE", DOUBLE;

  (* Statement keywords *)
  "DO", DO;
  "END", END;
  "IF", IF;
  "THEN", THEN;
  "ELSE", ELSE;
  "WHILE", WHILE;
  "UNTIL", UNTIL;
  "FOR", FOR;
  "TO", TO;
  "BY", BY;
  "CASE", CASE;
  "RETURN", RETURN;
  "EXIT", EXIT;
  "REPEAT", REPEAT;
  "GO", GO;
  "GOTO", GOTO;
  "CALL", CALL;

  (* Real-time keywords *)
  "SCHEDULE", SCHEDULE;
  "CANCEL", CANCEL;
  "TERMINATE", TERMINATE;
  "WAIT", WAIT;
  "SIGNAL", SIGNAL;
  "SET", SET;
  "RESET", RESET;
  "PRIORITY", PRIORITY;
  "DEPENDENT", DEPENDENT;
  "ON", ON;
  "AT", AT;
  "IN", IN;
  "AFTER", AFTER;
  "EVERY", EVERY;

  (* I/O keywords *)
  "READ", READ;
  "WRITE", WRITE;
  "READALL", READALL;
  "FILE", FILE;

  (* Operators and built-ins *)
  "NOT", NOT;
  "AND", AND;
  "OR", OR;
  "XOR", XOR;
  "CAT", CAT;
  "MOD", MOD;
  "TRUE", TRUE;
  "FALSE", FALSE;

  (* Access control *)
  "ACCESS", ACCESS;
  "EXCLUSIVE", EXCLUSIVE;
]

let lookup_ident s =
  try Hashtbl.find keywords (String.uppercase_ascii s)
  with Not_found -> IDENT s

(* Convert hex string to binary string *)
let hex_to_binary s =
  let hex_digit_to_bin c = match c with
    | '0' -> "0000" | '1' -> "0001" | '2' -> "0010" | '3' -> "0011"
    | '4' -> "0100" | '5' -> "0101" | '6' -> "0110" | '7' -> "0111"
    | '8' -> "1000" | '9' -> "1001"
    | 'A' | 'a' -> "1010" | 'B' | 'b' -> "1011"
    | 'C' | 'c' -> "1100" | 'D' | 'd' -> "1101"
    | 'E' | 'e' -> "1110" | 'F' | 'f' -> "1111"
    | _ -> ""
  in
  String.concat "" (List.map hex_digit_to_bin (String.to_seq s |> List.of_seq))
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alnum = alpha | digit | '_' | '$' | '@' | '#'
let ident = alpha alnum*

let int_lit = digit+
let exp = ['e' 'E'] ['+' '-']? digit+
let float_lit = digit+ '.' digit* exp? | digit+ exp | '.' digit+ exp?

let white = [' ' '\t']+
let newline = '\r'? '\n'

rule token = parse
  | white     { token lexbuf }
  | newline   { Lexing.new_line lexbuf; token lexbuf }

  (* Comments: C in column 1 makes entire line a comment *)
  | "/*"      { block_comment lexbuf }

  (* Literals *)
  | int_lit as n              { INT_LIT (int_of_string n) }
  | float_lit as f            { FLOAT_LIT (float_of_string f) }
  | '\'' ([^ '\'']* as s) '\'' { CHAR_LIT s }
  | '"' ([^ '"']* as s) '"'   { CHAR_LIT s }

  (* Bit literals: B'1010' or X'FF' *)
  | 'B' '\'' (['0' '1']+ as s) '\'' { BIT_LIT s }
  | 'X' '\'' (['0'-'9' 'A'-'F' 'a'-'f']+ as s) '\''
    { BIT_LIT (hex_to_binary s) }

  (* Operators *)
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { STAR }
  | '/'       { SLASH }
  | "**"      { STARSTAR }
  | '='       { EQ }
  | "^="      { NE }
  | "~="      { NE }
  | '<'       { LT }
  | '>'       { GT }
  | "<="      { LE }
  | ">="      { GE }
  | "||"      { CONCAT }

  (* Punctuation *)
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '['       { LBRACKET }
  | ']'       { RBRACKET }
  | ';'       { SEMI }
  | ':'       { COLON }
  | ','       { COMMA }
  | '.'       { DOT }
  | '$'       { DOLLAR }

  (* Identifiers and keywords *)
  | ident as id { lookup_ident id }

  | eof       { EOF }
  | _ as c    { raise (Lexer_error (Printf.sprintf "Unexpected character: %c" c,
                                    lexbuf.Lexing.lex_curr_p)) }

and block_comment = parse
  | "*/"      { token lexbuf }
  | newline   { Lexing.new_line lexbuf; block_comment lexbuf }
  | _         { block_comment lexbuf }
  | eof       { raise (Lexer_error ("Unterminated comment",
                                    lexbuf.Lexing.lex_curr_p)) }

(* End of lexer *)
