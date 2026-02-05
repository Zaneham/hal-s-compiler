(** HAL/S Parser

    Grammar based on HAL/S Language Specification.
    Uses Menhir for LR parsing.
*)

%{
open Ast

let loc startpos _endpos = {
  file = startpos.Lexing.pos_fname;
  line = startpos.Lexing.pos_lnum;
  col = startpos.Lexing.pos_cnum - startpos.Lexing.pos_bol;
}

let _dummy_loc = { file = ""; line = 0; col = 0 }
%}

(* Tokens *)
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <string> CHAR_LIT
%token <string> BIT_LIT
%token <string> IDENT

(* Keywords - Blocks *)
%token PROGRAM TASK FUNCTION PROCEDURE COMPOOL UPDATE CLOSE REENTRANT

(* Keywords - Declarations *)
%token DECLARE STRUCTURE REPLACE ARRAY VECTOR MATRIX
%token INTEGER SCALAR BOOLEAN BIT CHARACTER EVENT LABEL_KW NAME
%token AUTOMATIC STATIC INITIAL CONSTANT DENSE ALIGNED REMOTE RIGID LATCHED
%token SINGLE DOUBLE

(* Keywords - Statements *)
%token DO END IF THEN ELSE WHILE UNTIL FOR TO BY CASE
%token RETURN EXIT REPEAT GO GOTO CALL

(* Keywords - Real-time *)
%token SCHEDULE CANCEL TERMINATE WAIT SIGNAL SET RESET
%token PRIORITY DEPENDENT ON AT IN AFTER EVERY

(* Keywords - I/O *)
%token READ WRITE READALL FILE

(* Keywords - Operators *)
%token NOT AND OR XOR CAT MOD TRUE FALSE

(* Keywords - Access *)
%token ACCESS EXCLUSIVE

(* Operators *)
%token PLUS MINUS STAR SLASH STARSTAR
%token EQ NE LT GT LE GE
%token CONCAT

(* Punctuation *)
%token LPAREN RPAREN LBRACKET RBRACKET
%token SEMI COLON COMMA DOT DOLLAR

%token EOF

(* Precedence - lowest to highest *)
%left OR XOR
%left AND
%nonassoc NOT
%nonassoc EQ NE LT GT LE GE
%left CONCAT CAT
%left PLUS MINUS
%left STAR SLASH MOD
%right STARSTAR
%nonassoc UMINUS

%start <Ast.compilation_unit> compilation_unit

%%

compilation_unit:
  | blocks = list(block) EOF
    { { unit_blocks = blocks; unit_compools = [] } }

block:
  | PROGRAM name = IDENT SEMI
    decls = list(declaration)
    body = list(statement)
    CLOSE _name2 = IDENT SEMI
    { { block_name = name;
        block_type = Program;
        block_params = [];
        block_decls = decls;
        block_body = body;
        block_reentrant = false;
        block_access = [];
        block_loc = loc $startpos $endpos } }

  | TASK name = IDENT SEMI
    decls = list(declaration)
    body = list(statement)
    CLOSE _name2 = IDENT SEMI
    { { block_name = name;
        block_type = Task;
        block_params = [];
        block_decls = decls;
        block_body = body;
        block_reentrant = false;
        block_access = [];
        block_loc = loc $startpos $endpos } }

  | hdr = function_header SEMI
    decls = list(declaration)
    body = list(statement)
    CLOSE _name2 = IDENT SEMI
    { let (name, ret_type, params, reent) = hdr in
      { block_name = name;
        block_type = Function ret_type;
        block_params = params;
        block_decls = decls;
        block_body = body;
        block_reentrant = reent;
        block_access = [];
        block_loc = loc $startpos $endpos } }

  | hdr = procedure_header SEMI
    decls = list(declaration)
    body = list(statement)
    CLOSE _name2 = IDENT SEMI
    { let (name, params, reent) = hdr in
      { block_name = name;
        block_type = Procedure;
        block_params = params;
        block_decls = decls;
        block_body = body;
        block_reentrant = reent;
        block_access = [];
        block_loc = loc $startpos $endpos } }

function_header:
  | name = IDENT COLON reent = boption(REENTRANT) FUNCTION
    params = loption(param_list) return_type = type_spec
    { (name, return_type, params, reent) }

procedure_header:
  | name = IDENT COLON reent = boption(REENTRANT) PROCEDURE
    params = loption(param_list)
    { (name, params, reent) }

param_list:
  | LPAREN params = separated_nonempty_list(COMMA, param) RPAREN
    { params }

param:
  | name = IDENT
    { { param_name = name; param_type = TScalar (Integer Single); param_mode = ParamIn } }

(* Declarations *)
declaration:
  | DECLARE decl = var_decl SEMI
    { { decl_desc = DeclVar decl; decl_loc = loc $startpos $endpos } }

var_decl:
  | names = separated_nonempty_list(COMMA, IDENT) t = type_spec attrs = var_attrs
    { { var_names = names;
        var_type = t;
        var_init = None;
        var_attrs = attrs } }

  | names = separated_nonempty_list(COMMA, IDENT) t = type_spec
    attrs = var_attrs INITIAL LPAREN e = expr RPAREN
    { { var_names = names;
        var_type = t;
        var_init = Some e;
        var_attrs = { attrs with attr_initial = true } } }

var_attrs:
  | (* empty *)
    { default_attrs }
  | AUTOMATIC attrs = var_attrs
    { { attrs with attr_automatic = true } }
  | STATIC attrs = var_attrs
    { { attrs with attr_static = true } }
  | CONSTANT attrs = var_attrs
    { { attrs with attr_constant = true } }

type_spec:
  | INTEGER prec = precision
    { TScalar (Integer prec) }
  | SCALAR prec = precision
    { TScalar (Scalar prec) }
  | BOOLEAN
    { TScalar Boolean }
  | VECTOR LPAREN n = INT_LIT RPAREN prec = precision
    { TAggregate (Vector (n, prec)) }
  | MATRIX LPAREN m = INT_LIT COMMA n = INT_LIT RPAREN prec = precision
    { TAggregate (Matrix (m, n, prec)) }
  | BIT LPAREN n = INT_LIT RPAREN
    { TAggregate (BitString n) }
  | CHARACTER LPAREN n = INT_LIT RPAREN
    { TAggregate (CharString n) }
  | EVENT
    { TEvent }

precision:
  | (* empty *) { Single }
  | SINGLE      { Single }
  | DOUBLE      { Double }

(* Statements *)
statement:
  | lv = lvalue EQ e = expr SEMI
    { mk_stmt (loc $startpos $endpos) (SAssign (lv, e)) }

  | IF e = expr THEN s1 = list(statement) ELSE s2 = list(statement) END SEMI
    { mk_stmt (loc $startpos $endpos) (SIf (e, s1, s2)) }

  | IF e = expr THEN s1 = list(statement) END SEMI
    { mk_stmt (loc $startpos $endpos) (SIf (e, s1, [])) }

  | DO SEMI body = list(statement) END SEMI
    { mk_stmt (loc $startpos $endpos) (SDo (DoForever body)) }

  | DO WHILE e = expr SEMI body = list(statement) END SEMI
    { mk_stmt (loc $startpos $endpos) (SDo (DoWhile (e, body))) }

  | DO FOR v = IDENT EQ e1 = expr TO e2 = expr SEMI
    body = list(statement) END SEMI
    { mk_stmt (loc $startpos $endpos) (SDo (DoFor (v, e1, e2, None, body))) }

  | DO FOR v = IDENT EQ e1 = expr TO e2 = expr BY e3 = expr SEMI
    body = list(statement) END SEMI
    { mk_stmt (loc $startpos $endpos) (SDo (DoFor (v, e1, e2, Some e3, body))) }

  | RETURN SEMI
    { mk_stmt (loc $startpos $endpos) (SReturn None) }

  | RETURN e = expr SEMI
    { mk_stmt (loc $startpos $endpos) (SReturn (Some e)) }

  | EXIT SEMI
    { mk_stmt (loc $startpos $endpos) (SExit None) }

  | EXIT name = IDENT SEMI
    { mk_stmt (loc $startpos $endpos) (SExit (Some name)) }

  | CALL name = IDENT args = loption(arg_list) SEMI
    { mk_stmt (loc $startpos $endpos) (SCall (name, args)) }

  (* Real-time statements *)
  | SCHEDULE name = IDENT sched = schedule_opts SEMI
    { mk_stmt (loc $startpos $endpos)
        (SSchedule { sched_task = name;
                     sched_on = sched.sched_on;
                     sched_at = sched.sched_at;
                     sched_in = sched.sched_in;
                     sched_priority = sched.sched_priority;
                     sched_dependent = sched.sched_dependent;
                     sched_repeat = sched.sched_repeat }) }

  | TERMINATE SEMI
    { mk_stmt (loc $startpos $endpos) (STerminate None) }

  | TERMINATE e = expr SEMI
    { mk_stmt (loc $startpos $endpos) (STerminate (Some e)) }

  | CANCEL e = expr SEMI
    { mk_stmt (loc $startpos $endpos) (SCancel e) }

  | SIGNAL name = IDENT SEMI
    { mk_stmt (loc $startpos $endpos) (SSignal name) }

  | SET name = IDENT SEMI
    { mk_stmt (loc $startpos $endpos) (SSet name) }

  | RESET name = IDENT SEMI
    { mk_stmt (loc $startpos $endpos) (SReset name) }

  | WAIT SEMI
    { mk_stmt (loc $startpos $endpos)
        (SWait { wait_for = None; wait_until = None }) }

  | WAIT FOR e = expr SEMI
    { mk_stmt (loc $startpos $endpos)
        (SWait { wait_for = Some e; wait_until = None }) }

  | WAIT UNTIL e = expr SEMI
    { mk_stmt (loc $startpos $endpos)
        (SWait { wait_for = None; wait_until = Some e }) }

schedule_opts:
  | (* empty *)
    { { sched_task = ""; sched_on = None; sched_at = None;
        sched_in = None; sched_priority = None;
        sched_dependent = false; sched_repeat = None } }
  | ON e = expr rest = schedule_opts
    { { rest with sched_on = Some e } }
  | AT e = expr rest = schedule_opts
    { { rest with sched_at = Some e } }
  | IN e = expr rest = schedule_opts
    { { rest with sched_in = Some e } }
  | PRIORITY LPAREN e = expr RPAREN rest = schedule_opts
    { { rest with sched_priority = Some e } }
  | DEPENDENT rest = schedule_opts
    { { rest with sched_dependent = true } }

arg_list:
  | LPAREN args = separated_list(COMMA, expr) RPAREN
    { args }

(* L-values *)
lvalue:
  | name = IDENT
    { LVar name }
  | lv = lvalue LBRACKET subs = separated_nonempty_list(COMMA, subscript) RBRACKET
    { LSubscript (lv, subs) }

subscript:
  | e = expr
    { SubIndex e }
  | e1 = expr TO e2 = expr
    { SubRange (e1, e2) }
  | STAR
    { SubStar }

(* Expressions *)
expr:
  | e = expr_or
    { e }

expr_or:
  | e1 = expr_or OR e2 = expr_and
    { mk_expr (loc $startpos $endpos) (EBinop (Or, e1, e2)) }
  | e1 = expr_or XOR e2 = expr_and
    { mk_expr (loc $startpos $endpos) (EBinop (Xor, e1, e2)) }
  | e = expr_and
    { e }

expr_and:
  | e1 = expr_and AND e2 = expr_not
    { mk_expr (loc $startpos $endpos) (EBinop (And, e1, e2)) }
  | e = expr_not
    { e }

expr_not:
  | NOT e = expr_not
    { mk_expr (loc $startpos $endpos) (EUnop (Not, e)) }
  | e = expr_cmp
    { e }

expr_cmp:
  | e1 = expr_cat EQ e2 = expr_cat
    { mk_expr (loc $startpos $endpos) (EBinop (Eq, e1, e2)) }
  | e1 = expr_cat NE e2 = expr_cat
    { mk_expr (loc $startpos $endpos) (EBinop (Ne, e1, e2)) }
  | e1 = expr_cat LT e2 = expr_cat
    { mk_expr (loc $startpos $endpos) (EBinop (Lt, e1, e2)) }
  | e1 = expr_cat GT e2 = expr_cat
    { mk_expr (loc $startpos $endpos) (EBinop (Gt, e1, e2)) }
  | e1 = expr_cat LE e2 = expr_cat
    { mk_expr (loc $startpos $endpos) (EBinop (Le, e1, e2)) }
  | e1 = expr_cat GE e2 = expr_cat
    { mk_expr (loc $startpos $endpos) (EBinop (Ge, e1, e2)) }
  | e = expr_cat
    { e }

expr_cat:
  | e1 = expr_cat CONCAT e2 = expr_add
    { mk_expr (loc $startpos $endpos) (EBinop (Cat, e1, e2)) }
  | e1 = expr_cat CAT e2 = expr_add
    { mk_expr (loc $startpos $endpos) (EBinop (Cat, e1, e2)) }
  | e = expr_add
    { e }

expr_add:
  | e1 = expr_add PLUS e2 = expr_mul
    { mk_expr (loc $startpos $endpos) (EBinop (Add, e1, e2)) }
  | e1 = expr_add MINUS e2 = expr_mul
    { mk_expr (loc $startpos $endpos) (EBinop (Sub, e1, e2)) }
  | e = expr_mul
    { e }

expr_mul:
  | e1 = expr_mul STAR e2 = expr_pow
    { mk_expr (loc $startpos $endpos) (EBinop (Mul, e1, e2)) }
  | e1 = expr_mul SLASH e2 = expr_pow
    { mk_expr (loc $startpos $endpos) (EBinop (Div, e1, e2)) }
  | e1 = expr_mul MOD e2 = expr_pow
    { mk_expr (loc $startpos $endpos) (EBinop (Mod, e1, e2)) }
  | e = expr_pow
    { e }

expr_pow:
  | e1 = expr_unary STARSTAR e2 = expr_pow
    { mk_expr (loc $startpos $endpos) (EBinop (Exp, e1, e2)) }
  | e = expr_unary
    { e }

expr_unary:
  | MINUS e = expr_unary %prec UMINUS
    { mk_expr (loc $startpos $endpos) (EUnop (Neg, e)) }
  | e = expr_primary
    { e }

expr_primary:
  | n = INT_LIT
    { mk_expr (loc $startpos $endpos) (ELit (LitInt n)) }
  | f = FLOAT_LIT
    { mk_expr (loc $startpos $endpos) (ELit (LitFloat f)) }
  | s = CHAR_LIT
    { mk_expr (loc $startpos $endpos) (ELit (LitChar s)) }
  | b = BIT_LIT
    { mk_expr (loc $startpos $endpos) (ELit (LitBit b)) }
  | TRUE
    { mk_expr (loc $startpos $endpos) (ELit (LitBool true)) }
  | FALSE
    { mk_expr (loc $startpos $endpos) (ELit (LitBool false)) }
  | name = IDENT
    { mk_expr (loc $startpos $endpos) (EVar name) }
  | name = IDENT LPAREN args = separated_list(COMMA, expr) RPAREN
    { mk_expr (loc $startpos $endpos) (ECall (name, args)) }
  | e = expr_primary LBRACKET subs = separated_nonempty_list(COMMA, subscript) RBRACKET
    { mk_expr (loc $startpos $endpos) (ESubscript (e, subs)) }
  | LPAREN e = expr RPAREN
    { e }
  | VECTOR LPAREN args = separated_nonempty_list(COMMA, expr) RPAREN
    { mk_expr (loc $startpos $endpos) (EVector args) }
