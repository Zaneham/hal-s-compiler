(** HAL/S Abstract Syntax Tree

    Based on the HAL/S Language Specification and the
    HAL/S-FC Compiler System Specification (IR-95-6, 1977)
*)

(** Source location for error reporting *)
type loc = {
  file: string;
  line: int;
  col: int;
}

(** HAL/S has both single and double precision *)
type precision =
  | Single
  | Double

(** Basic HAL/S types *)
type scalar_type =
  | Integer of precision
  | Scalar of precision  (* floating point *)
  | Boolean

(** Aggregate types - vectors and matrices are first-class *)
type aggregate_type =
  | Vector of int * precision        (* dimension, precision *)
  | Matrix of int * int * precision  (* rows, cols, precision *)
  | BitString of int                 (* length *)
  | CharString of int                (* max length *)

(** Full type system *)
type hals_type =
  | TScalar of scalar_type
  | TAggregate of aggregate_type
  | TStructure of string                    (* structure template name *)
  | TArray of hals_type * int list          (* element type, dimensions *)
  | TName of hals_type                      (* NAME type - pointer *)
  | TLabel
  | TEvent

(** Literals *)
type literal =
  | LitInt of int
  | LitFloat of float
  | LitBit of string      (* e.g., "1011" *)
  | LitChar of string
  | LitBool of bool

(** Binary operators *)
type binop =
  | Add | Sub | Mul | Div
  | Mod                    (* integer modulo *)
  | Exp                    (* exponentiation - written as superscript in HAL/S *)
  | Eq | Ne | Lt | Le | Gt | Ge
  | And | Or | Xor
  | Cat                    (* string/bit concatenation *)
  | Cross                  (* vector cross product *)
  | Dot                    (* vector dot product *)

(** Unary operators *)
type unop =
  | Neg | Not
  | Transpose             (* matrix transpose, written as T superscript *)
  | Inverse               (* matrix inverse *)
  | Abval                 (* absolute value *)
  | Unit                  (* unit vector *)
  | Trace                 (* matrix trace *)

(** Subscript - HAL/S supports partitioning and component access *)
type subscript =
  | SubIndex of expr                     (* single index *)
  | SubRange of expr * expr              (* range i TO j *)
  | SubStar                              (* all elements *)
  | SubComponent of int                  (* vector component .1, .2, .3 *)

(** Expression *)
and expr = {
  expr_desc: expr_desc;
  expr_loc: loc;
  mutable expr_type: hals_type option;  (* filled in by type checker *)
}

and expr_desc =
  | ELit of literal
  | EVar of string
  | EBinop of binop * expr * expr
  | EUnop of unop * expr
  | ESubscript of expr * subscript list
  | ECall of string * expr list          (* function call *)
  | EConvert of hals_type * expr         (* type conversion *)
  | EVector of expr list                 (* vector constructor *)
  | EMatrix of expr list list            (* matrix constructor, row-major *)
  | EShaping of hals_type * expr         (* VECTOR(m), MATRIX(m,n) shaping *)

(** Assignment target *)
type lvalue =
  | LVar of string
  | LSubscript of lvalue * subscript list
  | LDeref of lvalue                      (* dereference NAME variable *)

(** Statement *)
type stmt = {
  stmt_desc: stmt_desc;
  stmt_loc: loc;
}

and stmt_desc =
  (* Basic statements *)
  | SAssign of lvalue * expr
  | SCall of string * expr list           (* procedure call *)
  | SReturn of expr option
  | SIf of expr * stmt list * stmt list   (* if-then-else *)
  | SDo of do_stmt                        (* DO loops *)
  | SExit of string option                (* EXIT [label] *)
  | SRepeat of string option              (* REPEAT [label] *)
  | SGoto of string
  | SLabel of string * stmt               (* labeled statement *)

  (* Real-time statements *)
  | SSchedule of schedule_spec
  | SCancel of expr                       (* CANCEL task *)
  | STerminate of expr option             (* TERMINATE [task] *)
  | SWait of wait_spec
  | SSignal of string                     (* SIGNAL event *)
  | SSet of string                        (* SET event *)
  | SReset of string                      (* RESET event *)
  | SUpdatePriority of expr * expr        (* UPDATE PRIORITY task TO n *)

  (* I/O statements *)
  | SRead of io_spec
  | SWrite of io_spec
  | SReadall of io_spec
  | SFile of io_spec

(** DO loop variants *)
and do_stmt =
  | DoForever of stmt list                          (* DO; ... END; *)
  | DoWhile of expr * stmt list                     (* DO WHILE cond; *)
  | DoUntil of stmt list * expr                     (* DO UNTIL cond; (test at end) *)
  | DoFor of string * expr * expr * expr option * stmt list
      (* DO var = start TO end [BY step]; *)
  | DoCase of expr * stmt list list                 (* DO CASE expr; ... END; *)

(** SCHEDULE statement options *)
and schedule_spec = {
  sched_task: string;
  sched_on: expr option;           (* ON event *)
  sched_at: expr option;           (* AT time *)
  sched_in: expr option;           (* IN delay - same as AFTER *)
  sched_priority: expr option;
  sched_dependent: bool;
  sched_repeat: repeat_spec option;
}

and repeat_spec = {
  repeat_every: expr option;
  repeat_after: expr option;
  repeat_until: expr option;
  repeat_while: expr option;
}

(** WAIT statement options *)
and wait_spec = {
  wait_for: expr option;           (* WAIT FOR event *)
  wait_until: expr option;         (* WAIT UNTIL time/condition *)
}

(** I/O specification *)
and io_spec = {
  io_channel: expr;
  io_items: expr list;
}

(** Declarations *)
type decl = {
  decl_desc: decl_desc;
  decl_loc: loc;
}

and decl_desc =
  | DeclVar of var_decl
  | DeclStructure of structure_template
  | DeclReplace of string * string list * string  (* REPLACE macro *)

and var_decl = {
  var_names: string list;
  var_type: hals_type;
  var_init: expr option;
  var_attrs: var_attrs;
}

and var_attrs = {
  attr_automatic: bool;           (* AUTOMATIC - stack allocated *)
  attr_static: bool;              (* STATIC - global lifetime *)
  attr_initial: bool;             (* INITIAL - initialized at load *)
  attr_constant: bool;            (* CONSTANT - immutable *)
  attr_dense: bool;               (* DENSE - packed storage *)
  attr_aligned: bool;             (* ALIGNED - boundary aligned *)
  attr_remote: bool;              (* REMOTE - in separate memory bank *)
  attr_rigid: bool;               (* RIGID - structure not reordered *)
  attr_latched: bool;             (* LATCHED - event stays set *)
}

and structure_template = {
  struct_name: string;
  struct_fields: (string * hals_type) list;
  struct_rigid: bool;
}

(** Block types - the compilation units of HAL/S *)
type block_type =
  | Program
  | Task
  | Function of hals_type           (* return type *)
  | Procedure
  | Compool                         (* shared data pool *)
  | Update                          (* incremental update block *)

(** Parameter passing modes *)
type param_mode =
  | ParamIn                         (* input, pass by value for scalars *)
  | ParamAssign                     (* output, pass by reference *)
  | ParamInout                      (* input/output *)

(** Function/Procedure parameter *)
type param = {
  param_name: string;
  param_type: hals_type;
  param_mode: param_mode;
}

(** Block definition *)
type block = {
  block_name: string;
  block_type: block_type;
  block_params: param list;
  block_decls: decl list;
  block_body: stmt list;
  block_reentrant: bool;
  block_access: string list;        (* ACCESS list for COMPOOL *)
  block_loc: loc;
}

(** Compilation unit *)
type compilation_unit = {
  unit_blocks: block list;
  unit_compools: string list;       (* external COMPOOLs used *)
}

(** Default attributes *)
let default_attrs = {
  attr_automatic = false;
  attr_static = false;
  attr_initial = false;
  attr_constant = false;
  attr_dense = false;
  attr_aligned = false;
  attr_remote = false;
  attr_rigid = false;
  attr_latched = false;
}

(** Helper to create a located expression *)
let mk_expr loc desc = { expr_desc = desc; expr_loc = loc; expr_type = None }

(** Helper to create a located statement *)
let mk_stmt loc desc = { stmt_desc = desc; stmt_loc = loc }
