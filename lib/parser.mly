%token <string> ID
%token <int> INT
%token <float> FLOAT
%token WHILE BEGIN END DO IF THEN ELSE SEMI 
%token ASSIGN PRINT PLUS MINUS SLASH STAR LPAREN RPAREN LBRACE RBRACE EOF


(* For an if case where does else attach to? 
    if foo
    then if bar
    else printf "NO"

    Does the else attach to the outer if or the inner if?
    Since Menhir increases precedence with order, the else lookahead
    shifts the case into the inner if.

    IF_NODE
    /   \
  cond  IF_ELSE_NODE
        /   |    \
      cond  else  stmt
*)
%nonassoc THEN 
%nonassoc ELSE

%left PLUS MINUS
%left STAR SLASH

%start <Ast.stm> prog

%%

/* Grammar Rules */

stmlist:
   l = separated_nonempty_list(SEMI, stm) { l }
;

prog:
   l = stmlist EOF { Ast.Seq l }
;

stm:
    x = ID ASSIGN  y = expr {  Ast.Assign (x, y) }
  | WHILE c = expr DO body = stm { Ast.While (c, body) }
  | IF c = expr THEN t = stm { Ast.If (c, t) }
  | IF c = expr THEN t = stm ELSE e = stm { Ast.IfElse (c, t, e) }
  | PRINT c = expr { Ast.Print (c) }
  | c = expr { Ast.ExpStmt (c) }
  | LBRACE l = stmlist RBRACE { Ast.Block l }
  | BEGIN l = stmlist END { Ast.Seq l }
;

expr:
    x = ID  { Ast.Var x }
  | n = INT { Ast.IntLit n }
  | n = FLOAT { Ast.FloatLit n }
  | lhs = expr op = binop rhs = expr { Ast.Binop (op, lhs, rhs) }
;

%inline binop:
  | PLUS  { Ast.Add }
  | MINUS { Ast.Sub }
  | STAR  { Ast.Mul }
  | SLASH { Ast.Div }

%%
