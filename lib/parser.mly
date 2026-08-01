%token <string> ID
%token <string> BUILTIN
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token WHILE BEGIN END DO IF THEN ELSE FOR SEMI PROC COMMA
%token DECL ASSIGN PLUS MINUS SLASH STAR LPAREN RPAREN 
%token LESSER GREATER LEQUAL GEQUAL EQEQ NEQUAL LBRACE RBRACE RETURN EOF


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

%left LESSER GREATER LEQUAL GEQUAL EQEQ NEQUAL
%left PLUS MINUS
%left STAR SLASH

%start <Ast.stm> prog

%%

/* Grammar Rules */

stmlist:
   l = list(stm)                          { l }
;

prog:
   l = stmlist EOF                        { Ast.Seq l }
;

block:
    LBRACE l = stmlist RBRACE             { Ast.Block l }
;

arglist:
    l = separated_list(COMMA, expr)       { l }
;

stm:
    s = simple_stm SEMI                   { s }
  | WHILE c = expr body = stm             { Ast.While (c, body) }
  | IF c = expr THEN t = stm %prec THEN   { Ast.If (c, t) }
  | IF c = expr THEN t = stm ELSE e = stm { Ast.IfElse (c, t, e) }
  | FOR i = simple_stm SEMI 
        c = expr SEMI 
        s = simple_stm
        b = block                         { Ast.For (i, c, s, b) }
  | b = block                             { b }
  | BEGIN l = stmlist END                 { Ast.Seq l }
;

simple_stm:
    x = ID DECL y = expr                  { Ast.Decl (x, y) }
  | x = ID ASSIGN y = expr                { Ast.Assign (x, y) }
  | c = expr                              { Ast.ExpStmt c }
  | n = BUILTIN LPAREN 
    args = arglist RPAREN                 { Ast.BuiltinCall (n, args) }
;

expr:
    x = ID                                { Ast.Var x }
  | n = INT                               { Ast.IntLit n }
  | n = FLOAT                             { Ast.FloatLit n }
  | n = STRING                            { Ast.StringLit n }
  | lhs = expr op = binop rhs = expr      { Ast.Binop (op, lhs, rhs) }
  | lhs = expr op = comp rhs = expr       { Ast.Comp  (op, lhs, rhs) }
  | LPAREN e = expr RPAREN                { e }
;

%inline binop:
  | PLUS  { Ast.Add }
  | MINUS { Ast.Sub }
  | STAR  { Ast.Mul }
  | SLASH { Ast.Div }

%inline comp:
  | LESSER  { Ast.Lesser }
  | GREATER { Ast.Greater }
  | LEQUAL  { Ast.LEqual }
  | GEQUAL  { Ast.GEqual }
  | EQEQ    { Ast.Equal  }
  | NEQUAL  { Ast.NEqual }

%%
