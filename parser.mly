%{
open Ast
open Operator

let parse_error msg = Parser_error.throw msg

%}

%token MINUS PLUS DIV MUL POW EQ LESS LESSEQ GREAT GREATEQ
%token <float> FLOAT
%token <int>   INT
%token <string> ID SWITCH
%token IF ELSE WHILE FOR REF COL DEF SET GSET CANCAST PREFIX
%token WHEN SCOL BREAK RETURN NONE ERROR CAT END
%token ESEP AND OR LPAR RPAR RCUR LCUR NOT TRUE FALSE LBR RBR BASE

%nonassoc RBR
%right SET
%left CAT
%left OR
%left AND
%left EQ LESS LESSEQ GREAT GREATEQ CANCAST
%left PLUS MINUS PREFIX
%left MUL DIV
%nonassoc NOT
%right POW
%nonassoc UMINUS



%start statement
%start program
%type <Ast.t> statement
%type <Ast.t list> program

%%

program: statements END {$1}

statements:
| statement {[$1]}
| statement statements {$1::$2}
| error {parse_error "expected statement"}
;

statement:
| SCOL {Unit}
| ERROR expr SCOL  {Abort $2}
| expr SCOL {Apply ($1,[])}
| expr arguments SCOL {Apply ($1,$2) }
| RETURN expr SCOL {Return $2}
| simple_expr LBR expr RBR SET expr SCOL {ArraySet ($1,$3,$6)}
| simple_expr SET expr SCOL {Set ($1,$3)}
| simple_expr GSET expr SCOL {GSet ($1,$3)}
| BREAK SCOL {Break}
| block {$1}
| DEF ID LPAR parameters RPAR block { Fun ($2,$4,$6)}
| DEF ID LPAR RPAR block { Fun ($2,[],$5)}
| IF LPAR expr RPAR statement ELSE statement {IfThenElse ($3,$5,$7)}
| WHEN LPAR expr RPAR statement {IfThenElse($3,$5,Unit)}
| WHILE LPAR expr RPAR statement {While ($3,$5)}
| FOR LPAR ID EQ expr RPAR statement {For ($3,$5,$7)}
;

expr:
| simple_expr {$1}
| MINUS expr %prec UMINUS {Unary (UMinus,$2)}
| NOT expr {Unary (Not,$2)}
| BASE  expr %prec UMINUS {Unary (Base,$2)}
| expr EQ  expr {Eq ($1,$3)}
| expr CAT expr     {Binary (ArrayCat, $1,$3)}
| expr PREFIX expr  {Binary (Prefix,$1,$3)}
| expr PLUS expr    {Binary (Plus, $1,$3)}
| expr MINUS expr   {Binary (Minus, $1,$3)}
| expr MUL expr     {Binary (Mul,$1,$3)}
| expr DIV expr     {Binary (Div, $1,$3)}
| expr POW expr     {Binary (Pow,$1,$3)}
| expr AND expr     {Binary (And, $1,$3)}
| expr OR  expr     {Binary (Or, $1,$3)}
| expr LESS expr    {Binary (Less,$1,$3)}
| expr LESSEQ expr  {Binary (LessEq, $1,$3)}
| expr GREATEQ expr {Binary (GreatEq,$1,$3)}
| expr GREAT expr   {Binary (Great, $1,$3)}
| expr CANCAST expr {Binary (CanCast, $1, $3)}
;

simple_expr:
| LPAR expr RPAR {$2}
| LPAR expr error {parse_error "Unclosed right paren"}
| TRUE {Bool true}
| FALSE {Bool false}
| INT {Num (float_of_int $1)}
| FLOAT {Num $1}
| REF LCUR expr RCUR {Ref $3}
| REF LCUR error {parse_error "Unclosed }"}
| REF ID {Ref (Id $2)}
| ID {Id $1}
| NONE {Unit}
| LCUR expr COL expr RCUR {Seq ($2,Num 1.,$4)}
| LCUR expr COL expr COL expr RCUR {Seq ($2,$4,$6)}
| array {Array $1}
| REF LPAR expr arguments RPAR  {Apply ($3,$4) }
| REF LPAR expr RPAR  {Apply ($3,[])}
| REF LPAR error RPAR {parse_error "Expected expression"}
| simple_expr LBR expr RBR {ArrayGet ($1,$3)}
;

block:
| LCUR statements RCUR {Block $2}
;

array:
| LCUR RCUR {[]}
| LCUR expr_list RCUR {$2}
;

expr_list:
| expr {[$1]}
| expr ESEP expr_list {$1::$3}
;

arguments:
| argument {[$1]}
| argument arguments {$1::$2}
| error {parse_error "Expected argument"}

;

argument:
| simple_expr {None,$1}
| SWITCH {Some $1, (Bool true)}
| SWITCH EQ simple_expr   {Some $1,$3}
;

parameters:
| parameter {[$1]}
| parameter ESEP parameters {$1::$3}
| error {parse_error "Expected parameter"}
;

parameter:
| ID {$1,None}
| SWITCH {$1, (Some (Bool false))}
| SWITCH EQ expr {$1, Some $3}

;






