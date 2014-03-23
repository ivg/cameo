{
open Lexing
open Parser

let lexical_error lexbuf = Parser_error.throw "Unknown lexeme"

}

let char  = ['a'-'z']
let digit = ['0'-'9']
let float =  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

let integer  = digit+
let position = digit+
let paref = ['1'-'9'] digit*
let id =  char (char | "_" | "." | "'" | digit)*
let txt = '"' (_ # '"' )* '"'
let newline = '\n' | '\r' | "\r\n" | "\n\r"

rule tokens = parse
  | eof {END}
  | integer as lxm {INT (int_of_string lxm)}
  | float as lxm {FLOAT (float_of_string lxm)}
  | "pi" {FLOAT (4. *. atan 1.)}
  | "nan" {FLOAT nan}
  | "infinity" {FLOAT infinity}
  | "neg_infinity" {FLOAT neg_infinity}
  | "None" {NONE}
  | "-"  {MINUS}
  | "+"  {PLUS}
  | "/"  {DIV}
  | "*"  {MUL}
  | "**" {POW}
  | "<-" {SET}
  | "<--"{GSET}
  | "="  {EQ}
  | "<"  {LESS}
  | "<=" {LESSEQ}
  | ">"  {GREAT}
  | ">=" {GREATEQ}
  | ":>?" {CANCAST}
  | "::"  {PREFIX}
  | "^"   {BASE}
  | "and" {AND}
  | "or" {OR}
  | "("  {LPAR}
  | ")"  {RPAR}
  | "not" {NOT}
  | "if" {IF}
  | "else" {ELSE}
  | "while" {WHILE}
  | "when" {WHEN}
  | "for"   {FOR}
  | "{"     {LCUR}
  | "}"     {RCUR}
  | "$"     {REF}
  | ","     {ESEP}
  | "def"   {DEF}
  | ":"     {COL}
  | ";"+    {SCOL}
  | "true"  {TRUE}
  | "false" {FALSE}
  | "["     {LBR}
  | "]"     {RBR}
  | "break" {BREAK}
  | "return" {RETURN}
  | "error"  {ERROR}
  | "@"      {CAT}
  | "#"      {comment lexbuf}
  | '-' (id as lxm) {SWITCH lxm}
  | "--" (id as lxm){SWITCH lxm}
  | id as lxm {ID lxm }
  | txt as lxm {ID (QString.unquote lxm) }
  | newline   {new_line lexbuf; tokens lexbuf}
  | [' ' '\t'] {tokens lexbuf}
  | _ {lexical_error lexbuf}
and comment = parse
  | newline {new_line lexbuf; tokens lexbuf}
  | _  {comment lexbuf}

{}


















