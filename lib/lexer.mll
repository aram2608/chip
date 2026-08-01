{
    open Parser
    exception Lexing_error of string
    let trim_quotes s = String.sub s 1 (String.length s - 2)
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int_lit = ['0'-'9']+
let flt_lit = ['0'-'9']+ [ '.' ] ['0'-'9']+
let str_lit = '"' ([^ '"' '\\' '\n' '\r'] | '\\' _)* '"'

rule token = parse
  | white          { token lexbuf }
  | newline        { Lexing.new_line lexbuf; token lexbuf }
  | "while"        { WHILE }
  | "begin"        { BEGIN }
  | "end"          { END }
  | "do"           { DO }
  | "if"           { IF }
  | "then"         { THEN }
  | "else"         { ELSE }
  | "print"        { PRINT }
  | "proc"         { PROC  }
  | ";"            { SEMI }
  | ":="           { ASSIGN }
  | "+"            { PLUS }
  | "-"            { MINUS }
  | "/"            { SLASH }
  | "*"            { STAR }
  | "{"            { LBRACE }
  | "}"            { RBRACE }
  | "("            { LPAREN }
  | ")"            { RPAREN }
  | str_lit as lxm { STRING (trim_quotes lxm) }
  | id as word     { ID word }
  | int_lit as lxm { INT (int_of_string lxm) }
  | flt_lit as lxm { FLOAT (float_of_string lxm) }
  | eof            { EOF }
  | _ as c         { raise (Lexing_error ("Unexpected character: " ^ String.make 1 c)) }

