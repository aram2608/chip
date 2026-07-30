{
    open Parser
    exception Lexing_error of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int_lit = ['0'-'9']+
let flt_lit = ['0'-'9']+ [ '.' ] ['0'-'9']+

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
  | ";"            { SEMI }
  | ":="           { ASSIGN }
  | "+"            { PLUS }
  | "-"            { MINUS }
  | "/"            { SLASH }
  | "*"            { STAR }
  | id as word     { ID word }
  | int_lit as lxm { INT (int_of_string lxm) }
  | flt_lit as lxm { FLOAT (float_of_string lxm) }
  | eof            { EOF }
  | _ as c         { raise (Lexing_error ("Unexpected character: " ^ String.make 1 c)) }

