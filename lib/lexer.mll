{
    open Parser
    open Syntax

    let get_pos lexbuf =
      let pos = Lexing.lexeme_start_p lexbuf in
      (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)

    let char_for_backslash = function
      | 'n' -> '\010'
      | 'r' -> '\013'
      | 'b' -> '\008'
      | 't' -> '\009'
      | c   -> c

       let string_buf = Buffer.create 256
}

let white = [' ' '\t']
let digit = ['0'-'9']
let num = '-'? digit+
let letter = ['a'-'z' 'A'-'Z' '_']
let var = letter (letter | digit)*
let ch = '\'' [^'\''] '\''
let inv_ch = '\'' [^'\''] [^'\''] 
let backslash_escapes =
    ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

rule lex = 
    parse
    (* Comments *)
    | "--" [^'\n']* '\n'    { Lexing.new_line lexbuf; lex lexbuf }
    (* Whitespaces *)
    | '\n'                  { Lexing.new_line lexbuf; lex lexbuf }
    | [' ' '\t']            { lex lexbuf }
    (* Keywords *)
    | "if"                  { IF }
    | "else"                { ELSE }
    | "pass"                { PASS }
    | "while"               { WHILE }
    | "function"            { FUNCTION }
    | "return"              { RETURN }
    | "print"               { PRINT }
    | "read"                { READ }
    (* | "convert"             { CONVERT } *)
    (* | "len"                 { LEN } *)
    (* Operators *)
    | "+"                   { PLUS }
    | "-"                   { MINUS }
    | "*"                   { TIMES }
    | "/"                   { DIVIDE }
    | "%"                   { MOD }
    | "<"                   { LT }
    | "<="                  { LEQ }
    | ">"                   { GT }
    | ">="                  { GEQ }
    | "=="                  { EQ }
    | "!="                  { NEQ }
    | "not"                 { NOT }
    | "and"                 { AND }
    | "or"                  { OR }
    | "="                   { ASSIGN }
    (* Symbols *)
    | ";"                   { SEMICOLON }
    | "("                   { LPAREN }
    | ")"                   { RPAREN }
    | "["                   { LBRACKET }
    | "]"                   { RBRACKET }
    | "{"                   { LBRACE }
    | "}"                   { RBRACE }
    | ":"                   { COLON }
    | ","                   { COMMA }
    (* Types *)
    | "int"                 { TINT }
    | "char"                { TCHAR }
    | "bool"                { TBOOL }
    | "intlist"             { TINTLIST }
    | "charlist"            { TCHARLIST }
    | "string"              { TCHARLIST }
    | "boollist"            { TBOOLLIST }
    | "none"                { TNONE }
    (* Values *)
    | "true"                { TRUE }
    | "false"               { FALSE }
    | '"'                   { Buffer.clear string_buf; 
                              lex_string lexbuf; 
                              STR (Buffer.contents string_buf) }
    | ch                    { CHAR (String.sub (Lexing.lexeme lexbuf) 1 1).[0] }
    | inv_ch                { Syntax.syntax_error ("Improperly defined character literal: " 
                              ^ (Lexing.lexeme lexbuf)) (get_pos lexbuf); exit 0 }
    | var                   { VAR (Lexing.lexeme lexbuf) }
    | num                   { NUM (int_of_string (Lexing.lexeme lexbuf)) }
    (* EOF *)
    | eof                   { EOF }
    | _                     { Syntax.syntax_error ("Illegal character(s): " 
                              ^ (Lexing.lexeme lexbuf)) (get_pos lexbuf); exit 0 } 

and lex_string  = 
    parse
    | '"'                   { () }
    
    | '\\' (backslash_escapes as c)
                            { Buffer.add_char string_buf (char_for_backslash c);
                            lex_string lexbuf }
    | _ as c                { Buffer.add_char string_buf c; lex_string lexbuf }
    | eof                   { Syntax.syntax_error "Unterminated string at end of file." 
                              (get_pos lexbuf); exit 0 }
