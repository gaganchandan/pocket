%{
    open Syntax
    let string_to_charlist (s:string) = 
        let rec make_expr (lst : char list) = 
            match lst with
            | [] -> []
            | c :: cs -> (Char (c, Lexing.dummy_pos)) :: make_expr cs
        in s |> String.to_seq |> List.of_seq |> make_expr 
%}

%token IF ELSE PASS WHILE FUNCTION RETURN PRINT READ CONVERT LEN 
%token PLUS MINUS TIMES DIVIDE MOD
%token EQ NEQ LT GT LEQ GEQ
%token AND OR NOT
%token ASSIGN
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token COLON SEMICOLON COMMA
%token TINT TCHAR TBOOL TINTLIST TCHARLIST TBOOLLIST TNONE
%token TRUE FALSE
%token <string> STR 
%token <char> CHAR 
%token <string> VAR
%token <int> NUM
%token EOF

%left LBRACKET
%left AND OR
%left LT GT LEQ GEQ
%left EQ NEQ
%left PLUS MINUS 
%left TIMES DIVIDE MOD 
%right NOT

%start <Syntax.command> prog
%type <Syntax.command> command
%type <Syntax.typed_var> typed_var
%type <Syntax.var> var
%type <string * Syntax.expr list> func_call
%type <Syntax.expr list> elem
%type <Syntax.typed_var list> param

%%
prog:
    | c=command EOF { c }

command:
    | LPAREN c=command RPAREN { c }
    | v=var ASSIGN e=expr SEMICOLON { Assign (v, e, $startpos) }
    | PRINT LPAREN e=expr RPAREN SEMICOLON { Print (e, $startpos) }
    | READ LPAREN v=var RPAREN SEMICOLON { Read (v, $startpos) }
    | CONVERT LPAREN v=var COMMA t=ptype RPAREN SEMICOLON { Convert (v, t, $startpos) }
    | LEN LPAREN e=expr RPAREN SEMICOLON { Len (e, $startpos) }
    | IF LPAREN e=expr RPAREN LBRACE c1=command RBRACE ELSE LBRACE c2=command RBRACE { If (e, c1, c2, $startpos) }
    | WHILE LPAREN e=expr RPAREN LBRACE c=command RBRACE { While (e, c, $startpos) }
    | fd=func_def  { fd }
    | fc=func_call SEMICOLON {FuncCallCmd (fc, $startpos) }
    | PASS SEMICOLON { Pass ($startpos) }
    | c1=command c2=command { Seq (c1, c2) }

typed_var:
    | v=VAR COLON t=ptype { (v, t, $startpos) }

var:
    | v=VAR { Var (v, $startpos) }
    | v=VAR COLON t=ptype { TypedVar (v, t, $startpos) }

ptype:
    | TINT { TInt ($startpos)}
    | TCHAR { TChar ($startpos) }
    | TBOOL { TBool ($startpos) }
    | TINTLIST { TIntList ($startpos) }
    | TCHARLIST { TCharList ($startpos) }
    | TBOOLLIST { TBoolList ($startpos) }
    | TNONE { TNone ($startpos) }
    
expr:
    | v=VAR { Id (v, $startpos) }
    | i=NUM { Int (i, $startpos) }
    | c=CHAR { Char (c, $startpos) }
    | s=STR { List ((string_to_charlist s), $startpos) }
    | TRUE { Bool (true, $startpos) }
    | FALSE { Bool (false, $startpos) }
    | l=lst { l }
    | LPAREN e=expr RPAREN { e }
    | NOT e=expr { Unop (Not, e, $startpos) }
    | e1=expr PLUS e2=expr { Binop (Add, e1, e2, $startpos) }
    | e1=expr MINUS e2=expr { Binop (Sub, e1, e2, $startpos) }
    | e1=expr TIMES e2=expr { Binop (Mul, e1, e2, $startpos) }
    | e1=expr DIVIDE e2=expr { Binop (Div, e1, e2, $startpos) }
    | e1=expr MOD e2=expr { Binop (Mod, e1, e2, $startpos) }
    | e1=expr LT e2=expr { Binop (Lt, e1, e2, $startpos) }
    | e1=expr GT e2=expr { Binop (Gt, e1, e2, $startpos) }
    | e1=expr LEQ e2=expr { Binop (Le, e1, e2, $startpos) }
    | e1=expr GEQ e2=expr { Binop (Ge, e1, e2, $startpos) }
    | e1=expr EQ e2=expr { Binop (Eq, e1, e2, $startpos) }
    | e1=expr NEQ e2=expr { Binop (Neq, e1, e2, $startpos) }
    | e1=expr AND e2=expr { Binop (And, e1, e2, $startpos) }
    | e1=expr OR e2=expr { Binop (Or, e1, e2, $startpos) }
    | e1=expr LBRACKET e2=expr RBRACKET { Index (e1, e2, $startpos) }
    | fc=func_call { FuncCall (fc, $startpos) }

lst:
    | LBRACKET RBRACKET { List ([], $startpos) }
    | LBRACKET e=elem RBRACKET { List (e, $startpos) }

elem:
    | e=expr { [e] }
    | e=expr COMMA l=elem { e::l }

func_call:
    | v=VAR LPAREN e=elem RPAREN { (v, e) }
    | v=VAR LPAREN RPAREN { (v, []) }

param:
    | t=typed_var { [t] }
    | t=typed_var COMMA p=param { t::p }

func_def: 
    | FUNCTION v=VAR LPAREN p=param RPAREN COLON t=ptype LBRACE b=func_body RBRACE { FuncDef (v, p, t, b, $startpos) }
    | FUNCTION v=VAR LPAREN RPAREN COLON t=ptype LBRACE b=func_body RBRACE { FuncDef (v, [], t, b, $startpos) }

func_body:
    | c=command { NoRet c }
    | c=command RETURN e=expr SEMICOLON { Ret (c, e, $startpos) }
