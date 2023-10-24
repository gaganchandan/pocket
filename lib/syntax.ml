(*

********************************************************************************
GRAMMAR OF POCKET
********************************************************************************

prog := command EOF

command := (command) | command; command | var = expr; 
           | print(expr); | read(var); | len(lst); 
           | IF expr { command } ELSE { command} 
           | WHILE expr { command } | func_def | func_call; | pass;

typed_var := IDENTIFIER : type

type := int | intlist | char | charlist | bool | boollist | none

var := IDENTIFIER | IDENTIFIER : type

expr := IDENTIFIER | NUM | CHAR | TRUE | FALSE | lst | (expr) | expr.[expr] | unop expr 
        | expr binop expr | func_call 

lst := [] | [elem]

elem := expr | expr, elem  

param := typed_var | typed_var, param

func_def := IDENTIFIER (param) : type { func_body } 
            | IDENTIFIER () : type { func_body }

func_body := command RETURN expr | command

func_call := IDENTIFIER (elem) | IDENTIFIER ()

unop := NOT 

binop := + | - | * | / | % | == | != | < | <= | > | >= | AND | OR

*)

open Types

type loc = Lexing.position

type command =
  | Seq of command * command
  | Assign of lvalue * expr * loc
  (* | VarDecl of typed_var * loc *)
  | Print of expr * typ * loc
  | Read of var * typ * loc
  (* | Convert of var * type' * loc *)
  | If of expr * command * command * loc
  | While of expr * command * loc
  | FuncDef of func_def
  | FuncCallCmd of (string * expr list) * loc
  | Pass of loc

and lvalue =
  | Var of string * loc
  | TypedVar of string * type' * loc
  | AssignIndex of expr * expr * loc

and var = Var of string * loc | TypedVar of string * type' * loc
and typed_var = string * type' * loc

and type' =
  | TInt of loc
  | TChar of loc
  | TBool of loc
  | TIntList of loc
  | TCharList of loc
  | TBoolList of loc
  | TNone of loc

and func_def = string * typed_var list * type' * func_body * loc
and func_body = Ret of command option * expr * loc | NoRet of command

and expr =
  | Id of string * loc
  | Int of int * loc
  | Char of char * loc
  | Bool of bool * loc
  | List of expr list * string option * int * loc
  | Index of expr * expr * loc
  (* | Len of expr * loc *)
  | Unop of unop * expr * loc
  | Binop of binop * expr * expr * loc
  | FuncCall of (string * expr list) * loc

and unop = Not

and binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or

let syntax_error (msg : string) (pos : int * int) : unit =
  let lnum, cnum = pos in
  print_endline
    ("SyntaxError on line " ^ string_of_int lnum ^ ":" ^ string_of_int cnum
   ^ ": " ^ msg);
  exit 0
