open Printf
open Syntax
open Types

let c_type_of_typ (typ : typ) : string =
  match typ with
  | TInt | TIntF | TIntList -> "int"
  | TChar | TCharF | TCharList -> "char"
  | TBool | TBoolF | TBoolList -> "bool"
  | TIntListF -> "int*"
  | TCharListF -> "char*"
  | TBoolListF -> "bool*"
  | _ ->
      print_endline "Precondition violated";
      exit 1

let c_type_of_type' (type' : type') : string =
  match type' with
  | TInt _ | TIntList _ -> "int"
  | TChar _ | TCharList _ -> "char"
  | TBool _ | TBoolList _ -> "bool"
  | _ ->
      print_endline "Precondition violated";
      exit 1

let rec gen_args args =
  match args with
  | [] -> ""
  | [ t ] -> c_type_of_type' t
  | t :: ts -> c_type_of_type' t ^ ", " ^ gen_args ts

let gen_fundecl = function
  | name, typ, args ->
      sprintf "%s %s(%s);\n" (c_type_of_type' typ) name (gen_args args)

let rec check_main fundecls =
  match fundecls with
  | [] ->
      print_endline "CompilationError: main function not found";
      exit 1
  | (name, _, _) :: rest -> if name = "main" then () else check_main rest

let rec gen_fundecls fundecls =
  match fundecls with [] -> "\n" | f :: fs -> gen_fundecl f ^ gen_fundecls fs

let gen_var_count : int ref = ref 0

let rec transpile' (ast : command) : string =
  match ast with
  | Assign (var, expr, loc) -> transpile_assign var expr
  (* | VarDecl (var, loc) -> transpile_vardecl var *)
  | Print (expr, typ, loc) -> transpile_print expr typ
  | Read (var, typ, loc) -> transpile_read var typ
  (* | Convert (var, typ, loc) -> transpile_convert var typ *)
  | If (expr, cmd1, cmd2, loc) -> transpile_if expr cmd1 cmd2
  | While (expr, cmd, loc) -> transpile_while expr cmd
  | FuncDef (name, args, typ, body, loc) -> transpile_funcdef name args typ body
  | FuncCallCmd ((name, exprs), loc) -> transpile_funccallcmd name exprs
  | Pass loc -> ";\n"
  | Seq (cmd1, cmd2) -> transpile' cmd1 ^ transpile' cmd2

and transpile_assign var expr =
  match var with
  | Var (s, loc) -> sprintf "%s = %s;\n" s (transpile_expr expr)
  | TypedVar (s, typ, loc) -> transpile_typed_assign s typ expr
  | AssignIndex (e1, e2, loc) ->
      sprintf "%s[%s] = %s;\n" (transpile_expr e1) (transpile_expr e2)
        (transpile_expr expr)

and transpile_typed_assign s typ expr =
  match typ with
  | TInt _ | TChar _ | TBool _ ->
      sprintf "%s %s = %s;\n" (c_type_of_type' typ) s (transpile_expr expr)
  | TIntList _ | TCharList _ | TBoolList _ ->
      sprintf "%s %s[] = %s;\n" (c_type_of_type' typ) s (transpile_expr expr)
  | TNone _ ->
      print_endline "Precondition violated";
      exit 1

(* and transpile_vardecl var = *)
(*   match var with s, typ, loc -> sprintf "%s %s;\n" (transpile_typ typ) s *)

and transpile_print expr typ =
  match typ with
  | TInt -> sprintf "printf(\"%%d\", %s);\n" (transpile_expr expr)
  | TChar -> sprintf "printf(\"%%c\", %s);\n" (transpile_expr expr)
  | TBool ->
      sprintf "printf(\"%%s\", %s ? \"true \" : \" false \");\n"
        (transpile_expr expr)
  | TCharList -> sprintf "printf(\"%%s\", %s);\n" (transpile_expr expr)
  | _ ->
      print_endline "Precondition violated";
      exit 1

and transpile_read var typ =
  match var with
  | Var (s, loc) | TypedVar (s, _, loc) -> (
      match typ with
      | TInt -> sprintf "scanf(\"%%d\", &%s);\n" s
      | TChar -> sprintf "scanf(\"%%c\", &%s);\n" s
      | TCharList -> sprintf "scanf(\"%%s\", %s);\n" s
      | _ ->
          print_endline "Precondition violated";
          exit 1)

and transpile_if expr cmd1 cmd2 =
  sprintf "if(%s){\n%s}else{\n%s}\n" (transpile_expr expr) (transpile' cmd1)
    (transpile' cmd2)

and transpile_while expr cmd =
  sprintf "while(%s){\n%s}\n" (transpile_expr expr) (transpile' cmd)

and transpile_funcdef name args typ body = 

let transpile = function
  | ast, fundecls ->
      check_main fundecls;
      "#include <stdio.h>\n#include <stdbool.h>\n\n" ^ gen_fundecls fundecls
      ^ transpile' ast
