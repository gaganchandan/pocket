open Printf
open Syntax
open Types

let c_type_of_typ (typ : typ) : string =
  match typ with
  | TInt | TIntF -> "int"
  | TChar | TCharF -> "char"
  | TBool | TBoolF -> "bool"
  | TIntList | TIntListF -> "int*"
  | TCharList | TCharListF -> "char*"
  | TBoolList | TBoolListF -> "bool*"
  | TNone | TNoneF -> "void"
  | typ ->
      print_endline ("c_type_of_typ " ^ string_of_typ typ);
      exit 1

let c_type_of_type' (type' : type') : string =
  match type' with
  | TInt _ -> "int"
  | TChar _ -> "char"
  | TBool _ -> "bool"
  | TIntList _ -> "int*"
  | TCharList _ -> "char*"
  | TBoolList _ -> "bool*"
  | TNone _ -> "void"

let assign_type (type' : type') : string =
  match type' with
  | TInt _ | TIntList _ -> "int"
  | TChar _ | TCharList _ -> "char"
  | TBool _ | TBoolList _ -> "bool"
  | _ ->
      print_endline "assign_type";
      exit 1

let rec gen_dec_args args =
  match args with
  | [] -> ""
  | [ t ] -> c_type_of_typ t
  | t :: ts -> c_type_of_typ t ^ ", " ^ gen_dec_args ts

let rec gen_def_args params =
  match params with
  | [] -> ""
  | [ (name, type', _) ] -> sprintf "%s %s" (c_type_of_type' type') name
  | (name, type', _) :: rest ->
      sprintf "%s %s, %s" (c_type_of_type' type') name (gen_def_args rest)

let gen_fundecl = function
  | name, typ, args ->
      sprintf "%s %s(%s);\n" (c_type_of_typ typ) name (gen_dec_args args)

let rec check_main fundecls =
  match fundecls with
  | [] ->
      print_endline "CompilationError: main function not found";
      exit 1
  | (name, _, _) :: rest -> if name = "main" then () else check_main rest

let rec gen_fundecls fundecls =
  match fundecls with [] -> "\n" | f :: fs -> gen_fundecl f ^ gen_fundecls fs

let list_count : int ref = ref (-1)

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
      sprintf "%s %s = %s;\n" (assign_type typ) s (transpile_expr expr)
  | TIntList _ | TCharList _ | TBoolList _ ->
      sprintf "%s %s[] = %s;\n" (assign_type typ) s (transpile_expr expr)
  | TNone _ ->
      print_endline "TNone transpile_typed_assign";
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
      print_endline ("transpile_print " ^ string_of_typ typ);
      exit 1

and transpile_read var typ =
  let read s typ =
    match typ with
    | TInt -> sprintf "scanf(\"%%d\", &%s);\n" s
    | TChar -> sprintf "scanf(\"%%c\", &%s);\n" s
    | TCharList -> sprintf "scanf(\"%%s\", %s);\n" s
    | _ ->
        print_endline "Precondition violated";
        exit 1
  in
  match var with
  | Var (s, loc) -> read s typ
  | TypedVar (s, type', loc) -> gen_var_decl s type' ^ read s typ

and gen_var_decl s type' =
  match type' with
  | TInt _ -> sprintf "int %s;\n" s
  | TChar _ -> sprintf "char %s;\n" s
  | TCharList _ -> sprintf "char %s[256];\n" s
  | _ -> "Precondition violated"

and transpile_if expr cmd1 cmd2 =
  sprintf "if(%s){\n%s}else{\n%s}\n" (transpile_expr expr) (transpile' cmd1)
    (transpile' cmd2)

and transpile_while expr cmd =
  sprintf "while(%s){\n%s}\n" (transpile_expr expr) (transpile' cmd)

and transpile_funcdef name params type' body =
  sprintf "%s %s(%s){\n%s\n}" (c_type_of_type' type') name (gen_def_args params)
    (transpile_body body)

and transpile_body body =
  match body with
  | Ret (Some cmd, expr, loc) ->
      transpile' cmd ^ sprintf "return %s;\n" (transpile_expr expr)
  | Ret (None, expr, loc) -> sprintf "return %s;\n" (transpile_expr expr)
  | NoRet cmd -> transpile' cmd

and transpile_funccallcmd name exprs =
  sprintf "%s(%s);\n" name (gen_params exprs)

and gen_params params =
  match params with
  | [] -> ""
  | [ e ] -> transpile_expr e
  | e :: es -> sprintf "%s, %s" (transpile_expr e) (gen_params es)

and transpile_expr = function
  | Id (s, loc) -> s
  | Int (i, loc) -> string_of_int i
  | Char (c, loc) -> sprintf "\'%c\'" c
  | Bool (b, loc) -> if b then "true" else "false"
  | List (exprs, s, loc) -> transpile_list exprs s
  | Index (e1, e2, loc) ->
      sprintf "%s[%s]" (transpile_expr e1) (transpile_expr e2)
  | Len (e, loc) ->
      sprintf "sizeof(%s) / sizeof(%s[0])" (transpile_expr e) (transpile_expr e)
  | Unop (Not, e, loc) -> sprintf "!%s" (transpile_expr e)
  | Binop (op, e1, e2, loc) -> transpile_binop op e1 e2
  | FuncCall ((name, exprs), loc) -> sprintf "%s(%s)" name (gen_params exprs)

and transpile_list exprs s =
  match s with
  | Some s -> sprintf "\"%s\"" s
  | None -> sprintf "{%s}" (gen_params exprs)

and transpile_binop op e1 e2 =
  match op with
  | Add -> sprintf "(%s) + (%s)" (transpile_expr e1) (transpile_expr e2)
  | Sub -> sprintf "(%s) - (%s)" (transpile_expr e1) (transpile_expr e2)
  | Mul -> sprintf "(%s) * (%s)" (transpile_expr e1) (transpile_expr e2)
  | Div -> sprintf "(%s) / (%s)" (transpile_expr e1) (transpile_expr e2)
  | Mod -> sprintf "(%s) %% (%s)" (transpile_expr e1) (transpile_expr e2)
  | Eq -> sprintf "(%s) == (%s)" (transpile_expr e1) (transpile_expr e2)
  | Neq -> sprintf "(%s) != (%s)" (transpile_expr e1) (transpile_expr e2)
  | Lt -> sprintf "(%s) < (%s)" (transpile_expr e1) (transpile_expr e2)
  | Le -> sprintf "(%s) <= (%s)" (transpile_expr e1) (transpile_expr e2)
  | Gt -> sprintf "(%s) > (%s)" (transpile_expr e1) (transpile_expr e2)
  | Ge -> sprintf "(%s) >= (%s)" (transpile_expr e1) (transpile_expr e2)
  | And -> sprintf "(%s) && (%s)" (transpile_expr e1) (transpile_expr e2)
  | Or -> sprintf "(%s) || (%s)" (transpile_expr e1) (transpile_expr e2)

let transpile = function
  | ast, fundecls ->
      check_main fundecls;
      "#include <stdio.h>\n#include <stdbool.h>\n\n" ^ gen_fundecls fundecls
      ^ transpile' ast
