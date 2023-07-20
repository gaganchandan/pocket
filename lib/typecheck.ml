open Syntax
open Lexing

let gen_pos pos = (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)

let type_error (msg : string) pos : unit =
  let lnum, cnum = gen_pos pos in
  print_endline
    ("TypeError: " ^ msg ^ " at " ^ string_of_int lnum ^ ":"
   ^ string_of_int cnum);
  exit 0

type typ =
  | TInt
  | TChar
  | TBool
  | TIntList
  | TCharList
  | TBoolList
  | TNone
  | TUnknown
  | TIntF
  | TCharF
  | TBoolF
  | TIntListF
  | TCharListF
  | TBoolListF
  | TNoneF
  | TUnknownF

let typ_of_type' (typ : type') : typ =
  match typ with
  | TInt _ -> TInt
  | TChar _ -> TChar
  | TBool _ -> TBool
  | TIntList _ -> TIntList
  | TCharList _ -> TCharList
  | TBoolList _ -> TBoolList
  | TNone _ -> TNone

let funtyp_of_typ = function
  | TInt -> TIntF
  | TChar -> TCharF
  | TBool -> TBoolF
  | TIntList -> TIntListF
  | TCharList -> TCharListF
  | TBoolList -> TBoolListF
  | TNone -> TNoneF
  | TUnknown -> TUnknownF
  | typ -> typ

let string_of_typ = function
  | TInt -> "int"
  | TChar -> "char"
  | TBool -> "bool"
  | TIntList -> "intlist"
  | TCharList -> "charlist"
  | TBoolList -> "boollist"
  | TNone -> "none"
  | TUnknown -> "unknown"
  | TIntF -> "int"
  | TCharF -> "char"
  | TBoolF -> "bool"
  | TIntListF -> "intlist"
  | TCharListF -> "charlist"
  | TBoolListF -> "boollist"
  | TNoneF -> "none"
  | TUnknownF -> "unknown"

let is_fun = function
  | TIntF | TCharF | TBoolF | TIntListF | TCharListF | TBoolListF | TNoneF
  | TUnknownF ->
      true
  | _ -> false

type value =
  | Int of int
  | Char of char
  | Bool of bool
  | IntList of int list
  | CharList of char list
  | BoolList of bool list
  | None
  | Unknown

type entry = {
  name : string;
  typ : typ;
  value : value option;
  args : entry list option;
  body : func_body option;
}

let rec lookup n env loc =
  match env with
  | [] ->
      type_error ("Undefined symbol " ^ n) loc;
      exit 0
  | e :: es -> (
      match e with { name } when name = n -> e | _ -> lookup n es loc)

let env = ref []

let rec typecheck_cmd cmd =
  match cmd with
  | Assign (var, expr, loc) -> typecheck_assign var expr loc
  | Print (expr, loc) -> typecheck_print expr loc
  | Read (var, loc) -> typecheck_read var loc
  | Convert (var, typ, loc) -> typecheck_convert var typ loc
  | Len (expr, loc) -> typecheck_len expr loc
  | If (expr, cmd1, cmd2, loc) -> typecheck_if expr cmd1 cmd2 loc
  | While (expr, cmd, loc) -> typecheck_while expr cmd loc
  | FuncDef (name, args, typ, body, loc) ->
      typecheck_funcdef name args typ body loc
  | FuncCallCmd ((name, exprlist), loc) ->
      typecheck_funccallcmd name exprlist loc
  | Pass loc -> ()
  | Seq (cmd1, cmd2) ->
      typecheck_cmd cmd1;
      typecheck_cmd cmd2

and typecheck_assign var expr loc =
  let t1 = typecheck_expr expr in
  let t2 = typecheck_var var in
  if t1 = t2 then ()
  else
    type_error
      ("Expected value of type " ^ string_of_typ t2 ^ ", got value of type "
     ^ string_of_typ t1)
      loc

and typecheck_print expr loc = typecheck_expr expr

and typecheck_read var loc =
  let t1 = typecheck_var var in
  if t1 = TCharList then ()
  else
    type_error
      ("read() expects variable of type charlist, got variable of type "
     ^ string_of_typ t1)
      loc

and typecheck_convert var typ loc =
  let (t1 : typ) = typecheck_var var in
  let (t2 : typ) = typ_of_type' typ in
  match (t1, t2) with
  | TCharList, TInt | TChar, TInt | TInt, TCharList | TInt, TChar -> ()
  | t1, typ ->
      type_error
        ("Cannot convert from " ^ string_of_typ t1 ^ " to " ^ string_of_typ t2)
        loc

and typecheck_len expr loc =
  let t1 = typecheck_expr expr in
  match t1 with
  | TIntList | TCharList | TBoolList -> ()
  | _ ->
      type_error
        ("len() expects expression of one of the list types, got expression of "
       ^ string_of_typ t1)
        loc

and typecheck_if expr cmd1 cmd2 loc =
  let t1 = typecheck_expr expr loc env in
  if t1 = TBool then (
    typecheck_cmd cmd1;
    typecheck_cmd cmd2)
  else
    type_error
      ("Guard for if statement must be of type bool, got " ^ string_of_typ t1)
      loc

and typecheck_while expr cmd loc =
  let t1 = typecheck_expr expr loc env in
  if t1 = TBool then typecheck_cmd cmd
  else
    type_error
      ("Guard for if statement must be of type bool, got " ^ string_of_typ t1)
      loc

and typecheck_funcdef name args typ body loc =
  let args, body = typecheck_funcbody args body loc in
  env :=
    [
      {
        name;
        typ = typ_of_type' typ;
        value = None;
        args = Some args;
        body = Some body;
      };
    ]
    :: !env

and typecheck_funccallcmd name exprlist loc =
  let args = List.map get_expr_type exprlist in
  let func = lookup name env loc in
  let params = func.args in
  let param_types = List.map (fun (name, typ, loc) -> (typ, loc)) params in
  match (param_types, args) with
  | [], [] -> ()
  | [], (arg, loc) :: _ -> type_error "Extra arguments to function" loc
