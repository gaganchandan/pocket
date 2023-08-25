open Syntax
open Lexing

(* let gen_pos pos = (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1) *)

let type_error (msg : string) pos : unit =
  print_endline
    ("TypeError on line " ^ string_of_int pos.pos_lnum ^ ": " ^ msg ^ "\n")

type typ =
  | TInt
  | TChar
  | TBool
  | TIntList
  | TCharList
  | TBoolList
  | TUnknownList
  | TNone
  | TUnknown
  | TIntF
  | TCharF
  | TBoolF
  | TIntListF
  | TCharListF
  | TBoolListF
  | TUnknownListF
  | TNoneF
  | TUnknownF
  | TError
  | TUndef

let string_of_typ = function
  | TInt -> "int"
  | TChar -> "char"
  | TBool -> "bool"
  | TIntList -> "intlist"
  | TCharList -> "charlist"
  | TBoolList -> "boollist"
  | TNone -> "none"
  | TUnknown -> "unknown"
  | TUnknownList -> "unknownlist"
  | TIntF -> "int"
  | TCharF -> "char"
  | TBoolF -> "bool"
  | TIntListF -> "intlist"
  | TCharListF -> "charlist"
  | TBoolListF -> "boollist"
  | TNoneF -> "none"
  | TUnknownF -> "unknown"
  | TUnknownListF -> "unknown"
  | TError -> "error"
  | TUndef -> "undefined"

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
  | TUnknownList -> TUnknownListF
  | _ -> failwith "Precondition failed"

let list_of_typ = function
  | TInt -> TIntList
  | TChar -> TCharList
  | TBool -> TBoolList
  | TUnknown -> TUnknownList
  | _ -> failwith "Precondition failed"

let typ_of_list = function
  | TIntList -> TInt
  | TBoolList -> TBool
  | TCharList -> TChar
  | _ -> failwith "Precondition failed"

let is_fun = function
  | TIntF | TCharF | TBoolF | TIntListF | TCharListF | TBoolListF | TNoneF
  | TUnknownF ->
      true
  | _ -> false

let is_list = function
  | TIntList | TCharList | TBoolList | TUnknownList -> true
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

(* let make_empty_list = function *)
(*   | TInt -> IntList [] *)
(*   | TChar -> CharList [] *)
(*   | TBool -> BoolList [] *)
(*   | _ -> failwith "Precondition failed" *)

let get_var_name = function
  | Var (name, _) -> name
  | TypedVar (name, _, _) -> name

type entry = {
  name : string;
  typ : typ;
  value : value option;
  args : entry list option;
  body : func_body option;
}

let print_entry (e : entry) =
  match e with
  | { name; typ; value; args; body } ->
      print_endline ("name: " ^ name);
      print_endline ("typ: " ^ string_of_typ typ)

let unravel_args entry =
  match entry with
  | { args } -> (
      match args with
      | None | Some [] -> []
      | Some lst -> List.map (fun { typ } -> typ) lst)

let rec lookup (n : string) (env : entry list ref) loc =
  match env with
  | { contents = [] } ->
      type_error ("Undefined symbol " ^ n) loc;
      { name = n; typ = TUndef; value = None; args = None; body = None }
  | { contents = e :: es } -> (
      match e with
      | { name } when name = n ->
          let () =
            if e.typ = TUndef then type_error ("Undefined symbol " ^ n) loc
            else ()
          in
          e
      | _ -> lookup n { contents = es } loc)

let rec add_typed_var (n : string) (t : typ) (env : entry list ref) loc =
  let rec aux n t env acc =
    match env with
    | [] -> { name = n; typ = t; value = None; args = None; body = None } :: acc
    | x :: xs ->
        if x.name = n then (
          type_error
            ("Variable " ^ n ^ " already defined with type " ^ string_of_typ t)
            loc;
          acc @ env)
        else aux n t xs (x :: acc)
  in
  env := aux n t !env []

(* match env with *)
(* | { contents = es } -> ( *)
(*     match es with *)
(*     | [] -> *)
(*         env := *)
(*           [ { name = n; typ = t; value = None; args = None; body = None } ] *)
(*     | x :: xs -> *)
(*         if x.name = n then *)
(*           type_error *)
(*             ("Variable " ^ n ^ " already defined with type " ^ string_of_typ t) *)
(*             loc *)
(*         else add_typed_var n t { contents = xs } loc) *)

let rec update_env (e : entry) (env : entry list ref) =
  let rec aux e env acc =
    match env with
    | [] -> e :: acc
    | x :: xs -> if x.name = e.name then (e :: acc) @ xs else aux e xs (x :: acc)
  in
  env := aux e !env []

let (env : entry list ref) = ref []

let rec typecheck_cmd (env : entry list ref) cmd =
  match cmd with
  | Assign (var, expr, loc) -> typecheck_assign var expr loc env
  | VarDecl (var, loc) -> typecheck_vardecl var loc env
  | Print (expr, loc) -> typecheck_print expr loc env
  | Read (var, loc) -> typecheck_read var loc env
  | Convert (var, typ, loc) -> typecheck_convert var typ loc env
  | If (expr, cmd1, cmd2, loc) -> typecheck_if expr cmd1 cmd2 loc env
  | While (expr, cmd, loc) -> typecheck_while expr cmd loc env
  | FuncDef (name, args, typ, body, loc) ->
      typecheck_funcdef name args typ body loc env
  | FuncCallCmd ((name, exprlist), loc) ->
      typecheck_funccallcmd name exprlist loc env
  | Pass loc -> ()
  | Seq (cmd1, cmd2) ->
      typecheck_cmd env cmd1;
      typecheck_cmd env cmd2

and typecheck_assign lvalue expr loc env =
  let t1 = typecheck_expr expr env in
  let func (lvalue : lvalue) =
    match lvalue with
    | Var (name, loc) -> typecheck_var (Var (name, loc)) env
    | TypedVar (name, typ, loc) -> typecheck_var (TypedVar (name, typ, loc)) env
    | AssignIndex (e1, e2, loc) ->
        let t1 = typecheck_expr e1 env in
        let t2 = typecheck_expr e2 env in
        if is_list t1 then
          if t2 = TInt then typ_of_list t1
          else
            let () = type_error "Index must be an integer" loc in
            TError
        else
          let () = type_error "Cannot index objects that are not lists" loc in
          TError
  in
  let t2 = func lvalue in
  if t1 = t2 then
    match lvalue with
    | Var (name, loc) ->
        update_env
          { name; typ = t2; value = None; args = None; body = None }
          env
    | TypedVar (name, typ, loc) ->
        update_env
          { name; typ = t2; value = None; args = None; body = None }
          env
    | _ -> ()
  else
    type_error
      ("Expected value of type " ^ string_of_typ t2 ^ ", got value of type "
     ^ string_of_typ t1)
      loc

and typecheck_vardecl var loc env =
  match var with
  | name, type', loc ->
      let t1 = typ_of_type' type' in
      add_typed_var name t1 env loc

and typecheck_print expr loc env =
  let _ = typecheck_expr expr env in
  ()

and typecheck_read var loc env =
  let t1 = typecheck_var var env in
  if t1 = TCharList then
    update_env
      {
        name = get_var_name var;
        typ = t1;
        value = None;
        args = None;
        body = None;
      }
      env
  else
    type_error
      ("read() expects variable of type charlist, got variable of type "
     ^ string_of_typ t1)
      loc

and typecheck_convert var typ loc env =
  let t1 = typecheck_var var env in
  let t2 = typ_of_type' typ in
  match (t1, t2) with
  | TCharList, TInt | TChar, TInt | TInt, TCharList | TInt, TChar ->
      update_env
        {
          name = get_var_name var;
          typ = t2;
          value = None;
          args = None;
          body = None;
        }
        env
  | t1, t2 ->
      type_error
        ("Cannot convert from " ^ string_of_typ t1 ^ " to " ^ string_of_typ t2)
        loc

and typecheck_len expr loc env =
  let t1 = typecheck_expr expr env in
  match t1 with
  | TIntList | TCharList | TBoolList -> ()
  | _ ->
      type_error
        ("len() expects expression of one of the list types, got expression of "
       ^ string_of_typ t1)
        loc

and typecheck_if expr cmd1 cmd2 loc env =
  let t1 = typecheck_expr expr env in
  if t1 = TBool then (
    typecheck_cmd env cmd1;
    typecheck_cmd env cmd1)
  else
    type_error
      ("Guard for if statement must be of type bool, got " ^ string_of_typ t1)
      loc

and typecheck_while expr cmd loc env =
  let t1 = typecheck_expr expr env in
  if t1 = TBool then typecheck_cmd env cmd
  else
    type_error
      ("Guard for if statement must be of type bool, got " ^ string_of_typ t1)
      loc

and typecheck_funcdef name args typ body loc env =
  let t1 = typ_of_type' typ in
  let args, t2 = typecheck_funcbody args body loc env in
  if t1 = t2 then
    env :=
      {
        name;
        typ = typ_of_type' typ;
        value = None;
        args = Some args;
        body = Some body;
      }
      :: !env
  else
    type_error
      ("Declared function type is " ^ string_of_typ t1
     ^ ", function returns value of type " ^ string_of_typ t2)
      loc

and typecheck_funccallcmd name exprlist loc env =
  let args = List.map (fun x -> typecheck_expr x env) exprlist in
  let func = lookup name env loc in
  let params = unravel_args func in
  let rec check params args =
    match (params, args) with
    | [], [] -> ()
    | [], arg :: _ -> type_error "Extra arguments to function" loc
    | param :: _, [] -> type_error "Not enough arguments to function" loc
    | param :: params, arg :: args ->
        if param = arg then check params args
        else type_error "Argument type mismatch in function call" loc
  in
  check params args

and typecheck_funcbody args body loc env =
  let (env : entry list ref) = ref [] in
  let rec env_of_args args =
    match args with
    | [] -> ()
    | x :: xs ->
        env :=
          (fun (name, typ, loc) ->
            {
              name;
              typ = typ_of_type' typ;
              value = None;
              args = None;
              body = None;
            })
            x
          :: !env;
        env_of_args xs
  in
  env_of_args args;

  let entry_of_arg (name, typ, loc) =
    { name; typ = typ_of_type' typ; value = None; args = None; body = None }
  in
  let check = function
    | NoRet cmd ->
        typecheck_cmd env cmd;
        (List.map entry_of_arg args, TNone)
    | Ret (cmd, expr, loc) ->
        let () =
          match cmd with None -> () | Some cmd -> typecheck_cmd env cmd
        in

        (List.map entry_of_arg args, typecheck_expr expr env)
  in
  check body

and typecheck_var var env =
  match var with
  | Var (var, loc) ->
      let e = lookup var env loc in
      e.typ
  | TypedVar (var, typ, loc) ->
      add_typed_var var (typ_of_type' typ) env loc;
      typ_of_type' typ

and typecheck_expr expr env =
  match expr with
  | Id (n, loc) ->
      let e = lookup n env loc in
      e.typ
  | Int (i, loc) -> TInt
  | Char (c, loc) -> TChar
  | Bool (b, loc) -> TBool
  | List (exprs, loc) -> (
      match exprs with
      | [] -> TUnknownList
      | x :: xs ->
          let t = typecheck_expr x env in
          if List.for_all (fun e -> typecheck_expr e env = t) xs then
            list_of_typ t
          else (
            type_error "List elements must be of the same type" loc;
            TError))
  | Index (lst, expr, loc) ->
      let t1 = typecheck_expr lst env in
      let t2 = typecheck_expr expr env in
      if is_list t1 then
        if t2 = TInt then typ_of_list t1
        else
          let () = type_error "Index must be an integer" loc in
          TError
      else
        let () = type_error "Cannot index objects that are not lists" loc in
        TError
  | Len (expr, loc) ->
      let t1 = typecheck_expr expr env in
      if is_list t1 then TInt
      else
        let () =
          type_error "Cannot call len() on objects that are not lists" loc
        in
        TError
  | Unop (unop, expr, loc) -> (
      match unop with
      | Not ->
          let t1 = typecheck_expr expr env in
          let () =
            if t1 = TBool then ()
            else
              type_error
                ("not expects variable of type bool, got variable of type "
               ^ string_of_typ t1)
                loc
          in
          t1)
  | Binop (binop, expr1, expr2, loc) -> (
      match binop with
      | Add -> (
          let t1 = typecheck_expr expr1 env in
          let t2 = typecheck_expr expr2 env in
          match (t1, t2) with
          | TInt, TInt -> TInt
          | TIntList, TIntList -> TIntList
          | TCharList, TCharList -> TCharList
          | TBoolList, TBoolList -> TBoolList
          | _ ->
              let () =
                type_error
                  ("Incompatible types " ^ string_of_typ t1 ^ " and "
                 ^ string_of_typ t2 ^ " for addition")
                  loc
              in
              TError)
      | Sub -> (
          let t1 = typecheck_expr expr1 env in
          let t2 = typecheck_expr expr2 env in
          match (t1, t2) with
          | TInt, TInt -> TInt
          | _ ->
              let () =
                type_error
                  ("Incompatible types " ^ string_of_typ t1 ^ " and "
                 ^ string_of_typ t2 ^ " for subtraction")
                  loc
              in
              TError)
      | Mul -> (
          let t1 = typecheck_expr expr1 env in
          let t2 = typecheck_expr expr2 env in
          match (t1, t2) with
          | TInt, TInt -> TInt
          | _ ->
              let () =
                type_error
                  ("Incompatible types " ^ string_of_typ t1 ^ " and "
                 ^ string_of_typ t2 ^ " for multiplication")
                  loc
              in
              TError)
      | Div -> (
          let t1 = typecheck_expr expr1 env in
          let t2 = typecheck_expr expr2 env in
          match (t1, t2) with
          | TInt, TInt -> TInt
          | _ ->
              let () =
                type_error
                  ("Incompatible types " ^ string_of_typ t1 ^ " and "
                 ^ string_of_typ t2 ^ " for division")
                  loc
              in
              TError)
      | Mod -> (
          let t1 = typecheck_expr expr1 env in
          let t2 = typecheck_expr expr2 env in
          match (t1, t2) with
          | TInt, TInt -> TInt
          | _ ->
              let () =
                type_error
                  ("Incompatible types " ^ string_of_typ t1 ^ " and "
                 ^ string_of_typ t2 ^ " for modulus")
                  loc
              in
              TError)
      | Eq | Neq -> (
          let t1 = typecheck_expr expr1 env in
          let t2 = typecheck_expr expr2 env in
          match (t1, t2) with
          | TInt, TInt -> TBool
          | TChar, TChar -> TBool
          | TBool, TBool -> TBool
          | TIntList, TIntList -> TBool
          | TCharList, TCharList -> TBool
          | TBoolList, TBoolList -> TBool
          | _ ->
              let () =
                type_error
                  ("Incompatible types " ^ string_of_typ t1 ^ " and "
                 ^ string_of_typ t2 ^ " for equality")
                  loc
              in
              TError)
      | Lt | Le | Gt | Ge -> (
          let t1 = typecheck_expr expr1 env in
          let t2 = typecheck_expr expr2 env in
          match (t1, t2) with
          | TInt, TInt -> TBool
          | _ ->
              let () =
                type_error
                  ("Incompatible types " ^ string_of_typ t1 ^ " and "
                 ^ string_of_typ t2 ^ " for comparison")
                  loc
              in
              TError)
      | And | Or -> (
          let t1 = typecheck_expr expr1 env in
          let t2 = typecheck_expr expr2 env in
          match (t1, t2) with
          | TBool, TBool -> TBool
          | _ ->
              let () =
                type_error
                  ("Incompatible types " ^ string_of_typ t1 ^ " and "
                 ^ string_of_typ t2 ^ " for boolean operation")
                  loc
              in
              TError))
  | FuncCall ((name, exprlist), loc) ->
      let () = typecheck_funccallcmd name exprlist loc env in
      let func = lookup name env loc in
      func.typ

let typecheck cmd = typecheck_cmd env cmd
