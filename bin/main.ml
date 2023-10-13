open Pocket
open Core

let parse (src : string) : Pocket.Syntax.command =
  let lexbuf = Lexing.from_string src in
  let ast = Pocket.Parser.prog Pocket.Lexer.lex lexbuf in
  ast

let interpret (file : string) : unit =
  file |> In_channel.read_all |> parse |> Pocket.Typecheck.typecheck |> fun _ ->
  ()

let transpile (file : string) =
  file |> In_channel.read_all |> parse |> Pocket.Typecheck.typecheck
  |> Pocket.Transpile.transpile

let main : unit =
  let args = Sys.get_argv () in
  let len = Array.length args in
  match len with
  | 2 ->
      let file = args.(1) in
      (* print_endline (transpile file) *)
      let c_code = transpile file in
      let file = "/tmp/pocket.c" in
      Out_channel.write_all file ~data:c_code;
      ignore (Sys.command ("gcc " ^ file))
  | _ ->
      print_endline "Usage: pocket <source>";
      exit 0
