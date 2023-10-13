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
      transpile file |> print_endline
  | _ ->
      print_endline "Usage: pocket <source>";
      exit 0
