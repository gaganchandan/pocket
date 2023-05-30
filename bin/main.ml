open Pocket
open Core

let parse (src : string) : Pocket.Syntax.command =
  let lexbuf = Lexing.from_string src in
  let ast = Pocket.Parser.prog Pocket.Lexer.lex lexbuf in
  ast

let run (file : string) : unit =
  file |> In_channel.read_all |> parse |> fun _ -> ()

let main : unit =
  let args = Sys.get_argv () in
  let len = Array.length args in
  match len with
  | 2 ->
      let file = args.(1) in
      run file
  | _ ->
      print_endline "Usage: pocket <source>";
      exit 0
