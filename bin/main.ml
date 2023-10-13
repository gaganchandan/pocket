open Core
open Pocket

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

let usage_msg = "pocket [options] file\nOptions:\n"
let input_file = ref ""
let executable = ref ""
let emit_c = ref false
let c_file = ref "a.c"
let anon_fun filename = input_file := filename

let speclist =
  [
    ("-emit-c", Arg.Set emit_c, "Emit C code");
    ("-o", Arg.Set_string executable, "Executable file name");
  ]

let () = Arg.parse speclist anon_fun usage_msg

let main : unit =
  let c_code = transpile !input_file in
  if String.equal !executable "" then (
    c_file := "a.c";
    executable := "a.out")
  else c_file := !executable ^ ".c";
  Out_channel.write_all !c_file ~data:c_code;
  ignore (Sys.command ("gcc " ^ !c_file ^ " -o " ^ !executable));
  if not !emit_c then ignore (Sys.command ("rm " ^ !c_file))
  else
    let exists = Sys.file_exists "/usr/bin/clang-format" in
    match exists with
    | `No | `Unknown -> ()
    | `Yes -> ignore (Sys.command ("clang-format -i " ^ !c_file))
