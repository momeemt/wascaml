open Compiler.Tokenizer
open Compiler.Parser
open Compiler.Codegen
(* open Compiler.Inferer *)

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <source file>\n" Sys.argv.(0)
  else
    let source_file = Sys.argv.(1) in
    let wat_file = Filename.chop_extension source_file ^ ".wat" in
    let in_channel = open_in source_file in
    let source_code =
      let len = in_channel_length in_channel in
      really_input_string in_channel len
    in
    close_in in_channel;
    let tokens = tokenize source_code in
    let ast = parse tokens in
    (* let _ = tinf ast in *)
    let wat = codegen ast in
    let out_channel = open_out wat_file in
    output_string out_channel wat;
    close_out out_channel;
    Printf.printf "Generated WAT file: %s\n" wat_file
