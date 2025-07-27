(* main.ml *)

open Tokens
open Lexer
open Parser
open Codegen

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: toyc <source.toy>";
    exit 1
  end;

  let filename = Sys.argv.(1) in
  let source = really_input_string (open_in filename) (in_channel_length (open_in filename)) in

  (* 1) Lexical analysis *)
  let tokens = tokenize source in

  (* 2) Parsing *)
  let ast = parse_program tokens in

  (* 3) Code generation *)
  let asm = generate ast in

  let oc = open_out (Filename.chop_extension filename ^ ".asm") in
  output_string oc asm;
  close_out oc;

  Printf.printf "Generated %s.asm\n" (Filename.chop_extension filename)
