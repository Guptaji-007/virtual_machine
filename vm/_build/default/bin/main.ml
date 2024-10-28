open Vm.Assembler_ast
open Vm.Vm_parser.Parser
open Vm.Encode

(* Function to read lines from a file *)
let read_lines filename =
  let ic = open_in filename in
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file -> close_in ic; List.rev acc
  in
  loop []

(* Function to write the assembly code to a file *)
let write_lines filename lines =
  let oc = open_out filename in
  List.iter (fun line -> output_string oc (line ^ "\n")) lines;
  close_out oc

(* Function to convert a list of Hack assembly instructions to a list of strings *)
let instruction_to_string = function
  | AInstr (ASymbol sym) -> "@" ^ sym
  | AInstr (AConstant n) -> "@" ^ string_of_int n
  | CInstr { dest; comp; jump; _ } ->
    let dest_str = match dest with Some d -> d ^ "=" | None -> "" in
    let jump_str = match jump with Some j -> ";" ^ j | None -> "" in
    dest_str ^ comp ^ jump_str

(* Function to process a single line of VM code and convert it to Hack assembly *)
let process_line line =
  match parse_line line with
  | command -> Encode.translate_vm_command command
  | exception Failure msg -> failwith ("Error processing line: " ^ line ^ ". " ^ msg)

(* Main function to process a VM file *)
let process_file input_filename output_filename =
  let lines = read_lines input_filename in
  let translated_lines = 
    lines
    |> List.filter (fun line -> line <> "") (* Remove empty lines *)
    |> List.map process_line               (* Parse and translate each line *)
    |> List.concat                         (* Flatten the list of instruction lists *)
    |> List.map instruction_to_string      (* Convert instructions to string representation *)
  in
  write_lines output_filename translated_lines

(* Entry point *)
let () =
  if Array.length Sys.argv <> 3 then
    Printf.eprintf "Usage: %s <input.vm> <output.asm>\n" Sys.argv.(0)
  else
    let input_filename = Sys.argv.(1) in
    let output_filename = Sys.argv.(2) in
    process_file input_filename output_filename
