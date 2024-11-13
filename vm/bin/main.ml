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

(* Function to convert a Hack assembly instruction to string *)
let instruction_to_string = function
  | AInstr (ASymbol sym) -> "@" ^ sym
  | AInstr (AConstant n) -> "@" ^ string_of_int n
  | CInstr { dest; comp; jump; _ } ->
    let dest_str = match dest with Some d -> d ^ "=" | None -> "" in
    let jump_str = match jump with Some j -> ";" ^ j | None -> "" in
    dest_str ^ comp ^ jump_str
  | LInstr (LSymbol label) -> "(" ^ label ^ ")" 

(* Exit sequence for halting the program *)
let resolve_exit = [
  LInstr (LSymbol "EXIT");
  AInstr (ASymbol "EXIT");  (* Label for termination *)
  CInstr { dest = None; comp = "0"; jump = Some "JMP"; metadata = None }  (* Infinite loop to halt execution *)
]

(* Check if the commands contain a function Sys.init *)
let contains_sys_init commands =
  List.exists (function
    | Encode.FunctionCommand (Function ("Sys.init", _)) -> true
    | _ -> false
  ) commands


let translate_vm_commands_with_sys_init_and_exit commands =
  if contains_sys_init commands then
    let sys_init_setup = Encode.initialize_and_call_sys_init () in
    let translated_commands = List.concat_map Encode.translate_vm_command commands in
    sys_init_setup @ translated_commands @ resolve_exit
  else
    let translated_commands = List.concat_map Encode.translate_vm_command commands in
    translated_commands @ resolve_exit

(* Function to process all lines of a VM file *)
let process_vm_lines lines =
  lines
  |> List.filter (fun line -> line <> "" && not (String.starts_with ~prefix:"//" line)) (* Remove empty/comment lines *)
  |> List.map parse_line (* Parse each line into a VM command *)
  |> translate_vm_commands_with_sys_init_and_exit (* Translate commands with optional Sys.init and exit *)
  |> List.map instruction_to_string (* Convert instructions to strings *)

(* Main function to process a VM file *)
let process_file input_filename output_filename =
  let lines = read_lines input_filename in
  let translated_lines = process_vm_lines lines in
  write_lines output_filename translated_lines

(* Entry point *)
let () =
  if Array.length Sys.argv <> 3 then
    Printf.eprintf "Usage: %s <input.vm> <output.asm>\n" Sys.argv.(0)
  else
    let input_filename = Sys.argv.(1) in
    let output_filename = Sys.argv.(2) in
    process_file input_filename output_filename
