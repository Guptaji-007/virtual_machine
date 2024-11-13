open Ast
open Assembler_ast

module Encode = struct

let resolve_binary_ac op = [
  AInstr (ASymbol "SP"); 
  CInstr { dest = Some "A"; comp = "M-1"; jump = None; metadata = None }; 
  CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };    
  CInstr { dest = Some "A"; comp = "A-1"; jump = None; metadata = None };  
  CInstr { dest = Some "M"; comp = op; jump = None; metadata = None };    
  AInstr (ASymbol "SP"); 
  CInstr { dest = Some "M"; comp = "M-1"; jump = None; metadata = None };  
]
let resolve_unary_ac op =[
  AInstr (ASymbol "SP"); 
  CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None }; 
  CInstr { dest = Some "A"; comp = "A-1"; jump = None; metadata = None };
  CInstr { dest = Some "M"; comp = op ; jump = None; metadata = None };
]

let label_counter = ref 0
let generate_label prefix =
  let label = prefix ^ string_of_int !label_counter in
  incr label_counter;
  label

let resolve_cmp comp = 
  let true_label = generate_label "TRUE_" in
  let end_label = generate_label "END_" in
  [
    AInstr (ASymbol "SP");                                 
    CInstr { dest = Some "A"; comp = "M-1"; jump = None; metadata = None }; 
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };   
    CInstr { dest = Some "A"; comp = "A-1"; jump = None; metadata = None }; 
    CInstr { dest = Some "D"; comp = "M-D"; jump = None; metadata = None }; 
    CInstr { dest = None; comp = "D"; jump = Some comp; metadata = None };
    AInstr (ASymbol true_label);                           
    CInstr { dest = Some "D"; comp = "0"; jump = None; metadata = None };
    AInstr (ASymbol end_label);                            
    CInstr { dest = None; comp = "0"; jump = Some "JMP"; metadata = None }; 
    AInstr (ASymbol true_label);                           
    CInstr { dest = Some "D"; comp = "-1"; jump = None; metadata = None };   
    AInstr (ASymbol end_label);                           
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };   
    AInstr (ASymbol "SP");                                
    CInstr { dest = Some "M"; comp = "M-1"; jump = None; metadata = None }; 
  ]


let vm_arithmetic_command = function
  | Add -> resolve_binary_ac "M+D"
  | Sub -> resolve_binary_ac "M-D"
  | And -> resolve_binary_ac "M&D"
  | Or  -> resolve_binary_ac "M|D"
  | Not -> resolve_unary_ac  "!M"
  | Neg -> resolve_unary_ac  "-M"
  | Eq  -> resolve_cmp "JEQ" 
  | Gt  -> resolve_cmp "JGT" 
  | Lt  -> resolve_cmp "JLT"  
  


let resolve_push segment index =
  match segment with
  | "constant" -> [
      AInstr (AConstant index);
      CInstr { dest = Some "D"; comp = "A"; jump = None; metadata = None };
      AInstr (ASymbol "SP");
      CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
      CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
      AInstr (ASymbol "SP");
      CInstr { dest = Some "M"; comp = "M+1"; jump = None; metadata = None };
    ]
  | "local" | "argument" | "this" | "that" ->
      let base_address = match segment with
        | "local" -> "LCL"
        | "argument" -> "ARG"
        | "this" -> "THIS"
        | "that" -> "THAT"
        | _ -> failwith "Invalid segment"
      in [
        AInstr (ASymbol base_address);
        CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
        AInstr (AConstant index);
        CInstr { dest = Some "A"; comp = "D+A"; jump = None; metadata = None };
        CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
        AInstr (ASymbol "SP");
        CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
        CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
        AInstr (ASymbol "SP");
        CInstr { dest = Some "M"; comp = "M+1"; jump = None; metadata = None };
      ]
  | "temp" ->
      let temp_address = 5 + index in [
        AInstr (AConstant temp_address);
        CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
        AInstr (ASymbol "SP");
        CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
        CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
        AInstr (ASymbol "SP");
        CInstr { dest = Some "M"; comp = "M+1"; jump = None; metadata = None };
      ]
  | "pointer" ->
      let pointer_address = if index = 0 then "THIS" else "THAT" in [
        AInstr (ASymbol pointer_address);
        CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
        AInstr (ASymbol "SP");
        CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
        CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
        AInstr (ASymbol "SP");
        CInstr { dest = Some "M"; comp = "M+1"; jump = None; metadata = None };
      ]
  | "static" -> 
  let static_address = 16 + index in
  [
    AInstr (AConstant static_address);     (* Load static address (16 + index) *)
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None }; (* D = M[static_address] *)
    AInstr (ASymbol "SP");                 (* Load SP into A register *)
    CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None }; (* A = SP *)
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None }; (* M[SP] = D *)
    AInstr (ASymbol "SP");                 (* Load SP into A register again *)
    CInstr { dest = Some "M"; comp = "M+1"; jump = None; metadata = None }; (* SP = SP + 1 *)
  ]
  | _ -> failwith "Unsupported segment for push"

let resolve_pop segment index =
  match segment with
  | "local" | "argument" | "this" | "that" ->
      let base_address = match segment with
        | "local" -> "LCL"
        | "argument" -> "ARG"
        | "this" -> "THIS"
        | "that" -> "THAT"
        | _ -> failwith "Invalid segment"
      in [
        AInstr (ASymbol base_address);
        CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
        AInstr (AConstant index);
        CInstr { dest = Some "D"; comp = "D+A"; jump = None; metadata = None };
        AInstr (ASymbol "R13");
        CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
        AInstr (ASymbol "SP");
        CInstr { dest = Some "A"; comp = "M-1"; jump = None; metadata = None };
        CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
        AInstr (ASymbol "R13");
        CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
        CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
        AInstr (ASymbol "SP");
        CInstr { dest = Some "M"; comp = "M-1"; jump = None; metadata = None };
      ]
  | "temp" ->
      let temp_address = 5 + index in [
        AInstr (ASymbol "SP");
        CInstr { dest = Some "A"; comp = "M-1"; jump = None; metadata = None };
        CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
        AInstr (AConstant temp_address);
        CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
        AInstr (ASymbol "SP");
        CInstr { dest = Some "M"; comp = "M-1"; jump = None; metadata = None };
      ]
    | "pointer" ->
    let pointer_address = if index = 0 then "R3" else "R4" in
    [
      AInstr (ASymbol pointer_address);                             (* Load SP into A register *)
      CInstr { dest = Some "D"; comp = "A"; jump = None; metadata = None }; (* A = SP - 1 *)
      AInstr (ASymbol "R13");                  (* Load pointer address (THIS or THAT) *)
      CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None }; (* M = D (store in pointer) *)
      AInstr (ASymbol "SP");                             (* Load SP into A register again *)
      CInstr { dest = Some "AM"; comp = "M-1"; jump = None; metadata = None }; (* SP = SP - 1 *)
      CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None }; (* SP = SP - 1 *)
      AInstr (ASymbol "R13");                  (* Load pointer address (THIS or THAT) *)
      CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None }; (* M = D (store in pointer) *)
      CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None }; (* M = D (store in pointer) *)
    ]
    | "static" -> 
    let static_address = 16 + index in [
      AInstr (AConstant static_address);    (* Load static address (16 + index) *)
      CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };  (* A = static_address *)
      CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };  (* D = M[static_address] *)
      AInstr (ASymbol "SP");                (* Load SP into A register *)
      CInstr { dest = Some "A"; comp = "M-1"; jump = None; metadata = None }; (* A = SP - 1 *)
      CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };  (* M[SP-1] = D *)
      AInstr (ASymbol "SP");                (* Load SP into A register again *)
      CInstr { dest = Some "M"; comp = "M-1"; jump = None; metadata = None }; (* SP = SP - 1 *)
    ]
  
  | _ -> failwith "Unsupported segment for pop"

let vm_pf_command= function
  | Label label_name ->
    [ LInstr (LSymbol label_name)]
  | Goto label_name ->
    [ AInstr (ASymbol label_name); CInstr { dest = None; comp = "0"; jump = Some "JMP"; metadata = None } ]
  | IfGoto label_name ->
    [
      AInstr (ASymbol "SP");  
      CInstr { dest = Some "A"; comp = "M-1"; jump = None; metadata = None }; 
      CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };   
      AInstr (ASymbol label_name);  
      CInstr { dest = None; comp = "D"; jump = Some "JNE"; metadata = None }; 
      AInstr (ASymbol "SP");
      CInstr { dest = Some "M"; comp = "M-1"; jump = None; metadata = None };  
    ]

let resolve_function name num_locals =
  let function_label = LInstr (LSymbol name) in  (* Wrap the name as LSymbol within LInstr *)

  let init_local () = [
    AInstr (AConstant 0);                                
    CInstr { dest = Some "D"; comp = "A"; jump = None; metadata = None };
    AInstr (ASymbol "SP");                               
    CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None }; 
    AInstr (ASymbol "SP");
    CInstr { dest = Some "M"; comp = "M+1"; jump = None; metadata = None }; 
  ] in

  let init_locals = List.init num_locals (fun _ -> init_local ()) |> List.concat in

  function_label :: init_locals  (* Ensure we return LInstr here *)


let resolve_call function_name num_args =
  let return_label = generate_label "RETURN_" in
  [
    AInstr (ASymbol return_label); (* Push return address *)
    CInstr { dest = Some "D"; comp = "A"; jump = None; metadata = None };
    AInstr (ASymbol "SP"); CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
    AInstr (ASymbol "SP"); CInstr { dest = Some "M"; comp = "M+1"; jump = None; metadata = None };
  
    (* Push LCL *)
    AInstr (ASymbol "LCL");
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "M"; comp = "M+1"; jump = None; metadata = None };
  
    (* Push ARG *)
    AInstr (ASymbol "ARG");
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "M"; comp = "M+1"; jump = None; metadata = None };
  
    (* Push THIS *)
    AInstr (ASymbol "THIS");
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "M"; comp = "M+1"; jump = None; metadata = None };
  
    (* Push THAT *)
    AInstr (ASymbol "THAT");
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "M"; comp = "M+1"; jump = None; metadata = None };

    (* LCL = SP *)
    AInstr (ASymbol "SP");
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "LCL");
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
  
     (* ARG = SP - num_args - 5 *) (*<---changes can be made here--->*)
    AInstr (AConstant (num_args + 5));
    CInstr { dest = Some "D"; comp = "A"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "D"; comp = "M-D"; jump = None; metadata = None };
    AInstr (ASymbol "ARG");
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
    
    (* Jump to the function *)
    AInstr (ASymbol function_name);
    CInstr { dest = None; comp = "0"; jump = Some "JMP"; metadata = None };
  
    (* Declare return label *)
    AInstr (ASymbol return_label)
  ]


let resolve_return =
  [
    (* endFrame = LCL *)
    AInstr (ASymbol "LCL");
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "R13");
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    (* returnAddress = *(endFrame - 5) *)
    AInstr (AConstant 5);
    CInstr { dest = Some "A"; comp = "D-A"; jump = None; metadata = None };
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "R14");
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    (* *ARG = pop() *)
    AInstr (ASymbol "SP");
    CInstr { dest = Some "M"; comp = "M-1"; jump = None; metadata = None };  (*changed a=m-1 to am=m-1*)
    CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };  (*changed a=m-1 to am=m-1*)
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "ARG");
    CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    (* SP = ARG + 1 *)
    AInstr (ASymbol "ARG");
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "M"; comp = "D+1"; jump = None; metadata = None };

    (* Restore THAT, THIS, ARG, LCL *)
    AInstr (ASymbol "R13");
    CInstr { dest = Some "AM"; comp = "M-1"; jump = None; metadata = None };
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "THAT");
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    AInstr (ASymbol "R13");
    CInstr { dest = Some "AM"; comp = "M-1"; jump = None; metadata = None };
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "THIS");
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    AInstr (ASymbol "R13");
    CInstr { dest = Some "AM"; comp = "M-1"; jump = None; metadata = None };
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "ARG");
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    AInstr (ASymbol "R13");
    CInstr { dest = Some "AM"; comp = "M-1"; jump = None; metadata = None };
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "LCL");
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    (* Jump to return address *)
    AInstr (ASymbol "R14");
    CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
    CInstr { dest = None; comp = "0"; jump = Some "JMP"; metadata = None }; 

  ]

(* Define the types for VM commands *)

type vm_command =
  | FunctionCommand of function_command
  | ArithmeticCommand of arithmetic_command
  | MemoryCommand of memory_command * string * int
  | ProgramFlowCommand of program_flow_command

(* Function to initialize the stack pointer and call Sys.init *)
let initialize_and_call_sys_init () =
  let return_label = generate_label "Sys.init$ret" in
  [
    (* Set SP to 256 *)
    AInstr (AConstant 256);
    CInstr { dest = Some "D"; comp = "A"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    (* Call Sys.init *)
    AInstr (ASymbol return_label);
    CInstr { dest = Some "D"; comp = "A"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "M"; comp = "M+1"; jump = None; metadata = None };

    (* Save LCL, ARG, THIS, THAT on the stack *)
    AInstr (ASymbol "LCL");
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "M"; comp = "M+1"; jump = None; metadata = None };

    AInstr (ASymbol "ARG");
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "M"; comp = "M+1"; jump = None; metadata = None };

    AInstr (ASymbol "THIS");
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "M"; comp = "M+1"; jump = None; metadata = None };

    AInstr (ASymbol "THAT");
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "M"; comp = "M+1"; jump = None; metadata = None };

    AInstr (ASymbol "SP");
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "LCL");
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    AInstr (AConstant 5);
    CInstr { dest = Some "D"; comp = "D-A"; jump = None; metadata = None };
    AInstr (AConstant 0);
    CInstr { dest = Some "D"; comp = "D-A"; jump = None; metadata = None };
    AInstr (ASymbol "ARG");
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    (* Jump to Sys.init *)
    AInstr (ASymbol "Sys.init");
    CInstr { dest = None; comp = "0"; jump = Some "JMP"; metadata = None };

    (* Return label *)
    AInstr (ASymbol return_label)
  ]

(* Function to translate a single VM command *)
let translate_vm_command = function
  | FunctionCommand cmd -> 
      (match cmd with
      | Function (name, num_locals) -> resolve_function name num_locals
      | Call (function_name, num_args) -> resolve_call function_name num_args
      | Return -> resolve_return)
  | ArithmeticCommand cmd -> 
      vm_arithmetic_command cmd
  | ProgramFlowCommand cmd -> 
      vm_pf_command cmd
  | MemoryCommand (command, segment, index) ->
      match command with
      | Push -> resolve_push segment index  
      | Pop -> resolve_pop segment index 

(* Function to translate a list of VM commands with initial Sys.init setup *)
let translate_vm_commands commands =
  let sys_init_setup = initialize_and_call_sys_init () in
  let translated_commands = List.concat_map translate_vm_command commands in
  sys_init_setup @ translated_commands

end
