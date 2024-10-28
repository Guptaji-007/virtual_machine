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
        AInstr (AConstant 13);
        CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
        AInstr (ASymbol "SP");
        CInstr { dest = Some "A"; comp = "M-1"; jump = None; metadata = None };
        CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
        AInstr (AConstant 13);
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
      let pointer_address = if index = 0 then "THIS" else "THAT" in [
        AInstr (ASymbol "SP");
        CInstr { dest = Some "A"; comp = "M-1"; jump = None; metadata = None };
        CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
        AInstr (ASymbol pointer_address);
        CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
        AInstr (ASymbol "SP");
        CInstr { dest = Some "M"; comp = "M-1"; jump = None; metadata = None };
      ]
  | _ -> failwith "Unsupported segment for pop"

let vm_pf_command= function
  | Label label_name ->
    [ AInstr (ASymbol label_name) ]
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
  let init_locals = 
    List.init num_locals (fun _ -> 
      [
        AInstr (AConstant 0); (* Push 0 onto the stack (initial value for a local variable) *)
        CInstr { dest = Some "D"; comp = "A"; jump = None; metadata = None };
        AInstr (ASymbol "SP"); (* Set A to SP *)
        CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
        CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None }; (* Store 0 in local *)
        AInstr (ASymbol "SP");
        CInstr { dest = Some "M"; comp = "M+1"; jump = None; metadata = None }; (* Increment SP *)
      ]
    ) |> List.concat
  in
  [ AInstr (ASymbol name) ] @ init_locals
    

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
  
    (* ARG = SP - num_args - 5 *)
    AInstr (AConstant (num_args + 5));
    CInstr { dest = Some "D"; comp = "A"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "D"; comp = "M-D"; jump = None; metadata = None };
    AInstr (ASymbol "ARG");
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };
  
    (* LCL = SP *)
    AInstr (ASymbol "SP");
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "LCL");
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
    AInstr (AConstant 13);
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    (* returnAddress = *(endFrame - 5) *)
    AInstr (AConstant 5);
    CInstr { dest = Some "A"; comp = "D-A"; jump = None; metadata = None };
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (AConstant 14);
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    (* *ARG = pop() *)
    AInstr (ASymbol "SP");
    CInstr { dest = Some "A"; comp = "M-1"; jump = None; metadata = None };
    CInstr { dest = Some "D"; comp = "M"; jump = None; metadata = None };
    AInstr (ASymbol "ARG");
    CInstr { dest = Some "A"; comp = "M"; jump = None; metadata = None };
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    (* SP = ARG + 1 *)
    AInstr (ASymbol "ARG");
    CInstr { dest = Some "D"; comp = "M+1"; jump = None; metadata = None };
    AInstr (ASymbol "SP");
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    (* Restore THAT, THIS, ARG, LCL *)
    AInstr (AConstant 1);
    CInstr { dest = Some "D"; comp = "A-D"; jump = None; metadata = None };
    AInstr (ASymbol "THAT");
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    AInstr (AConstant 2);
    CInstr { dest = Some "D"; comp = "A-D"; jump = None; metadata = None };
    AInstr (ASymbol "THIS");
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    AInstr (AConstant 3);
    CInstr { dest = Some "D"; comp = "A-D"; jump = None; metadata = None };
    AInstr (ASymbol "ARG");
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    AInstr (AConstant 4);
    CInstr { dest = Some "D"; comp = "A-D"; jump = None; metadata = None };
    AInstr (ASymbol "LCL");
    CInstr { dest = Some "M"; comp = "D"; jump = None; metadata = None };

    (* Jump to return address *)
    AInstr (AConstant 14);
    CInstr { dest = None; comp = "0"; jump = Some "JMP"; metadata = None }
  ]


type vm_command =
  | FunctionCommand of function_command
  | ArithmeticCommand of arithmetic_command
  | MemoryCommand of memory_command * string * int

let translate_vm_command = function
  | FunctionCommand cmd -> 
      (match cmd with
      | Function (name, num_locals) -> resolve_function name num_locals
      | Call (function_name, num_args) -> resolve_call function_name num_args
      | Return -> resolve_return)
  | ArithmeticCommand cmd -> 
      vm_arithmetic_command cmd
  | MemoryCommand (command, segment, index) ->
      match command with
      | Push -> resolve_push segment index  
      | Pop -> resolve_pop segment index 


  end