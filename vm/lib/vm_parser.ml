open Ast
open Encode

module Parser = struct
  let rmspace line =
    let line = String.trim line in
    match String.index_opt line '/' with
    | Some pos when pos >= 0 && pos + 1 < String.length line && line.[pos] = '/' && line.[pos + 1] = '/' ->
        String.trim (String.sub line 0 pos)
    | _ -> line
  
  let tokenize line =
    line
    |> rmspace                   (* Clean up the line *)
    |> String.split_on_char ' '   (* Split by spaces *)
    |> List.filter (fun s -> s <> "")  (* Remove any empty tokens *)
  

  
  let parse_command tokens =
    match tokens with
    | ["push"; segment; index] -> Encode.MemoryCommand (Push, segment, int_of_string index)
    | ["pop"; segment; index] -> MemoryCommand (Pop, segment, int_of_string index)
    | ["add"] -> ArithmeticCommand Add
    | ["sub"] -> ArithmeticCommand Sub
    | ["neg"] -> ArithmeticCommand Neg
    | ["eq"] -> ArithmeticCommand Eq
    | ["gt"] -> ArithmeticCommand Gt
    | ["lt"] -> ArithmeticCommand Lt
    | ["and"] -> ArithmeticCommand And
    | ["or"] -> ArithmeticCommand Or
    | ["not"] -> ArithmeticCommand Not
    | ["function"; name; n_vars] -> FunctionCommand (Function (name, int_of_string n_vars))
    | ["call"; name; n_args] -> FunctionCommand (Call (name, int_of_string n_args))
    | ["return"] -> FunctionCommand Return
    | ["label"; label_name] -> ProgramFlowCommand (Label label_name)
    | ["goto"; label_name] -> ProgramFlowCommand (Goto label_name)
    | ["if-goto"; label_name] -> ProgramFlowCommand (IfGoto label_name)
    | _ -> failwith "Unrecognized/Incorrect format command"
    
  let parse_line line =
    let tokens = tokenize line in
    match tokens with
    | [] -> None  (* Skip empty or comment-only lines *)
    | _ -> Some (parse_command tokens)
end
