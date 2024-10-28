open Ast
open Encode

module Parser = struct
  let rmspace line =
    let line = String.trim line in
    match String.index_opt line '/' with
    | Some pos when pos > 0 && line.[pos - 1] = '/' -> String.trim (String.sub line 0 (pos - 1))
    | Some pos -> String.trim (String.sub line 0 pos)
    | None -> line
  

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
    | _ -> failwith "Unrecognized/Incorrect format command"
  
  let parse_line line =
    let tokens = tokenize line in
    parse_command tokens

end
