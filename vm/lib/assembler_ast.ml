(* assembler_ast.ml *)

(* A instruction *)
type 'a a_instruction =
  | AConstant of int    (* @12 *)
  | ASymbol of 'a       (* @label *)
  

(* C instruction *)
type 'a c_instruction = {
  dest : string option; (* destination *)
  comp : string;        (* computation *)
  jump : string option; (* jump *)
  metadata : 'a option; (* added metadata option *)
}

type 'a l_instruction =
  | LSymbol of 'a   

(* Overall instruction *)
type 'a instruction =
  | AInstr of 'a a_instruction
  | CInstr of 'a c_instruction
  | LInstr of 'a l_instruction 

