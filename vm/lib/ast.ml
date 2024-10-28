type arithmetic_command=
  | Add
  | Sub
  | Neg
  | Eq
  | Gt
  | Lt
  | And
  | Or
  | Not

type memory_segment=
  | Local
  | Argument
  | This
  | That
  | Constant
  | Static
  | Temp
  | Pointer

type memory_command =
  | Push
  | Pop 

type program_flow_command=
  | Label of string
  | Goto of string
  | IfGoto of string

type function_command = 
  | Function of string*int
  | Call of string*int
  | Return

type vm_instruction =
  | Arithmetic of arithmetic_command
  | Memory of memory_command
  | ProgramFlow of program_flow_command
  | Function of function_command

