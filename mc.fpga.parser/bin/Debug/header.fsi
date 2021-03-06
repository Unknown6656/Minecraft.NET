

namespace mc.fpga.parser
  type bop =
    | OR
    | AND
    | XOR
    | SHL
    | SHR
    | ROL
    | ROR
    | ADD
    | SUB
    | MOD
    | DIV
    | MUL
    | POW
    | EQ
    | NEQ
    | LOW
    | LEQ
    | GRT
    | GEQ
    with
      member ToCSFString : unit -> string
      member IsBoolean : bool
    end
  type uop =
    | NOT
    | NEG
    | BNOT
    with
      member ToCSFString : unit -> string
    end
  type Field =
    | Port of int
    | Constant of int
    | Variable of string
    with
      member ToCSString : unit -> string
      override ToString : unit -> string
    end
  type Expression =
    | Value of Field
    | UnaryOperation of (uop * Expression)
    | BinaryOperation of (Expression * bop * Expression)
    with
      member ToCSString : unit -> string
      override ToString : unit -> string
    end
  type ConstantOperation =
    | UnaryContantOperation of uop * Field
    | BinaryContantOperation of Field * bop * Field
  type ParsingError =
    | InvalidVariableString
    | AssginmentExpressionExpected
    | VariableAlreadyDefined
    | InvalidAssignment
    | InvalidBracesCount
    | NotAConstantExpression
    | InvalidNumberFormat
    | InvalidExpression
    | InvalidPortNumber

namespace mc.fpga.parser
  module Globals = begin
    val inline ( |RegEx|_| ) :
      p:string ->
        i:string -> System.Text.RegularExpressions.GroupCollection option
    val inline ( |RegExc|_| ) :
      p:string ->
        i:string -> System.Text.RegularExpressions.GroupCollection option
    val inline ( /?? ) : x:'a option -> y:'a -> 'a
  end
  type Error =
    class
      new : l:int * m:string -> Error
      new : l:int * p:ParsingError -> Error
      val Line: int
      val Message: string
      override ToString : unit -> string
    end
  module Strings = begin
    val UnaryOperator : (uop * string) []
    val BinaryOperator : (bop * string) []
    val GetUnaryOperator : s:string -> uop
    val GetBinaryOperator : s:string -> bop
    val StripBraces : s:string -> string
    val VariableRegex : string
    val PortRegex : string
    val ConstantRegex : string
    val FieldRegex : string
    val DefineRegex : string
    val AssignmentRegex : string
    val UnaryOperatorRegex : string
    val BinaryOperatorRegex : string
    val internal bin : f:string -> string
    val UnaryOperationRegex : string
    val BinaryOperationRegex : string
    val EntryPoint : string []
  end
  module Resources = begin
    val internal mgnt : System.Resources.ResourceManager
    val ProgramFrame : string
  end
  type ConstantProcessingResult =
    | Success of int
    | Failure of ParsingError
  type ParsingResult<'a> =
    | Success of 'a
    | Failure of ParsingError
  type CompilationResult =
    | Success of System.Reflection.Assembly
    | Failure of Error []
  type InterpretationResult =
    | Method of System.Reflection.MethodInfo
    | Errors of Error []
  module Interpreter = begin
    val enc : s:string -> string
    val ParseVariabe : s:ParsingResult<string> * b:bool -> ParsingResult<Field>
    val ParseVariabeP : s:ParsingResult<string> -> ParsingResult<Field>
    val Parse : s:string * size:int -> string [] * Error []
    val CompileCS : code:string -> CompilationResult
    val InterpreteFPGAL :
      code:string * size:int -> string * InterpretationResult
  end

