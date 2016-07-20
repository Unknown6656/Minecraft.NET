

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
      member Calculate : x1:int * x2:int -> int
    end
  type uop =
    | NOT
    | NEG
    with
      member Calculate : x1:int -> int
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
      override ToString : unit -> string
    end

namespace mc.fpga.parser
  type ParsingError =
    | InvalidVariableString
    | AssginmentExpressionExpected
    | VariableAlreadyDefined
    | InvalidAssignment
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
    val ( |RegEx|_| ) :
      p:string ->
        i:string -> System.Text.RegularExpressions.GroupCollection option
    val ConstantRegex : string
    val VariableRegex : string
    val PortRegex : string
    val DefineRegex : string
    val OutRegex : string
    val InRegex : string
    val AssignmentRegex : string
    val EntryPoint : string []
  end
  module Resources = begin
    val internal mgnt : System.Resources.ResourceManager
    val ProgramFrame : string
  end
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
    val ParseVariabe : s:ParsingResult<string> -> ParsingResult<Field>
    val Parse : s:string * size:int -> string [] * Error []
    val CompileCS : code:string * mainclass:string -> CompilationResult
    val InterpreteFPGAL : code:string * size:int -> InterpretationResult
  end
  module Test = begin
    val testun : op:string -> x1:string -> int
    val testbin : x1:string -> op:string -> x2:string -> int
    val testbinw : s:string -> string
  end

