namespace mc.fpga.parser
    type bop =
        | OR | AND | XOR | SHL | SHR | ROL | ROR | ADD | SUB | MOD | DIV | MUL | POW | EQ | NEQ | LOW | LEQ | GRT | GEQ
        with
            member Calculate : x1:int * x2:int -> int
        end
    type uop =
        | NOT | NEG
        with
            member Calculate : x1:int -> int
        end
    type Field =
        | Port of int
        | Constant of int
        | Variable of string
        with
            override ToString : unit -> string
            member ToCSString : unit -> string
        end
    type Expression =
        | Value of Field
        | UnaryOperation of (uop * Expression)
        | BinaryOperation of (Expression * bop * Expression)
        with
            override ToString : unit -> string
        end
    type Error =
        class
            new : l:int * m:string -> Error
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
            val ( |RegEx|_| ) : p:string -> i:string -> System.Text.RegularExpressions.GroupCollection option
            val VariableRegex : string
            val PortRegex : string
            val DefineRegex : string
            val OutRegex : string
            val InRegex : string
            val AssignmentRegex : string
            val ProgramEntry : string
        end
    module Resources = begin
            val internal mgnt : System.Resources.ResourceManager
            val ProgramFrame : string
        end
    type CompilationResult =
        | Success of System.Reflection.Assembly
        | Failure of Error []
    type InterpretationResult =
        | Method of System.Reflection.MethodInfo
        | Errors of Error []
    type ParsingResult<'a> =
        | Success of 'a
        | Failure of string
    module Interpreter = begin
            val CompileCS : code:string * mainclass:string -> CompilationResult
            val Parse : s:string * size:int -> string [] * Error []
            val InterpreteFPGAL : code:string * size:int -> InterpretationResult
        end
    module Test = begin
            val testun : op:string -> x1:string -> int
            val testbin : x1:string -> op:string -> x2:string -> int
            val testbinw : s:string -> string
        end
