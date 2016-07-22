namespace mc.fpga.parser


type bop =
    | OR | AND | XOR | SHL | SHR | ROL | ROR | ADD | SUB | MOD | DIV | MUL | POW | EQ | NEQ | LOW | LEQ | GRT | GEQ
    member public this.IsBoolean
        with get() =
            match this with
            | EQ | LOW | GRT | NEQ | GEQ | LEQ -> true
            | _ -> false
    member this.ToCSFString() =
        match this with
        | OR -> "(§1 | §2)"
        | AND -> "(§1 & §2)"
        | XOR -> "(§1 ^ §2)"
        | SHL -> "(int)((ulong)(§1 << §2) & 0xffffffffUL)"
        | SHR -> "(int)(§1 >> §2)"
        | ROL -> "rol(§1, §2)"
        | ROR -> "ror(§1, §2)"
        | ADD -> "(§1 + §2)"
        | SUB -> "(§1 - §2)"
        | MOD -> "(§1 % §2)"
        | DIV -> "(§1 / §2)"
        | MUL -> "(§1 * §2)"
        | POW -> "(int)System.Math.Pow(§1, §2)"
        | EQ ->  "((§1 == §2) ? 1 : 0)"
        | NEQ -> "((§1 != §2) ? 1 : 0)"
        | LOW -> "((§1 < §2) ? 1 : 0)"
        | LEQ -> "((§1 <= §2) ? 1 : 0)"
        | GRT -> "((§1 > §2) ? 1 : 0)"
        | GEQ -> "((§1 >= §2) ? 1 : 0)"

type uop =
    | NOT | NEG | BNOT
    member this.ToCSFString() =
        match this with
        | BNOT -> "(§1 == 0 ? 1 : 0)"
        | NOT -> "(~(§1))"
        | NEG -> "(-(§1))"
  
type Field =
    | Port of int
    | Constant of int
    | Variable of string
    override this.ToString() =
        match this with
        | Port(p) -> "$" + p.ToString()
        | Constant(c) -> c.ToString()
        | Variable(v) -> v
    member this.ToCSString() =
        match this with
        | Port(p) -> "ports[" + p.ToString() + "]"
        | Constant(c) -> "(" + c.ToString() + ")"
        | Variable(v) -> "variables[\"__var" + v + "\"]" 

type Expression =
    | Value of Field
    | UnaryOperation of (uop * Expression)
    | BinaryOperation of (Expression * bop * Expression)
    override this.ToString() =
        match this with
        | Value(f) -> f.ToString()
        | UnaryOperation(op, x1) -> "(" + op.ToString() + x1.ToString() + ")"
        | BinaryOperation(x1, op, x2) -> "(" + x1.ToString() + " " + op.ToString() + " " + x2.ToString() + ")"
    member this.ToCSString() =
        match this with
        | Value(f) -> "(int)" + f.ToCSString()
        | UnaryOperation(op, x1) ->
            op.ToCSFString().Replace("§1", x1.ToCSString())
        | BinaryOperation(x1, op, x2) ->
            op.ToCSFString().Replace("§1", x1.ToCSString()).Replace("§2", x2.ToCSString())

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
