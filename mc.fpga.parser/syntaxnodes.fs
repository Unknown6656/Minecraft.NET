namespace mc.fpga.parser


type bop =
    | OR | AND | XOR | SHL | SHR | ROL | ROR | ADD | SUB | MOD | DIV | MUL | POW | EQ | NEQ | LOW | LEQ | GRT | GEQ
    member public this.Calculate (x1 : int, x2 : int) =
        let rec intbinop (x1 : int, op : bop, x2 : int) =
            let inv o = 1 - intbinop(x1, o, x2)
            match op with
            | AND -> x1 &&& x2
            | OR -> x1 ||| x2
            | XOR -> x1 ^^^ x2
            | SHL -> x1 <<< x2
            | SHR -> x1 >>> x2
            | ROL -> let o = (32 + x2) % 32
                     (x1 <<< o) ||| (x1 >>> (32 - o))
            | ROR -> intbinop(x1, ROL, -x2)
            | ADD -> x1 + x2
            | SUB -> x1 - x2
            | MUL -> x1 * x2
            | DIV -> x1 / x2
            | MOD -> x1 % x2
            | POW -> float(x1) ** float(x2)
                        |> int
            | EQ -> if x1 = x2 then 1 else 0
            | LOW -> if x1 < x2 then 1 else 0
            | GRT -> if x1 > x2 then 1 else 0
            | NEQ -> inv EQ
            | GEQ -> inv LOW
            | LEQ -> inv GRT
        intbinop(x1, this, x2)
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
    member public this.Calculate (x1) =
        match this with
        | NOT -> ~~~x1
        | BNOT -> if x1 = 0 then 1 else 0
        | NEG -> SUB.Calculate(0, x1)
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
