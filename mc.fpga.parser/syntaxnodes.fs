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

type uop =
    | NOT | NEG
    member public this.Calculate (x1) =
        match this with
        | NOT -> ~~~x1
        | NEG -> SUB.Calculate(0, x1)

type Field =
    | Port of int
    | Constant of int
    | Variable of string
    override this.ToString() =
        match this with
        | Port(p) -> "$" + p.ToString()
        | Constant(c) -> c.ToString()
        | Variable(v) -> v

type Expression =
    | Value of Field
    | UnaryOperation of (uop * Expression)
    | BinaryOperation of (Expression * bop * Expression)
    override this.ToString() =
        match this with
        | Value(f) -> f.ToString()
        | UnaryOperation(op, x1) -> "(" + op.ToString() + x1.ToString() + ")"
        | BinaryOperation(x1, op, x2) -> "(" + x1.ToString() + " " + op.ToString() + " " + x2.ToString() + ")"
