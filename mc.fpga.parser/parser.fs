namespace mc.fpga.parser

open System.Text.RegularExpressions
open System.Collections.Generic
open System

type uop =
    | NOT | NEG | INC | DEC

type bop =
    | OR | AND | XOR | SHL | SHR | ROL | ROR | ADD | SUB | MOD | DIV | MUL | POW | EQ | NEQ | LOW | LEQ | GRT | GEQ

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

type Error =
    val Line : int
    val Message : string
    new(l, m) = {
        Line = l
        Message = m
    }
    override this.ToString() =
        sprintf "Error on line %d: '%s'." this.Line this.Message

module Strings =
    let UnaryOperator = [|
            for op in Enum.GetValues(typeof<uop>) |> Seq.cast<uop> do
                yield (op, match op with
                            | NOT -> "~"
                            | NEG -> "-"
                            | INC -> "++"
                            | DEC -> "--"
                            )
        |]
    let BinaryOperator = [|
            for op in Enum.GetValues(typeof<bop>) |> Seq.cast<bop> do
                yield (op, match op with
                            | OR -> "|"
                            | AND -> "&"
                            | XOR -> "^"
                            | SHL -> "<<"
                            | SHR -> ">>"
                            | ROL -> "<|"
                            | ROR -> "|>"
                            | ADD -> "+"
                            | SUB -> "-"
                            | MOD -> "%"
                            | DIV -> "/"
                            | MUL -> "*"
                            | POW -> "**"
                            | EQ -> "="
                            | NEQ -> "<>"
                            | LOW -> "<"
                            | LEQ -> "<="
                            | GRT -> ">"
                            | GEQ -> ">="
                            )
        |]

module Parser =
    let Parse (s : string, size : int) : string[] * Error[] =
        let rec binop (x1 : int, op : bop, x2 : int) =
            let inv o = 1 - binop(x1, o, x2)
            match op with
            | AND -> x1 &&& x2
            | OR -> x1 ||| x2
            | XOR -> x1 ^^^ x2
            | SHL -> x1 <<< x2
            | SHR -> x1 >>> x2
            | ROL -> (x1 <<< x2) ||| (x1 >>> (32 - x2))
            | ROR -> (x1 >>> x2) ||| (x1 <<< (32 - x2))
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
        let unop (op : uop, x1) =
            match op with
            | NOT -> ~~~x1
            | NEG -> binop(0, SUB, x1)
            | INC -> binop(x1, ADD, 1)
            | DEC -> binop(x1, SUB, 1)
//        let procins (i : (int * Operation)[], p : int[]) =
//            for ins in i do
//                let n = fst(ins)
//                let res = match snd(ins) with
//                          | UnaryOperation(x1, o) -> unop(o, p.[x1])
//                          | BinaryOperation(x1, o , x2) -> binop(p.[x1], o, p.[x2])
//                Array.set p n res
        let lines = [|
                        for l in s.Split('\r', '\n') do
                            let t = (if l.Contains("#") then l.Remove(l.IndexOf '#') else l).Trim()
                            if t.Length > 0 then yield t
                    |]
        let mutable error = List.Empty
        let add l e = e::l
        let oline = [|
                        for i in 0 .. lines.Length do
                            let line = lines.[i].ToLower()
                            if line.StartsWith "def " then
                                // vars.Add ... 0
                                ()
                            else if line.StartsWith "in " then
                                ()
                            else if line.StartsWith "out " then
                                ()
                            else
                                if line.Contains "=" then
                                    let left = (line.Split '=').[0].Trim()
                                    let right = line.Remove(0, line.IndexOf('=') + 1).Trim()


                                    ()
                                else
                                    error <- add(error, Error(i, "Expected assignment expression"))
                    |]
        (oline, error |> List.toArray)