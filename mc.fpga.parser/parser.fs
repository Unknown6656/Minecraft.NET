﻿#if INTERACTIVE
    #load "header.fsi"
    #load "syntaxnodes.fs"
#endif

namespace mc.fpga.parser

open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection
open System.Collections.Generic
open System.CodeDom.Compiler
open System.Globalization
open System.Reflection
open System.Resources
open Microsoft.CSharp
open System.Text
open System

[<AutoOpen>]
module Globals =
    let inline (|RegEx|_|) p i =
        let m = System.Text.RegularExpressions.Regex.Match (i, p, RegexOptions.Compiled + RegexOptions.IgnoreCase)
        if m.Success then
            Some m.Groups
        else
            None
    let inline (|RegExc|_|) p i =
        let m = System.Text.RegularExpressions.Regex.Match (i, p, RegexOptions.Compiled)
        if m.Success then
            Some m.Groups
        else
            None
    let inline (/??) (x : option<_>) y = if x.IsNone then y else x.Value
    
type Error =
    val Line : int
    val Message : string
    new(l, m) = {
        Line = l
        Message = m
    }
    new(l, p : ParsingError) = {
        Line = l
        Message = match p with
                  | InvalidVariableString -> "The given variable string is invalid"
                  | VariableAlreadyDefined -> "The variable has already been defined"
                  | AssginmentExpressionExpected -> "Expected an assignment expression"
                  | InvalidAssignment -> "The assignment of a value to a constant is not valid"
                  | InvalidBracesCount -> "The count of the braces `(` and `)` must be equal"
                  | NotAConstantExpression -> "The given expression is not constant"
                  | InvalidNumberFormat -> "The given string could not be parsed as number"
                  | InvalidExpression -> "The expression is invalid"
    }
    override this.ToString() =
        sprintf "Error on line %d: '%s'." this.Line this.Message

module Strings =
    let UnaryOperator = [|
            (NOT, "~")
            (NEG, "-")
            (BNOT, "!")
        |]
    let BinaryOperator = [|
            (OR, "|")
            (AND, "&")
            (XOR, "^")
            (SHL, "<<")
            (SHR, ">>")
            (ROL, "<|")
            (ROR, "|>")
            (ADD, "+")
            (SUB, "-")
            (MOD, "%")
            (DIV, "/")
            (MUL, "*")
            (POW, "**")
            (EQ, "=")
            (NEQ, "<>")
            (LOW, "<")
            (LEQ, "<=")
            (GRT, ">")
            (GEQ, ">=")
        |]
    let GetUnaryOperator s =
        UnaryOperator
        |> Array.find (fun e -> snd e = s)
        |> fst
    let GetBinaryOperator s =
        BinaryOperator
        |> Array.find (fun e -> snd e = s)
        |> fst
    let rec StripBraces (s :string) =
        let s = s.Trim()
        if s.StartsWith("(") && s.EndsWith(")") then
            StripBraces s.[1 .. s.Length - 2]
        else
            s
    let VariableRegex = "(\b?[_a-z]\w*\b?)"
    let PortRegex = @"(\b?\$[1-9]*[0-9])"
    let ConstantRegex = @"(\-?([0-9]+|[0-9a-f]+h\b?))"
    let FieldRegex = sprintf "(%s|%s|%s)" ConstantRegex VariableRegex PortRegex
//    let InRegex = @"^in\s+(" + PortRegex + @")$"
//    let OutRegex = @"^out\s+(" + PortRegex + @")$"
    let DefineRegex = @"^def\s+(" + VariableRegex + @")$"
    let AssignmentRegex = @"^(?<target>" + PortRegex + "|" + VariableRegex + @")\s*\=\s*(?<expression>.+)$"
    let UnaryOperatorRegex = @"(?<operator>[~\-\!])"
    let BinaryOperatorRegex = @"(?<operator>(\<=|\>=|=|\<\>|\&|\^|\<\<|\>\>|\|\>|\<\||\+|\-|\||\*\*|\*|\<|\>|\/|%))"
    let internal bin f = @"(?<" + f + ">(\(.+\)|" + FieldRegex + @"))"
    let UnaryOperationRegex = UnaryOperatorRegex + @"\s*" + bin("x")
    let BinaryOperationRegex = bin("x") + @"\s*" + BinaryOperatorRegex + @"\s*" + bin("y")
    let EntryPoint = [| "FPGAL.Program"; "Process" |]

module Resources =
    let internal mgnt =
        new ResourceManager("resources", typeof<Error>.Assembly)
    let public ProgramFrame =
        mgnt.GetString("programframe", CultureInfo.InvariantCulture)
        
type ConstantProcessingResult =
    | Success of int
    | Failure of ParsingError

type ParsingResult<'a> =
    | Success of 'a
    | Failure of ParsingError

type CompilationResult =
    | Success of Assembly
    | Failure of Error[]
    
type InterpretationResult =
    | Method of MethodInfo
    | Errors of Error[]
    
/// <summary>
/// Represents an MC-FPGAL interpreter
/// </summary>
module Interpreter =
    let enc s = "^\s*" + s + "\s*$"

    /// <summary>
    /// Parses the given string as variable, port or constant
    /// </summary>
    /// <param name="s">Variable string</param>
    /// <param name="b">Flag, which indicates whether the field parsing should be global or not</param>
    let ParseVariabe (s : ParsingResult<string>, b : bool) : ParsingResult<Field> =
        let suc = ParsingResult.Success
        let err = ParsingResult.Failure
        let penc = if b then enc else fun s -> s
        match s with
        | ParsingResult.Success(sr) ->
            try
                match Strings.StripBraces sr with
                | RegEx (penc Strings.VariableRegex) g -> suc(Field.Variable(g.[1].ToString()))
                | RegEx (penc Strings.PortRegex) g -> suc(Field.Port(Int32.Parse(g.[1].ToString().Remove(0, 1))))
                | RegEx (penc Strings.ConstantRegex) g ->
                    let c = g.[1].ToString().ToLower()
                    if c.EndsWith "h" then
                        suc(Field.Constant(Int32.Parse(c.[0 .. c.Length - 2], NumberStyles.HexNumber + NumberStyles.AllowLeadingSign)))
                    else
                        suc(Field.Constant(Int32.Parse c))
                | _ -> err InvalidVariableString
            with
            | ex -> err InvalidVariableString
        | ParsingResult.Failure(m) -> err m
        
    /// <summary>
    /// Parses the given string as variable, port or constant
    /// </summary>
    /// <param name="s">Variable string</param>
    let ParseVariabeP s = ParseVariabe(s, false)

    /// <summary>
    /// Parses the given MC-FPAL code into C# code
    /// </summary>
    /// <param name="s">MC-FPGAL code</param>
    /// <param name="size">FPGAL size (aka number of pins)</param>
    let Parse (s : string, size : int) : string[] * Error[] =
        let errors = new List<Error>()
        let vars = new List<String>()
        let suc = ParsingResult.Success
        let canstrip (s : string) =
            if s.StartsWith("(") && s.EndsWith(")") then
                let cnt = ref 0
                [|
                   for c in s.[0 .. s.Length - 2] do match c with
                                                     | '(' -> cnt := !cnt + 1
                                                     | ')' -> cnt := !cnt - 1
                                                     | _ -> ()
                                                     yield !cnt > 0
                |]
                |> Array.exists ((=) false)
                |> not
            else
                false
        let rec matchcore (s : string) =
            try
                let s = s.Trim()
                let s = if canstrip s then Strings.StripBraces s else s
                match ParseVariabe(suc s, true) with
                | ParsingResult.Success f -> match f with
                                             | Port p -> if (p < size) && (p >= 0) then
                                                            f.ToCSString() |> suc
                                                         else
                                                            ParsingResult.Failure InvalidPortNumber
                                             | _ -> f.ToCSString() |> suc
                | ParsingResult.Failure f ->
                    match s with
                    | RegEx (enc Strings.BinaryOperationRegex) g ->
                        let op = g.["operator"].ToString() |> Strings.GetBinaryOperator
                        let x1 = g.["x"].ToString >> matchcore
                        let x2 = g.["y"].ToString >> matchcore
                        match (x1(), x2()) with
                        | (ParsingResult.Success f1, ParsingResult.Success f2) ->
                            op.ToCSFString().Replace("§1", f1).Replace("§2", f2)
                            |> suc
                        | _ -> ParsingResult.Failure InvalidExpression
                    | RegEx (enc Strings.UnaryOperationRegex) g ->
                        let op = g.["operator"].ToString() |> Strings.GetUnaryOperator
                        match g.["x"].ToString() |> matchcore with
                        | ParsingResult.Success f ->
                            op.ToCSFString().Replace("§1", f)
                            |> suc
                        | ParsingResult.Failure f -> ParsingResult.Failure f
                    | _ -> ParsingResult.Failure InvalidExpression
            with
            | _ -> ParsingResult.Failure InvalidExpression
        let lines = [|
                        for l in s.Split('\r', '\n') do
                            let t = (if l.Contains("#") then l.Remove(l.IndexOf '#') else l).Trim()
                            if t.Length > 0 then yield t
                    |]
        let oline = [|
                        let err i (a : ParsingError) = Error(i + 1, a) |> errors.Add
                        let errm i (m : string) = Error(i + 1, m) |> errors.Add
                        for i in 0 .. lines.Length - 1 do
                            let line = lines.[i].ToLower()
                            match line with
                            | RegEx Strings.DefineRegex g ->
                                let var = g.[1].ToString()
                                if vars.Contains var then
                                    err i VariableAlreadyDefined
                                else
                                    vars.Add var
                                    yield "variables.Add(\"__var" + var + "\", 0);"
                            | RegEx Strings.AssignmentRegex g ->
                                let expr = g.["expression"].ToString()
                                let cnt = ref 0
                                for c in expr do match c with
                                                 | '(' -> cnt := !cnt + 1
                                                 | ')' -> cnt := !cnt - 1
                                                 | _ -> ()
                                if !cnt <> 0 then
                                    err i InvalidBracesCount
                                else
                                    match matchcore expr with
                                    | ParsingResult.Failure f -> err i f
                                    | ParsingResult.Success code ->
                                        match g.["target"].ToString()
                                              |> suc
                                              |> ParseVariabeP with
                                        | ParsingResult.Failure f -> err i f
                                        | ParsingResult.Success m -> match m with
                                                                     | Constant c -> err i InvalidAssignment
                                                                     | _ -> yield m.ToCSString() + " = (int)(" + code + ");"
                            |_ -> if line.Contains "=" then
                                      err i AssginmentExpressionExpected
                                  else
                                      errm i ("The statement `" + line + "` could not be interpreted")
                    |]
        (oline, errors |> Seq.toArray)
        
    /// <summary>
    /// Compiles the given C# code into a non-executable assembly
    /// </summary>
    /// <param name="code">C# code</param>
    let CompileCS (code : string) : CompilationResult =
        let prov = new CSharpCodeProvider()
        let parm = new CompilerParameters(
                       IncludeDebugInformation = true,
                       GenerateExecutable = false,
                       GenerateInMemory = true,
                       CompilerOptions = "/optimize+ /platform:anycpu /unsafe" // "/debug"
                   ) in
            parm.ReferencedAssemblies.AddRange [| "mscorlib.dll"; "System.dll"; "System.Data.dll" |]
        let res = prov.CompileAssemblyFromSource(parm, code)
        if res.Errors.HasErrors then
            Failure [|
                        for e in res.Errors do
                            if not e.IsWarning then
                                yield Error(e.Line, e.ErrorText)
                    |]
        else
            Success res.CompiledAssembly
    
    /// <summary>
    /// Interpretes the given MC-FPGAL code and compiles it into an invokable method instance
    /// </summary>
    /// <param name="code">MC-FPGAL code</param>
    /// <param name="size">FPGAL size (aka number of pins)</param>
    let InterpreteFPGAL (code : string, size : int) : string * InterpretationResult =
        let ident = new String(' ', 16)
        let res = Parse(code, size)
        let err = snd res
        if err.Length > 0 then
            ("", Errors err)
        else
            let cscode = [
                            let cls = Strings.EntryPoint.[0].Split('.')
                            let pfrm = Resources.ProgramFrame
                                                .Replace("__NAMESPACE__", cls.[0])
                                                .Replace("__CLASS__", cls.[1])
                                                .Replace("__METHOD__", Strings.EntryPoint.[1])
                                                .Replace("__SIZE__", size.ToString())
                            let offs = pfrm.IndexOf("/*__ENTRYPOINT__*/")
                            yield pfrm.[0 .. offs - 1]
                            for line in fst res -> ident + line + "\n"
                            yield pfrm.[offs + "/*__ENTRYPOINT__*/".Length .. pfrm.Length - 1]
                         ]
                         |> List.fold (+) ""
            (cscode, match CompileCS cscode with
                     | Success a -> Method(a.GetType(Strings.EntryPoint.[0]).GetMethod(Strings.EntryPoint.[1], BindingFlags.Static + BindingFlags.Public))
                     | Failure f -> Errors f
                     )
