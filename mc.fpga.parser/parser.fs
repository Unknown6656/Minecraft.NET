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

type ParsingError =
    | InvalidVariableString
    | AssginmentExpressionExpected
    | VariableAlreadyDefined
    | InvalidAssignment

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
    }
    override this.ToString() =
        sprintf "Error on line %d: '%s'." this.Line this.Message

module Strings =
    let UnaryOperator = [|
            (NOT, "~")
            (NEG, "-")
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
        let st = s.Trim()
        if st.StartsWith("(") && st.EndsWith(")") then
            StripBraces st.[1 .. s.Length - 2]
        else
            st
    let (|RegEx|_|) p i =
        let m = System.Text.RegularExpressions.Regex.Match (i, p)
        if m.Success then
            Some m.Groups
        else
            None
    let ConstantRegex = @"(\-?([0-9]+|[0-9a-f]+h))"
    let VariableRegex = "([_a-z]\w*)"
    let PortRegex = @"(\$[1-9]*[0-9])"
    let DefineRegex = @"^def\s+(" + VariableRegex + @")$"
    let OutRegex = @"^def\s+(" + PortRegex + @")$"
    let InRegex = @"^def\s+(" + PortRegex + @")$"
    let AssignmentRegex = @"^(?<target>" + PortRegex + "|" + VariableRegex + ")\s*\=\s*(?<expression>.+)$"
    let EntryPoint = [| "FPGAL.Program"; "Process" |]

module Resources =
    let internal mgnt =
        new ResourceManager("resources", typeof<Error>.Assembly)
    let public ProgramFrame =
        mgnt.GetString("programframe", CultureInfo.InvariantCulture)

type ParsingResult<'a> =
    | Success of 'a
    | Failure of ParsingError

type CompilationResult =
    | Success of Assembly
    | Failure of Error[]
    
type InterpretationResult =
    | Method of MethodInfo
    | Errors of Error[]

module Interpreter =
    let ParseVariabe (s : ParsingResult<string>) : ParsingResult<Field> =
        let suc = ParsingResult.Success
        let err = ParsingResult.Failure
        match s with
        | ParsingResult.Success(s) ->
            match s with
            | Strings.RegEx Strings.VariableRegex g -> suc(Field.Variable(g.[1].ToString()))
            | Strings.RegEx Strings.PortRegex g -> suc(Field.Port(Int32.Parse(g.[1].ToString().Remove(0, 1))))
            | Strings.RegEx Strings.ConstantRegex g ->
                                                    let c = g.[1].ToString().ToLower()
                                                    if c.EndsWith "h" then
                                                        suc(Field.Constant(Int32.Parse(c.[0 .. c.Length - 2], NumberStyles.HexNumber + NumberStyles.AllowLeadingSign)))
                                                    else
                                                        suc(Field.Constant(Int32.Parse c))
            | _ -> err InvalidVariableString
        | ParsingResult.Failure(m) -> err m

    let Parse (s : string, size : int) : string[] * Error[] =
        let error = new List<Error>()
        let vars = new List<String>()
        let lines = [|
                        for l in s.Split('\r', '\n') do
                            let t = (if l.Contains("#") then l.Remove(l.IndexOf '#') else l).Trim()
                            if t.Length > 0 then yield t
                    |]
        let oline = [|
                        let suc = ParsingResult.Success
                        for i in 0 .. lines.Length do
                            let line = lines.[i].ToLower()
                            match line with
                            | Strings.RegEx Strings.DefineRegex g ->
                                let var = g.ToString()
                                if vars.Contains var then
                                    Error(i, VariableAlreadyDefined) |> error.Add
                                else
                                    vars.Add var
                                    yield "variables.Add(\"__var" + var + "\", 0)"
                            | Strings.RegEx Strings.OutRegex g -> () // TODO
                            | Strings.RegEx Strings.InRegex g -> () // TODO
                            | Strings.RegEx Strings.AssignmentRegex g ->
                                let expr = Strings.StripBraces(g.["expression"].ToString())
                                let code = null


                                // EVAL EXPRESSION


                                let targp = g.["target"].ToString()
                                            |> suc
                                            |> ParseVariabe
                                match targp with
                                | ParsingResult.Failure f -> Error(i, f) |> error.Add
                                | ParsingResult.Success m ->
                                    match m with
                                    | Constant c -> Error(i, InvalidAssignment) |> error.Add
                                    | _ -> yield m.ToCSString() + " = (int)(" + code + ");"
                                ()
                            |_ ->
                                if line.Contains "=" then
                                    Error(i, AssginmentExpressionExpected) |> error.Add
                                else
                                    Error(i, "The statement `" + line + "` could not be interpreted") |> error.Add
                    |]
        (oline, error |> Seq.toArray)
        
    let CompileCS (code : string, mainclass) : CompilationResult =
        let prov = new CSharpCodeProvider()
        let parm = new CompilerParameters(
                       IncludeDebugInformation = true,
                       GenerateExecutable = true,
                       GenerateInMemory = true,
                       MainClass = mainclass,
                       CompilerOptions = "/optimize+ /platform:anycpu" // "/debug"
                   )
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

    let InterpreteFPGAL (code : string, size : int) : InterpretationResult =
        let ident = new String('\t', 5)
        let res = Parse(code, size)
        let err = snd res
        if err.Length > 0 then
            Errors err
        else
            let cscode = [
                            let cls = Strings.EntryPoint.[0].Split('.')
                            let pfrm = Resources.ProgramFrame
                                                .Replace("__NAMESPACE__", cls.[0])
                                                .Replace("__CLASS__", cls.[1])
                                                .Replace("__METHOD__", Strings.EntryPoint.[1])
                            let offs = pfrm.IndexOf("/*__ENTRYPOINT__*/")
                            yield pfrm.[0 .. offs]
                            for line in fst res -> ident + line
                            yield pfrm.[offs + "/*__ENTRYPOINT__*/".Length .. pfrm.Length - 1]
                         ]
                         |> List.fold (+) ""
            match CompileCS(cscode, Strings.EntryPoint.[0]) with
            | Success a -> Method(a.GetType(Strings.EntryPoint.[0]).GetMethod(Strings.EntryPoint.[1], BindingFlags.Static + BindingFlags.Public))
            | Failure f -> Errors f

module Test =
    let testun op x1 =
        let uop = Strings.GetUnaryOperator op
        x1
        |> Int32.Parse
        |> uop.Calculate

    let testbin x1 op x2 =
        let bop = Strings.GetBinaryOperator op
        bop.Calculate(Int32.Parse(x1), Int32.Parse(x2))

    let testbinw (s : string) =
        s
        