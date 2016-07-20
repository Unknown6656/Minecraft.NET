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
    let VariableRegex = "([_a-z]\w*)"
    let PortRegex = @"(\$[1-9]*[0-9])"
    let DefineRegex = @"^def\s+(" + VariableRegex + @")$"
    let OutRegex = @"^def\s+(" + PortRegex + @")$"
    let InRegex = @"^def\s+(" + PortRegex + @")$"
    let AssignmentRegex = @"^(?<target>" + PortRegex + "|" + VariableRegex + ")\s*\=\s*(?<expression>.+)$"
    let ProgramEntry = @"//# ENTRY POINT"

module Resources =
    let internal mgnt =
        new ResourceManager("resources", typeof<Error>.Assembly)
    let public ProgramFrame =
        mgnt.GetString("programframe", CultureInfo.InvariantCulture)

type CompilationResult =
    | Success of Assembly
    | Failure of Error[]
    
type InterpretationResult =
    | Method of MethodInfo
    | Errors of Error[]

module Interpreter =
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

    let Parse (s : string, size : int) : string[] * Error[] =
        let error = new List<Error>()
        let lines = [|
                        for l in s.Split('\r', '\n') do
                            let t = (if l.Contains("#") then l.Remove(l.IndexOf '#') else l).Trim()
                            if t.Length > 0 then yield t
                    |]
        let oline = [|
                        for i in 0 .. lines.Length do
                            let line = lines.[i].ToLower()
                            match line with
                            | Strings.RegEx Strings.DefineRegex g ->
                                yield "int " + g.ToString() + " = 0;"
//                            | Strings.RegEx Strings.OutRegex g -> ()
//                            | Strings.RegEx Strings.InRegex g -> ()
                            | Strings.RegEx Strings.AssignmentRegex g ->
                                let targ = g.["target"].ToString()
                                let expr = g.["expression"].ToString()


                                ()
                            |_ ->
                                if line.Contains "=" then
                                    Error(i, "Expected assignment expression") |> error.Add
                                else
                                    Error(i, "The line `" + line + "` could not be interpreted") |> error.Add
                    |]
        (oline, error |> Seq.toArray)

    let InterpreteFPGAL (code : string, size : int) : InterpretationResult =
        let res = Parse(code, size)
        let err = snd res
        if err.Length > 0 then
            Errors err
        else
            let cscode = [
                            let pfrm = Resources.ProgramFrame
                            let offs = pfrm.IndexOf(Strings.ProgramEntry)
                            yield pfrm.[0 .. offs]
                            for line in fst res -> line
                            yield pfrm.[offs + Strings.ProgramEntry.Length .. pfrm.Length - 1]
                         ]
                         |> List.fold (+) ""
            match CompileCS(cscode, "FPGAL.Program") with
            | Success a -> Method(a.GetType("FPGAL.Program").GetMethod("Process", BindingFlags.Static + BindingFlags.Public))
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
        