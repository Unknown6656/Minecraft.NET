using System.Collections.Generic;
using System.Threading.Tasks;
using System.Linq;
using System.Text;
using System;

namespace mcfpga.cs
{
    public static class Parser
    {

        public static Tuple<ParserResult, ParserError> Parse(string code);
    }

    public interface ParserResult
    {
        Dictionary<int, int> Ports { get; }
        byte[] RawProgram { get; }

        bool Execute();
    }

    [Serializable]
    public class ParserError
    {
        public int Line { set; get; }
        public bool Critical { set; get; }
        public string Message { set; get; }

        public ParserError(int l, string msg, bool crit = true)
        {
            this.Line = l;
            this.Message = msg;
            this.Critical = crit;
        }
    }
}
