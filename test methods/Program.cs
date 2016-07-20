using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

using mc.fpga.parser;

namespace test_methods
{
    public static class Program
    {
        public static void Main(string[] args)
        {
            var o = from s in new string[] { "top ", "315", "elv ", "kek" } select Test.testbinw(s);
            var a = o.ToArray();

            var r = Resources.ProgramFrame;
        }
    }
}
