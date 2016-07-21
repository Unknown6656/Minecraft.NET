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
            var t = Interpreter.InterpreteFPGAL(@"
$1 = ($5 - (123)) <| (((($2))) * ((2 + (-1)) * (-3)))
", 8);
        }
    }
}
