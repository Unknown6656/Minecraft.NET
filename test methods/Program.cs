using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

using mc.fpga.parser;
using System.Text.RegularExpressions;
using System.Reflection;

namespace test_methods
{
    public static class Program
    {
        public static void Main(string[] args)
        {
            const int pincount = 8;
            string code = @"
def tmp1
def tmp2
tmp1 = $1 << 3
tmp2 = $5 >= 0
$2 = tmp1 ^ tmp2
";
            var result = Interpreter.InterpreteFPGAL(code, pincount);

            Console.WriteLine(code + "\n");
            Console.WriteLine(result.Item1 + "\n");

            if (result.Item2.IsMethod)
            {
                MethodInfo nfo = (result.Item2 as InterpretationResult.Method).Item;
                int[] fpga_pins = new int[pincount] { 0, 1, 0, 1, 0, 1, 0, 1 };

                Console.WriteLine(string.Join(", ", fpga_pins)); // `BEFORE` STATE
                nfo.Invoke(null, new object[] { fpga_pins }); // TEST ACTUAL METHOD
                Console.WriteLine(string.Join(", ", fpga_pins)); // `AFTER` STATE
            }
        }
    }
}
