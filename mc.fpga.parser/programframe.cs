using System.Collections.Generic;
using System;

namespace __NAMESPACE__
{
    public static class __CLASS__
    {
        public static void __METHOD__(ref int[] ports)
        {
            Dictionary<string, int> variables = new Dictionary<string, int>();
            Func<int, int, int> rol = new Func<int, int, int>((v, o) => {
                o += 32;
                o %= 32;

                return (v << o) | (v >> (32 - o));
            });
            Func<int, int, int> ror = new Func<int, int, int>((v, o) => rol(v, 32 - o));

            unchecked
            {
/*__ENTRYPOINT__*/
            }
        }
    }
}
