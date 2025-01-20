using System;
using System.Collections.Generic;
using System.Globalization;

namespace GUI
{
    public class InterpreterState
    {
        private Dictionary<string, double> symbolTable;

        public InterpreterState()
        {
            symbolTable = new Dictionary<string, double>();
        }

        public double EvaluateExpression(string expression)
        {
            try
            {
                expression = expression.Trim();

                if (expression.StartsWith("y =", StringComparison.OrdinalIgnoreCase))
                {
                    expression = expression.Substring(3).Trim();
                }

                return global::MathInterpreter.Program.EvaluateExpression(expression);
            }
            catch (Exception ex)
            {
                throw new Exception($"Evaluation error: {ex.Message}");
            }
        }

        public double EvaluateFunctionPoint(string function, double x)
        {
            try
            {
                string expression = function.Replace("x", x.ToString(CultureInfo.InvariantCulture));
                return EvaluateExpression(expression);
            }
            catch (Exception ex)
            {
                throw new Exception($"Function evaluation error at x={x}: {ex.Message}");
            }
        }
    }
}