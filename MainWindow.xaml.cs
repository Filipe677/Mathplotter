using System;
using System.Collections.Generic;
using System.Globalization;
using System.Windows;
using OxyPlot;
using OxyPlot.Series;
using OxyPlot.Axes;

namespace GUI
{
    public partial class MainWindow : Window
    {
        private PlotModel plotModel;
        private InterpreterState interpreterState;

        public MainWindow()
        {
            InitializeComponent();
            InitializePlotModel();
            interpreterState = new InterpreterState();
        }

        private void InitializePlotModel()
        {
            plotModel = new PlotModel { Title = "Function Plot" };

            plotModel.Axes.Add(new LinearAxis
            {
                Position = AxisPosition.Bottom,
                Title = "X",
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot,
                MajorGridlineThickness = 1,
                MinorGridlineThickness = 0.5,
                ExtraGridlines = new[] { 0.0 },
                ExtraGridlineThickness = 1.5
            });

            plotModel.Axes.Add(new LinearAxis
            {
                Position = AxisPosition.Left,
                Title = "Y",
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot,
                MajorGridlineThickness = 1,
                MinorGridlineThickness = 0.5,
                ExtraGridlines = new[] { 0.0 },
                ExtraGridlineThickness = 1.5
            });

            PlotView.Model = plotModel;
        }

        private void PlotButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                plotModel.Series.Clear();

                string function = FunctionInputBox.Text.Trim();
                if (string.IsNullOrWhiteSpace(function))
                {
                    MessageBox.Show("Please enter a function to plot.");
                    return;
                }

                if (function.StartsWith("x =", StringComparison.OrdinalIgnoreCase))
                {
                    MessageBox.Show("Please enter the function in the form 'y = f(x)' instead of 'x = f(y)'");
                    return;
                }

                if (function.StartsWith("y =", StringComparison.OrdinalIgnoreCase))
                {
                    function = function.Substring(3).Trim();
                }

                if (!TryParseRanges(out double xMin, out double xMax, out double yMin, out double yMax))
                {
                    return;
                }

                var series = new LineSeries
                {
                    StrokeThickness = 2,
                    Color = OxyColors.Blue,
                    InterpolationAlgorithm = null
                };

                double step = (xMax - xMin) / 2000;
                var interpreter = new InterpreterState();
                var points = new List<DataPoint>();

                for (double x = xMin; x <= xMax; x += step)
                {
                    try
                    {
                        string expression = function.Replace("x", $"({x.ToString(CultureInfo.InvariantCulture)})");
                        var result = interpreter.EvaluateExpression(expression);

                        if (!double.IsInfinity(result) && !double.IsNaN(result) && result >= yMin && result <= yMax)
                        {
                            points.Add(new DataPoint(x, result));
                        }
                    }
                    catch (Exception pointEx)
                    {
                        System.Diagnostics.Debug.WriteLine($"Error at x={x}: {pointEx.Message}");
                        continue;
                    }
                }

                points.Sort((a, b) => a.X.CompareTo(b.X));

                foreach (var point in points)
                {
                    series.Points.Add(point);
                }

                plotModel.Series.Add(series);

                // Update axes
                ((LinearAxis)plotModel.Axes[0]).Minimum = xMin;
                ((LinearAxis)plotModel.Axes[0]).Maximum = xMax;
                ((LinearAxis)plotModel.Axes[1]).Minimum = yMin;
                ((LinearAxis)plotModel.Axes[1]).Maximum = yMax;

                plotModel.InvalidatePlot(true);
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error plotting function: {ex.Message}");
            }
        }

        private void PlotPoint(double x, string function, InterpreterState interpreter, LineSeries series, double yMin, double yMax)
        {
            try
            {
                string expression = function.Replace("x", $"({x.ToString(CultureInfo.InvariantCulture)})");
                var result = interpreter.EvaluateExpression(expression);

                if (!double.IsInfinity(result) && !double.IsNaN(result) && result >= yMin && result <= yMax)
                {
                    series.Points.Add(new DataPoint(x, result));
                }
            }
            catch (Exception)
            {
                
            }
        }
        private bool TryParseRanges(out double xMin, out double xMax, out double yMin, out double yMax)
        {
            xMin = xMax = yMin = yMax = 0;
            try
            {
                if (!double.TryParse(XMinBox.Text, out xMin) ||
                    !double.TryParse(XMaxBox.Text, out xMax) ||
                    !double.TryParse(YMinBox.Text, out yMin) ||
                    !double.TryParse(YMaxBox.Text, out yMax))
                {
                    MessageBox.Show("Invalid range values. Please enter valid numbers.");
                    return false;
                }

                if (xMin >= xMax || yMin >= yMax)
                {
                    MessageBox.Show("Invalid range values. Make sure Min values are less than Max values.");
                    return false;
                }

                return true;
            }
            catch
            {
                MessageBox.Show("Invalid range values. Please enter valid numbers.");
                return false;
            }
        }

        private void EvaluateButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                string input = InputBox.Text;
                if (string.IsNullOrWhiteSpace(input))
                {
                    OutputBox.Text = "Error: Input is empty!";
                    return;
                }

                var interpreter = new InterpreterState();
                var result = interpreter.EvaluateExpression(input);
                OutputBox.Text = result.ToString(CultureInfo.InvariantCulture);
            }
            catch (Exception ex)
            {
                OutputBox.Text = $"Error: {ex.Message}";
            }
        }

        private void ClearButton_Click(object sender, RoutedEventArgs e)
        {
            InputBox.Text = string.Empty;
            OutputBox.Text = string.Empty;
            FunctionInputBox.Text = string.Empty;
        }

        private void Help_ValidSyntax_Click(object sender, RoutedEventArgs e)
        {
            MessageBox.Show(
                "Valid Syntax:\n\n" +
                "1. Basic arithmetic: +, -, *, /, ^, ()\n" +
                "2. Variable assignment: x = 5\n" +
                "3. Functions: y = 2*x + 1\n" +
                "4. Use 'x' as the variable in function plots\n" +
                "5. Powers: x^2, x^3, etc.",
                "Valid Syntax",
                MessageBoxButton.OK,
                MessageBoxImage.Information);
        }

        private void Help_Examples_Click(object sender, RoutedEventArgs e)
        {
            MessageBox.Show(
                "Examples:\n\n" +
                "Calculator:\n" +
                "2 + 3 * 4\n" +
                "x = 5\n" +
                "y = x + 3\n\n" +
                "Function Plotting:\n" +
                "2*x + 1\n" +
                "x^2\n" +
                "x^2 - 2*x + 1\n" +
                "3*x^2 + 2*x - 1",
                "Examples",
                MessageBoxButton.OK,
                MessageBoxImage.Information);
        }
    }
}