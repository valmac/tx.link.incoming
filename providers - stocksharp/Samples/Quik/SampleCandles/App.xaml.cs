namespace SampleCandles
{
	using System.Windows;
	using System.Windows.Threading;

	/// <summary>
	/// Interaction logic for App.xaml
	/// </summary>
	partial class App
	{
		private void Application_DispatcherUnhandledException(object sender, DispatcherUnhandledExceptionEventArgs e)
		{
			MessageBox.Show(base.MainWindow, e.Exception.ToString());
			e.Handled = true;
		}
	}
}