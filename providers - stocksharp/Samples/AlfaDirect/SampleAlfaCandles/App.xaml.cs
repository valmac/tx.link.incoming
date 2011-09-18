namespace SampleSmartCandles
{
	using System.Windows;
	using System.Windows.Threading;

	partial class App
	{
		private void Application_DispatcherUnhandledException(object sender, DispatcherUnhandledExceptionEventArgs e)
		{
			MessageBox.Show(base.MainWindow, e.Exception.ToString());
			e.Handled = true;
		}
	}
}