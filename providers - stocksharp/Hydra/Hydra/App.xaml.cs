namespace StockSharp.Hydra
{
	using System.Windows;
	using System.Windows.Threading;

	public partial class App
	{
		private void Application_DispatcherUnhandledException(object sender, DispatcherUnhandledExceptionEventArgs e)
		{
			if (MainWindow != null)
			{
				MessageBox.Show(MainWindow, e.Exception.ToString());
				((MainWindow)MainWindow).LogError(e.Exception);
				e.Handled = true;
			}
			else
				MessageBox.Show(e.Exception.ToString());
		}
	}
}