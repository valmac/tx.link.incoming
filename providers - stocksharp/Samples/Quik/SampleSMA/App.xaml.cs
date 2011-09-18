namespace SampleSMA
{
	using System.Windows;
	using System.Windows.Threading;

	/// <summary>
	/// Interaction logic for App.xaml
	/// </summary>
	public partial class App
	{
		private void Application_DispatcherUnhandledException(object sender, DispatcherUnhandledExceptionEventArgs e)
		{
			MessageBox.Show(base.MainWindow, e.Exception.ToString());
			e.Handled = true;
		}
	}
}