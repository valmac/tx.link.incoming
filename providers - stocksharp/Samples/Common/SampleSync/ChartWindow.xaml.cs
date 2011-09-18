namespace SampleSync
{
	using System.ComponentModel;

	/// <summary>
	/// Interaction logic for ChartWindow.xaml
	/// </summary>
	partial class ChartWindow
	{
		public ChartWindow()
		{
			InitializeComponent();
		}

		public bool RealClose { get; set; }

		protected override void OnClosing(CancelEventArgs e)
		{
			if (!this.RealClose)
			{
				base.Hide();
				e.Cancel = true;
			}

			base.OnClosing(e);
		}
	}
}