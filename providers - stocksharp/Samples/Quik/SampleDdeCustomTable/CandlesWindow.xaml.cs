namespace SampleDdeCustomTable
{
	using System.ComponentModel;

	using Ecng.Xaml;

	public partial class CandlesWindow
	{
		public CandlesWindow()
		{
			InitializeComponent();
			this.Candles = new ThreadSafeObservableCollection<QuikCandle>();
			this.CandleDetails.ItemsSource = this.Candles;
		}

		public ThreadSafeObservableCollection<QuikCandle> Candles { get; private set; }

		protected override void OnClosing(CancelEventArgs e)
		{
			if (!this.RealClose)
			{
				base.Hide();
				e.Cancel = true;
			}

			base.OnClosing(e);
		}

		public bool RealClose { get; set; }
	}
}