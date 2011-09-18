namespace SampleSmart
{
	using System.ComponentModel;

	using StockSharp.BusinessEntities;

	using Ecng.Xaml;

	public partial class PortfoliosWindow
	{
		public PortfoliosWindow()
		{
			this.Portfolios = new ThreadSafeObservableCollection<Portfolio>();
			InitializeComponent();
			this.PortfolioDetails.ItemsSource = this.Portfolios;
		}

		public ThreadSafeObservableCollection<Portfolio> Portfolios { get; private set; }

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