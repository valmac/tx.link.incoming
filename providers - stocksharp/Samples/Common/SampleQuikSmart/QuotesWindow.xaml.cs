namespace SampleQuikSmart
{
	using System.ComponentModel;

	using StockSharp.BusinessEntities;

	using Ecng.Xaml;

	public partial class QuotesWindow
	{
		public QuotesWindow()
		{
			this.Quotes = new ThreadSafeObservableCollection<Quote>();
			InitializeComponent();
		}

		public ThreadSafeObservableCollection<Quote> Quotes { get; private set; }

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