namespace SampleGUI
{
	using System.Collections.ObjectModel;
	using System.ComponentModel;

	using StockSharp.BusinessEntities;

	public partial class QuotesWindow
	{
		public QuotesWindow()
		{
			this.Quotes = new ObservableCollection<Quote>();
			InitializeComponent();
		}

		public ObservableCollection<Quote> Quotes { get; private set; }

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