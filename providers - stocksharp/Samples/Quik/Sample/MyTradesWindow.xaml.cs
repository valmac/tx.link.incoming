namespace Sample
{
	using System.Collections.ObjectModel;
	using System.ComponentModel;

	using StockSharp.BusinessEntities;

	public partial class MyTradesWindow
	{
		public MyTradesWindow()
		{
			this.Trades = new ObservableCollection<MyTrade>();
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

		public ObservableCollection<MyTrade> Trades { get; private set; }
	}
}
