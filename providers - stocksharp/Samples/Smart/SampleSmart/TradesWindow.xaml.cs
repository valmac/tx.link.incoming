namespace SampleSmart
{
	using System.Collections.ObjectModel;
	using System.ComponentModel;

	using StockSharp.BusinessEntities;

	public partial class TradesWindow
	{
		public TradesWindow()
		{
			this.Trades = new ObservableCollection<Trade>();
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

		public ObservableCollection<Trade> Trades { get; private set; }
	}
}