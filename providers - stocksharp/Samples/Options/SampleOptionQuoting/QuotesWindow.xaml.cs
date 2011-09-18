namespace SampleOptionQuoting
{
	using System.Collections.ObjectModel;
	using System.ComponentModel;
	using System.Linq;

	using Ecng.Xaml;
	using Ecng.Collections;

	using StockSharp.Algo;
	using StockSharp.BusinessEntities;

	public partial class QuotesWindow
	{
		private MarketDepth _depth;

		public QuotesWindow()
		{
			this.Quotes = new ObservableCollection<IVQuote>();
			InitializeComponent();
		}

		public ObservableCollection<IVQuote> Quotes { get; private set; }

		public void Init(Security security)
		{
			_depth = security.Trader.GetMarketDepth(security);
			_depth.QuotesChanged += OnQuotesChanged;
			_depth.Trader.RegisterQuotes(_depth.Security);
		}

		private void OnQuotesChanged()
		{
			var ivDepth = _depth.IV();

			this.GuiAsync(() =>
			{
				this.Quotes.Clear();
				this.Quotes.AddRange(ivDepth.Select(q => new IVQuote(q)));
			});
		}

		protected override void OnClosing(CancelEventArgs e)
		{
			_depth.QuotesChanged -= OnQuotesChanged;
			_depth.Trader.UnRegisterQuotes(_depth.Security);
			base.OnClosing(e);
		}
	}
}