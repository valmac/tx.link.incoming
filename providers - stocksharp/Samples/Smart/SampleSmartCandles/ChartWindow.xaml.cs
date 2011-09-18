namespace SampleSmartCandles
{
	using System.Collections.Generic;
	using System.ComponentModel;

	using StockSharp.Algo.Candles;

	partial class ChartWindow
	{
		private readonly List<Candle> _candles = new List<Candle>();

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

		public void DrawCandles(IEnumerable<Candle> candles)
		{
			_candles.AddRange(candles);
			stockChart1.DataSets[0].ItemsSource = _candles;
		}
	}
}