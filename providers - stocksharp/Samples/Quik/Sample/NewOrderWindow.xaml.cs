namespace Sample
{
	using System.Windows;

	using Ecng.Common;

	using StockSharp.BusinessEntities;

	public partial class NewOrderWindow
	{
		public NewOrderWindow()
		{
			InitializeComponent();
			this.Portfolio.Trader = MainWindow.Instance.Trader;
		}

		public Security Security { get; set; }

		private void Send_Click(object sender, RoutedEventArgs e)
		{
			var order = new Order
			{
				Portfolio = this.Portfolio.SelectedPortfolio,
				Volume = this.Volume.Text.To<int>(),
				Price = this.Price.Text.To<decimal>(),
				Security = this.Security,
				Direction = this.IsBuy.IsChecked == true ? OrderDirections.Buy : OrderDirections.Sell,
			};
			
			MainWindow.Instance.Trader.RegisterOrder(order);
			base.DialogResult = true;
		}
	}
}
