namespace SampleGUI
{
	using System;
	using System.ComponentModel;
	using System.Windows;
	using System.Windows.Controls;

	using Ecng.Common;

	using StockSharp.Algo;
	using StockSharp.BusinessEntities;

	public partial class NewOrderWindow
	{
		public NewOrderWindow()
		{
			InitializeComponent();
			this.Portfolio.Trader = MainWindow.Instance.Trader;
		}

		protected override void OnClosing(CancelEventArgs e)
		{
			this.Portfolio.Trader = null;
			base.OnClosing(e);
		}

		public Security Security { get; set; }

		private void Send_Click(object sender, RoutedEventArgs e)
		{
			try
			{
				//var dir = OrderDirections.Buy;

				//for (int i = 0; i < 10; i++)
				//{
				//    var order = new Order
				//    {
				//        Type = this.IsMarket.IsChecked == true ? OrderTypes.Market : OrderTypes.Limit,
				//        Portfolio = this.Portfolio.SelectedPortfolio,
				//        Volume = 1,
				//        Price = dir == OrderDirections.Buy ? this.Security.BestBid.Price + 200 : this.Security.BestAsk.Price - 200,
				//        Security = this.Security,
				//        Direction = dir,
				//    };

				//    dir = dir.Invert();

				//    MainWindow.Instance.Trader.RegisterOrder(order);
				//}

				var order = new Order
				{
					Type = this.IsMarket.IsChecked == true ? OrderTypes.Market : OrderTypes.Limit,
					Portfolio = this.Portfolio.SelectedPortfolio,
					Volume = this.Volume.Text.To<int>(),
					Price = this.Price.Text.To<decimal>(),
					Security = this.Security,
					Direction = this.IsBuy.IsChecked == true ? OrderDirections.Buy : OrderDirections.Sell,
				};

				MainWindow.Instance.Trader.RegisterOrder(order);

				base.DialogResult = true;
			}
			catch (Exception ex)
			{
				MessageBox.Show(this, ex.Message, "Ошибка");
			}
		}

		private void Portfolio_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			this.Send.IsEnabled = this.Portfolio.SelectedPortfolio != null;
		}

		private void IsMarket_Click(object sender, RoutedEventArgs e)
		{
			this.Price.IsEnabled = this.IsMarket.IsChecked != true;
		}
	}
}
