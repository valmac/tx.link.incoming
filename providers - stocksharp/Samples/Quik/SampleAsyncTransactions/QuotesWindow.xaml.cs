namespace SampleAsyncTransactions
{
	using System.ComponentModel;
	using System.Windows;
	using System.Windows.Controls;
	using System.Windows.Input;
	using System.Windows.Media;

	using StockSharp.BusinessEntities;

	using Ecng.Xaml;

	public partial class QuotesWindow
	{
		public QuotesWindow()
		{
			this.Quotes = new ThreadSafeObservableCollection<SampleQuote>();
			InitializeComponent();
		}

		public Security Security { get; set; }
		public ThreadSafeObservableCollection<SampleQuote> Quotes { get; private set; }

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

		private void QuotesCtrl_MouseDoubleClick(object sender, MouseButtonEventArgs e)
		{
			// если нажат левый Ctrl, то снимаем все заявки
			if (Keyboard.IsKeyDown(Key.LeftCtrl))
			{
				MainWindow.Instance.Trader.CancelOrders(false, MainWindow.Instance.Portfolio, null, null, this.Security);
				return;
			}

			var quote = GetSelectedQuote(e);

			if (quote != null)
			{
				MainWindow.Instance.Trader.RegisterOrder(new Order
				{
					Security = quote.Quote.Security,
					Direction = quote.Quote.OrderDirection,
					Price = quote.Quote.Price,
					Volume = 1,
					Portfolio = MainWindow.Instance.Portfolio,
				});
			}
		}

		private SampleQuote GetSelectedQuote(RoutedEventArgs e)
		{
			var dep = (DependencyObject)e.OriginalSource;

			while ((dep != null) && !(dep is ListViewItem))
			{
				dep = VisualTreeHelper.GetParent(dep);
			}

			if (dep == null)
				return null;

			return (SampleQuote)QuotesCtrl.ItemContainerGenerator.ItemFromContainer(dep);
		}
	}
}