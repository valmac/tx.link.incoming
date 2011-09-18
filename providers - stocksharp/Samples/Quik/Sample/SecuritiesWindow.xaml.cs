namespace Sample
{
	using System;
	using System.Collections.ObjectModel;
	using System.ComponentModel;
	using System.Linq;
	using System.Threading;
	using System.Windows;
	using System.Windows.Controls;

	using Ecng.Collections;
	using Ecng.Common;
	using Ecng.Xaml;

	using StockSharp.BusinessEntities;

	public partial class SecuritiesWindow
	{
		private readonly Timer _timer;
		private readonly SynchronizedDictionary<Security, QuotesWindow> _quotesWindows = new SynchronizedDictionary<Security, QuotesWindow>();

		public SecuritiesWindow()
		{
			this.Securities = new ObservableCollection<Security>();
			InitializeComponent();

			_timer = TimeSpan.FromSeconds(1).CreateTimer(() => _quotesWindows.SyncDo(d =>
			{
				foreach (var p in d)
				{
					var pair = p;

					var wnd = pair.Value;

					wnd.GuiAsync(() =>
					{
						wnd.Quotes.Clear();
						wnd.Quotes.AddRange(MainWindow.Instance.Trader.GetMarketDepth(pair.Key).Select(q => new SampleQuote(q)));
					});
				}
			}));
		}

		protected override void OnClosing(CancelEventArgs e)
		{
			if (!this.RealClose)
			{
				base.Hide();
				e.Cancel = true;
			}
			else
			{
				_timer.Dispose();

				_quotesWindows.SyncDo(d =>
				{
					foreach (var pair in d)
					{
						MainWindow.Instance.Trader.UnRegisterQuotes(pair.Key);

						pair.Value.RealClose = true;
						pair.Value.Close();
					}
				});
			}

			base.OnClosing(e);
		}

		public ObservableCollection<Security> Securities { get; private set; }

		public bool RealClose { get; set; }

		private void NewOrder_Click(object sender, RoutedEventArgs e)
		{
			var security = (Security)this.SecuritiesDetails.SelectedValue;

			var newOrder = new NewOrderWindow { Title = "Новая заявка на '{0}'".Put(security.Code), Security = security };
			newOrder.ShowModal(this);
		}

		private void SecuritiesDetails_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			this.NewStopOrder.IsEnabled = this.NewOrder.IsEnabled =
			this.Quotes.IsEnabled = this.SecuritiesDetails.SelectedIndex != -1;
		}

		private void NewStopOrder_Click(object sender, RoutedEventArgs e)
		{
			var security = (Security)this.SecuritiesDetails.SelectedValue;

			var newOrder = new NewStopOrderWindow
			{
				Title = "Новая заявка на '{0}'".Put(security.Code),
				Security = security,
			};
			newOrder.ShowModal(this);
		}

		private void Quotes_Click(object sender, RoutedEventArgs e)
		{
			var window = _quotesWindows.SafeAdd((Security)this.SecuritiesDetails.SelectedValue, security =>
			{
				// начинаем получать котировки стакана
				MainWindow.Instance.Trader.RegisterQuotes(security);

				// создаем окно со стаканом
				return new QuotesWindow { Title = security.Code + " котировки" };
			});

			if (window.Visibility == Visibility.Visible)
				window.Hide();
			else
				window.Show();
		}
	}
}