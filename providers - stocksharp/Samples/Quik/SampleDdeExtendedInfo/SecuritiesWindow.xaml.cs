namespace SampleDdeExtendedInfo
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
				foreach (var pair in d)
				{
					var quotes = MainWindow.Instance.Trader.GetMarketDepth(pair.Key).Select(q => new SampleQuote(q)).ToArray();

					var wnd = pair.Value;

					wnd.GuiAsync(() =>
					{
						wnd.Quotes.Clear();
						wnd.Quotes.AddRange(quotes);
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

		private void SecuritiesDetails_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			this.Quotes.IsEnabled = this.SecuritiesDetails.SelectedIndex != -1;
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