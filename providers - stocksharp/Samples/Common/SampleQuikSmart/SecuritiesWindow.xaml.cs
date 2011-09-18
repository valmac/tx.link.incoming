namespace SampleQuikSmart
{
	using System;
	using System.Collections.Generic;
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
		private readonly List<Security> _bidAskSecurities = new List<Security>();
		private readonly List<Security> _securities = new List<Security>();
		private readonly List<string> _securitiesClasses = new List<string>();

		public SecuritiesWindow()
		{
			this.Securities = new ObservableCollection<Security>();
			InitializeComponent();

			this.SecurityTypes.SetDataSource<SecurityTypes>();

			_timer = TimeSpan.FromSeconds(1).CreateTimer(() => _quotesWindows.SyncDo(d =>
			{
				foreach (var pair in d)
				{
					pair.Value.Quotes.Clear();
					pair.Value.Quotes.AddRange(MainWindow.Instance.Trader.GetMarketDepth(pair.Key));
				}
			}));
		}

		public bool RealClose { get; set; }

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

				var trader = MainWindow.Instance.Trader;
				if (trader != null)
				{
					_quotesWindows.SyncDo(d =>
					{
						foreach (var pair in d)
						{
							trader.UnRegisterQuotes(pair.Key);

							pair.Value.RealClose = true;
							pair.Value.Close();
						}
					});

					_bidAskSecurities.ForEach(s =>
					{
						trader.UnRegisterSecurity(s);
						trader.UnRegisterTrades(s);
					});
				}
			}

			base.OnClosing(e);
		}

		public ObservableCollection<Security> Securities { get; set; }

		public void AddSecurities(IEnumerable<Security> securities)
		{
			if (securities == null)
				throw new ArgumentNullException("securities");

			foreach (var secClass in securities.GroupBy(s => s.Class).Select(g => g.Key))
				_securitiesClasses.TryAdd(secClass);

			this.SecurityClasses.ItemsSource = _securitiesClasses.ToArray();
			_securities.AddRange(securities);
		}

		public Security SelectedSecurity
		{
			get { return (Security)this.SecuritiesDetails.SelectedValue; }
		}

		private void NewOrder_Click(object sender, RoutedEventArgs e)
		{
			var security = this.SelectedSecurity;

			var newOrder = new NewOrderWindow { Title = "Новая заявка на '{0}'".Put(security.Code), Security = security };
			newOrder.ShowModal(this);
		}

		private void SecuritiesDetails_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			this.BidAsk.IsEnabled = this.NewOrder.IsEnabled = this.Quotes.IsEnabled = this.SelectedSecurity != null;
		}

		private void Quotes_Click(object sender, RoutedEventArgs e)
		{
			var window = _quotesWindows.SafeAdd(this.SelectedSecurity, security =>
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

		private void BidAsk_Click(object sender, RoutedEventArgs e)
		{
			var security = this.SelectedSecurity;
			var trader = MainWindow.Instance.Trader;

			if (_bidAskSecurities.Contains(security))
			{
				// останавливаем обновления по инструменту
				trader.UnRegisterSecurity(security);
				trader.UnRegisterTrades(security);

				_bidAskSecurities.Remove(security);
			}
			else
			{
				// начинаем получать обновления по инструменту
				trader.RegisterSecurity(security);
				trader.RegisterTrades(security);

				_bidAskSecurities.Add(security);
			}
		}

		private void FilterSecurities()
		{
			this.Securities.Clear();

			if (this.SecurityTypes.SelectedIndex != -1 && this.SecurityClasses.SelectedIndex != -1)
			{
				var secType = this.SecurityTypes.GetSelectedValue<SecurityTypes>();
				var secClass = (string)this.SecurityClasses.SelectedValue;
				var secName = this.SecurityName.Text.ToLowerInvariant();

				this.Securities.AddRange(_securities.Where(s =>
					s.Type == secType &&
					s.Class == secClass &&
					(secName.IsEmpty() || s.Code.ToLowerInvariant().Contains(secName) || s.Name.ToLowerInvariant().Contains(secName))));
			}
		}

		private void SecurityTypes_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			FilterSecurities();
		}

		private void SecurityClasses_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			FilterSecurities();
		}

		private void SecurityName_TextChanged(object sender, TextChangedEventArgs e)
		{
			FilterSecurities();
		}
	}
}