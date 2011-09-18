namespace SampleSmartCandles
{
	using System;
	using System.Collections.Generic;
	using System.Collections.ObjectModel;
	using System.ComponentModel;
	using System.Linq;
	using System.Windows;
	using System.Windows.Controls;
	using MessageBox = System.Windows.MessageBox;

	using Ecng.Common;
	using Ecng.Collections;
	using Ecng.ComponentModel;
	using Ecng.Xaml;

	using StockSharp.Algo.Candles;
	using StockSharp.BusinessEntities;
	using StockSharp.Smart;

	partial class MainWindow
	{
		private readonly Dictionary<CandleToken, ChartWindow> _chartWindows = new Dictionary<CandleToken, ChartWindow>();
		private SmartTrader _trader;
		private CandleManager _candleManager;
		private bool _isExportStarted;
		private readonly ObservableCollection<Security> _securitiesSource = new ObservableCollection<Security>();

		public MainWindow()
		{
			InitializeComponent();
			this.CandleType.SetDataSource<CandleTypes>();
			this.CandleType.SetSelectedValue<CandleTypes>(CandleTypes.TimeFrame);
			this.TimeFrame.SelectedTime = TimeSpan.FromMinutes(5);

			var items = this.HistoryInterval.Items;
			items.Add(new ComboBoxItem { Content = "1 минута", Tag = SmartTimeFrames.Minute1 });
			items.Add(new ComboBoxItem { Content = "5 минут", Tag = SmartTimeFrames.Minute5 });
			items.Add(new ComboBoxItem { Content = "10 минут", Tag = SmartTimeFrames.Minute10 });
			items.Add(new ComboBoxItem { Content = "15 минут", Tag = SmartTimeFrames.Minute15 });
			items.Add(new ComboBoxItem { Content = "30 минут", Tag = SmartTimeFrames.Minute30 });
			items.Add(new ComboBoxItem { Content = "1 час", Tag = SmartTimeFrames.Hour1 });
			items.Add(new ComboBoxItem { Content = "2 часа", Tag = SmartTimeFrames.Hour2 });
			items.Add(new ComboBoxItem { Content = "4 часа", Tag = SmartTimeFrames.Hour4 });
			items.Add(new ComboBoxItem { Content = "День", Tag = SmartTimeFrames.Day });
			items.Add(new ComboBoxItem { Content = "Неделя", Tag = SmartTimeFrames.Week });
			items.Add(new ComboBoxItem { Content = "Месяц", Tag = SmartTimeFrames.Month1 });
			items.Add(new ComboBoxItem { Content = "Квартал", Tag = SmartTimeFrames.Month3 });
			items.Add(new ComboBoxItem { Content = "Год", Tag = SmartTimeFrames.Year });

			this.HistoryInterval.SelectedIndex = 2;
			this.From.SelectedDate = DateTime.Today - TimeSpan.FromDays(7);
			this.To.SelectedDate = DateTime.Now;

			this.Security.ItemsSource = _securitiesSource;
		}

		private void Connect_Click(object sender, RoutedEventArgs e)
		{
			if (this.Login.Text.IsEmpty())
			{
				MessageBox.Show(this, "Не указан логин.");
				return;
			}
			else if (this.Password.Password.IsEmpty())
			{
				MessageBox.Show(this, "Не указан пароль.");
				return;
			}

			// создаем шлюз
			_trader = new SmartTrader(this.Login.Text, this.Password.Password, this.Address.SelectedAddress);

			// очищаем из текстового поля в целях безопасности
			this.Password.Clear();

			_trader.ProcessDataError += error => this.GuiAsync(() => MessageBox.Show(this, error.ToString(), "Ошибка обработки данных"));
			_trader.NewSecurities += securities =>
			{
				// так как инструментов слишком много, то выводим только два популярных с ММВБ и РТС
				securities = securities.Where(s => s.Id == "LKOH@EQBR" || s.Id == "RIM1@RTS");

				this.GuiAsync(() => _securitiesSource.AddRange(securities));

				// начинаем получать текущие сделки (для построения свечек в реальном времени)
				securities.ForEach(_trader.RegisterTrades);
			};
			_trader.NewPortfolios += portfolios => portfolios.ForEach(_trader.RegisterPortfolio);
			_trader.Connected += () => this.GuiAsync(() => this.Export.IsEnabled = true);

			_trader.NewHistoryCandles += NewHistoryCandles;

			_candleManager = new CandleManager(_trader);
			_candleManager.NewCandles += DrawCandles;
			_candleManager.CandlesChanged += DrawCandles;

            _trader.Connect();
			this.ConnectBtn.IsEnabled = false;
		}

		private void NewHistoryCandles(CandleToken token, IEnumerable<TimeFrameCandle> candles)
		{
			DrawCandles(token, candles.Cast<Candle>());
		}

		private void DrawCandles(CandleToken token, IEnumerable<Candle> candles)
		{
			var wnd = _chartWindows.TryGetValue(token);
			if (wnd != null)
				this.GuiAsync(() => wnd.DrawCandles(candles));
		}

		protected override void OnClosing(CancelEventArgs e)
		{
			foreach (var pair in _chartWindows)
				pair.Value.RealClose = true;

			if (_trader != null)
			{
				if (_isExportStarted)
					StopExport();

				_trader.Dispose();
			}

			base.OnClosing(e);
		}

		private void StartExport()
		{
			_isExportStarted = true;
			_trader.StartExport();
		}

		private void StopExport()
		{
			_trader.StopExport();
			_isExportStarted = false;
		}

		private void Export_Click(object sender, RoutedEventArgs e)
		{
			if (_isExportStarted)
				StopExport();
			else
				StartExport();
		}

		private Security SelectedSecurity
		{
			get { return (Security)this.Security.SelectedValue; }
		}

		private void ShowChart_Click(object sender, RoutedEventArgs e)
		{
			var security = this.SelectedSecurity;
			CandleToken token;

			if (this.IsRealTime.IsChecked == true)
			{
				var type = this.CandleType.GetSelectedValue<CandleTypes>().Value;

				switch (type)
				{
					case CandleTypes.TimeFrame:
						token = _candleManager.RegisterTimeFrameCandles(security, this.TimeFrame.SelectedTime);
						break;
					case CandleTypes.Tick:
						token = _candleManager.RegisterTickCandles(security, this.VolumeCtrl.Text.To<int>());
						break;
					case CandleTypes.Volume:
						token = _candleManager.RegisterVolumeCandles(security, this.VolumeCtrl.Text.To<int>());
						break;
					case CandleTypes.Range:
						token = _candleManager.RegisterRangeCandles(security, this.PriceRange.Unit);
						break;
					default:
						throw new ArgumentOutOfRangeException();
				}
			}
			else
			{
				var timeFrame = (SmartTimeFrames)((ComboBoxItem)this.HistoryInterval.SelectedItem).Tag;
				token = _trader.RegisterHistoryCandles(security, timeFrame, new Range<DateTime>((DateTime)this.From.SelectedDate, (DateTime)this.To.SelectedDate));
			}

			var arg = token.Arg;

			if (arg is SmartTimeFrames)
				arg = ((ComboBoxItem)this.HistoryInterval.SelectedItem).Content;

			_chartWindows.SafeAdd(token, key => new ChartWindow
			{
				Title = "{0} {1} {2}".Put(security.Code, token.CandleType.Name.Replace("Candle", string.Empty), arg)
			}).Show();
		}

		private void Security_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			this.ShowChart.IsEnabled = this.SelectedSecurity != null;
		}

		private void CandleTypes_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			var type = this.CandleType.GetSelectedValue<CandleTypes>().Value;

			this.TimeFrame.SetVisibility(type == CandleTypes.TimeFrame);
			this.PriceRange.SetVisibility(type == CandleTypes.Range);
			this.VolumeCtrl.SetVisibility(type == CandleTypes.Tick || type == CandleTypes.Volume);
		}

		private void OnChartTypeChanged(object sender, RoutedEventArgs e)
		{
			this.RealTimeSettings.IsEnabled = this.IsRealTime.IsChecked == true;
			this.HistorySettings.IsEnabled = this.IsHistory.IsChecked == true;
		}
	}
}