using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Windows;
using System.Windows.Controls;
using System.Diagnostics;

using Ecng.Common;
using Ecng.Collections;
using Ecng.ComponentModel;
using Ecng.Xaml;

using StockSharp.Algo.Candles;
using StockSharp.BusinessEntities;
using StockSharp.AlfaDirect;

namespace SampleAlfaCandles
{
	partial class MainWindow
	{
		private readonly Dictionary<CandleToken, ChartWindow> _chartWindows = new Dictionary<CandleToken, ChartWindow>();
		private AlfaTrader _trader;
		private bool _isExportStarted;
		private readonly ObservableCollection<Security> _securitiesSource = new ObservableCollection<Security>();

		public MainWindow()
		{
			InitializeComponent();

			var items = HistoryInterval.Items;
			items.Add(new ComboBoxItem { Content = "1 минута", Tag = AlfaTimeFrames.Minute1 });
			items.Add(new ComboBoxItem { Content = "5 минут", Tag = AlfaTimeFrames.Minute5 });
			items.Add(new ComboBoxItem { Content = "10 минут", Tag = AlfaTimeFrames.Minute10 });
			items.Add(new ComboBoxItem { Content = "15 минут", Tag = AlfaTimeFrames.Minute15 });
			items.Add(new ComboBoxItem { Content = "30 минут", Tag = AlfaTimeFrames.Minute30 });
			items.Add(new ComboBoxItem { Content = "1 час", Tag = AlfaTimeFrames.Hour });
			items.Add(new ComboBoxItem { Content = "День", Tag = AlfaTimeFrames.Day });
			items.Add(new ComboBoxItem { Content = "Неделя", Tag = AlfaTimeFrames.Week });
			items.Add(new ComboBoxItem { Content = "Месяц", Tag = AlfaTimeFrames.Month });
			items.Add(new ComboBoxItem { Content = "Год", Tag = AlfaTimeFrames.Year });

			HistoryInterval.SelectedIndex = 2;
			From.SelectedDate = DateTime.Today - TimeSpan.FromDays(7);
			To.SelectedDate = DateTime.Now;

			Security.ItemsSource = _securitiesSource;

			Logger.Instance().Updated += UpdateLog;
		}

		private static void UpdateLog()
		{
			var entries = Logger.Instance().NewEntries;

			if (entries.Count == 0)
				return;

			foreach (var entry in entries)
			{
				var text = String.Format("{0:HH:mm:ss} | {1,-5} | {2,-15} | {3}",
					DateTime.Now, entry.Level, entry.Source, entry.Message);

				Trace.WriteLine(text);
				Trace.Flush();
			}
		}

		private void ConnectClick(object sender, RoutedEventArgs e)
		{
			// создаем шлюз
			_trader = new AlfaTrader();

			_trader.ProcessDataError += error => this.GuiAsync(() => MessageBox.Show(this, error.ToString(), "Ошибка обработки данных"));
			_trader.NewSecurities += securities =>
			{
				this.GuiAsync(() => _securitiesSource.AddRange(securities));

				// начинаем получать текущие сделки (для построения свечек в реальном времени)
				securities.ForEach(_trader.RegisterTrades);
			};
			_trader.NewPortfolios += portfolios => portfolios.ForEach(_trader.RegisterPortfolio);
			_trader.Connected += () => this.GuiAsync(() => Export.IsEnabled = true);

            _trader.Connect();
			ConnectBtn.IsEnabled = false;
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

		private void ExportClick(object sender, RoutedEventArgs e)
		{
			if (_isExportStarted)
				StopExport();
			else
				StartExport();
		}

		private Security SelectedSecurity
		{
			get { return (Security)Security.SelectedValue; }
		}

		private void ShowChartClick(object sender, RoutedEventArgs e)
		{
			var security = SelectedSecurity;

			var timeFrame = (AlfaTimeFrames)((ComboBoxItem)HistoryInterval.SelectedItem).Tag;

			var from = (DateTime) From.SelectedDate;
			var to = (DateTime) To.SelectedDate;

			var candles = _trader.GetHistoryData(security, timeFrame, new Range<DateTime>(from, to));

			var wnd = new ChartWindow
			{
			    Title = "{0}, {1}, {2} - {3}".Put(security.Code, 
												((ComboBoxItem)HistoryInterval.SelectedItem).Content,
												from.ToShortDateString(), to.ToShortDateString())
			};

			this.GuiAsync(() => wnd.DrawCandles(candles));

			wnd.Show();
		}

		private void SecuritySelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			ShowChart.IsEnabled = SelectedSecurity != null;
		}
	}
}