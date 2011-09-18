namespace SampleSync
{
	using System;
	using System.Collections.Generic;
	using System.ComponentModel;
	using System.Windows;
	using System.Windows.Controls;
	using System.Windows.Forms;
	using MessageBox = System.Windows.MessageBox;

	using Ecng.Common;
	using Ecng.Collections;
	using Ecng.Xaml;

	using StockSharp.Xaml;
	using StockSharp.Algo.Candles;
	using StockSharp.BusinessEntities;
	using StockSharp.Quik;

	public partial class MainWindow
	{
		private readonly Dictionary<CandleToken, ChartWindow> _chartWindows = new Dictionary<CandleToken, ChartWindow>();
		private GuiTrader<QuikTrader> _trader;
		private bool _isDdeStarted;
		private GuiCandleManager<CandleManager> _candleManager;

		public MainWindow()
		{
			InitializeComponent();

			// попробовать сразу найти месторасположение Quik по запущенному процессу
			this.Path.Text = QuikTerminal.GetDefaultPath();
		}

		protected override void OnClosing(CancelEventArgs e)
		{
			foreach (var pair in _chartWindows)
				pair.Value.RealClose = true;

			if (_trader != null)
			{
				if (_isDdeStarted)
					StopDde();

				_trader.Dispose();
				_trader.Trader.Dispose();
			}

			base.OnClosing(e);
		}

		private void FindPath_Click(object sender, RoutedEventArgs e)
		{
			var dlg = new FolderBrowserDialog();

			if (!this.Path.Text.IsEmpty())
				dlg.SelectedPath = this.Path.Text;

			if (dlg.ShowDialog() == System.Windows.Forms.DialogResult.OK)
			{
				this.Path.Text = dlg.SelectedPath;
			}
		}

		private void Connect_Click(object sender, RoutedEventArgs e)
		{
			if (this.Path.Text.IsEmpty())
				MessageBox.Show(this, "Путь к Quik не выбран");
			else
			{
				// создаем шлюз к Quik-у и синхронизуем его
				_trader = new QuikTrader(this.Path.Text).GuiSyncTrader();

				// или напрямую через конструктор GuiTrader
				// (пред. нужно закомментировать, это - раскомментировать)
				// new GuiTrader<QuikTrader>(new QuikTrader(this.Path.Text));

				_trader.Connected += () => this.ExportDde.IsEnabled = true;

				// теперь можно обратиться к элементу окна 'Security' (это выпадающий список) без конструкции Sync
				_trader.NewSecurities += securities => this.Security.ItemsSource = _trader.Securities;

				// производим соединение
				_trader.Connect();

				// создаем синхронизованный менеджер свечек по несинхронизованному шлюзу
				_candleManager = new CandleManager(_trader.Trader).GuiSyncCandleManager();

				// или напрямую через конструктор GuiCandleManager
				// (пред. нужно закомментировать, это - раскомментировать)
				// new GuiCandleManager<CandleManager>(new CandleManager(_trader.Trader));

				_candleManager.NewCandles += DrawCandles;
				_candleManager.CandlesChanged += DrawCandles;

				this.ConnectBtn.IsEnabled = false;
			}
		}

		private void ShowChart_Click(object sender, RoutedEventArgs e)
		{
			var security = (Security)this.Security.SelectedValue;
			var token = _candleManager.CandleManager.RegisterTimeFrameCandles(security, TimeSpan.FromMinutes(5));
			_chartWindows.SafeAdd(token, key => new ChartWindow { Title = "{0} {1}".Put(security.Code, token.Arg) }).Show();
		}

		private void DrawCandles(CandleToken token, IEnumerable<Candle> candles)
		{
			this.GuiAsync(() =>
			{
				var wnd = _chartWindows.TryGetValue(token);
				if (wnd != null)
					wnd.Chart.Candles.AddRange(candles);
			});
		}

		private void StartDde()
		{
			_trader.Trader.StartExport(_trader.Trader.SecuritiesTable, _trader.Trader.TradesTable);
			_isDdeStarted = true;
		}

		private void StopDde()
		{
			_trader.Trader.StopExport(_trader.Trader.SecuritiesTable, _trader.Trader.TradesTable);
			_isDdeStarted = false;
		}

		private void ExportDde_Click(object sender, RoutedEventArgs e)
		{
			if (_isDdeStarted)
				StopDde();
			else
				StartDde();
		}

		private void Security_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			if (this.Security.SelectedIndex != -1)
				this.ShowChart.IsEnabled = true;
		}
	}
}