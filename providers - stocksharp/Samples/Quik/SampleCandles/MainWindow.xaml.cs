namespace SampleCandles
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

	using StockSharp.BusinessEntities;
	using StockSharp.Quik;
	using StockSharp.Algo.Candles;

	partial class MainWindow
	{
		private readonly Dictionary<CandleToken, ChartWindow> _chartWindows = new Dictionary<CandleToken, ChartWindow>();
		private QuikTrader _trader;
		private bool _isDdeStarted;
		private CandleManager _candleManager;

		public MainWindow()
		{
			InitializeComponent();
			this.CandleType.SetDataSource<CandleTypes>();
			this.CandleType.SetSelectedValue<CandleTypes>(CandleTypes.TimeFrame);

			this.TimeFrame.SelectedTime = TimeSpan.FromMinutes(5);

			// попробовать сразу найти месторасположение Quik по запущенному процессу
			this.Path.Text = QuikTerminal.GetDefaultPath();
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
				MessageBox.Show(this, "Путь к Quik не выбран.");
			else
			{
				_trader = new QuikTrader(this.Path.Text);
				
				_trader.Connected += () => this.GuiAsync(() => this.ExportDde.IsEnabled = true);
				_trader.NewSecurities += securities => this.GuiAsync(() => this.Security.ItemsSource = _trader.Securities);

				_trader.Connect();

				_candleManager = new CandleManager(_trader);
				_candleManager.NewCandles += DrawCandles;
				_candleManager.CandlesChanged += DrawCandles;

				this.ConnectBtn.IsEnabled = false;
			}
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

		protected override void OnClosing(CancelEventArgs e)
		{
			foreach (var pair in _chartWindows)
				pair.Value.RealClose = true;

			if (_trader != null)
			{
				if (_isDdeStarted)
					StopDde();

				_trader.Dispose();
			}

			base.OnClosing(e);
		}

		private void StartDde()
		{
			_trader.StartExport(_trader.SecuritiesTable, _trader.TradesTable);
			_isDdeStarted = true;
		}

		private void StopDde()
		{
			_trader.StopExport(_trader.SecuritiesTable, _trader.TradesTable);
			_isDdeStarted = false;
		}

		private void ExportDde_Click(object sender, RoutedEventArgs e)
		{
			if (_isDdeStarted)
				StopDde();
			else
				StartDde();
		}

		private CandleTypes SelectedCandleType
		{
			get { return this.CandleType.GetSelectedValue<CandleTypes>().Value; }
		}

		private Security SelectedSecurity
		{
			get { return (Security)this.Security.SelectedValue; }
		}

		private void ShowChart_Click(object sender, RoutedEventArgs e)
		{
			var type = this.SelectedCandleType;
			var security = this.SelectedSecurity;

			CandleToken token;

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

			_chartWindows.SafeAdd(token, key => new ChartWindow { Title = "{0} {1} {2}".Put(security.Code, type, token.Arg) }).Show();
		}

		private void Security_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			var security = this.SelectedSecurity;

			if (security != null)
			{
				this.ShowChart.IsEnabled = true;
				this.PriceRange.Security = security;
				this.PriceRange.Unit = 0.5.Percents();
			}
		}

		private void CandleTypes_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			var type = this.SelectedCandleType;
			this.TimeFrame.IsEnabled = type == CandleTypes.TimeFrame;
			this.PriceRange.IsEnabled = type == CandleTypes.Range;
			this.VolumeCtrl.IsEnabled = (!this.TimeFrame.IsEnabled && !this.PriceRange.IsEnabled);
		}
	}
}