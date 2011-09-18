namespace SampleSMA
{
	using System;
	using System.Collections.Generic;
	using System.Diagnostics;
	using System.Linq;
	using System.ComponentModel;
	using System.Globalization;
	using System.IO;
	using System.Threading;
	using System.Windows;
	using System.Windows.Forms;
	using MessageBox = System.Windows.MessageBox;

	using AmCharts.Windows.Stock;

	using Ecng.Collections;
	using Ecng.Common;
	using Ecng.Xaml;
	using Ecng.ComponentModel;

	using StockSharp.Algo.Candles;
	using StockSharp.Algo.Reporting;
	using StockSharp.Algo.Strategies;
	using StockSharp.BusinessEntities;
	using StockSharp.Quik;
	using StockSharp.Xaml;

	public partial class MainWindow
	{
		private readonly TimeSpan _timeFrame = TimeSpan.FromMinutes(5);
		private QuikTrader _trader;
		private SmaStrategy _strategy;
		private bool _isDdeStarted;
		private DateTime _lastCandleTime;
		private bool _isTodaySmaDrawn;
		private CandleManager _candleManager;
		private readonly ICollection<CustomChartIndicator> _longSmaGraph;
		private readonly ICollection<CustomChartIndicator> _shortSmaGraph;
		private Security _lkoh;

		public MainWindow()
		{
			InitializeComponent();

			// изменяет текущий формат, чтобы нецелое числа интерпритировалось как разделенное точкой.
			var cci = new CultureInfo(Thread.CurrentThread.CurrentCulture.Name) { NumberFormat = { NumberDecimalSeparator = "." } };
			Thread.CurrentThread.CurrentCulture = cci;

			_longSmaGraph = _chart.CreateTrend("Длинная", GraphType.Line);
			_shortSmaGraph = _chart.CreateTrend("Короткая", GraphType.Line);
		}

		private void _orders_OrderSelected(object sender, EventArgs e)
		{
			this.CancelOrders.IsEnabled = _orders.SelectedOrders.Count() > 0;
		}

		protected override void OnClosing(CancelEventArgs e)
		{
			if (_trader != null)
			{
				if (_isDdeStarted)
					StopDde();

				_trader.Dispose();
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
			if (_trader == null || !_trader.IsConnected)
			{
				if (_trader == null)
				{
					if (this.Path.Text.IsEmpty())
					{
						MessageBox.Show(this, "Путь к Quik не выбран.");
						return;
					}

					// создаем шлюз
					_trader = new QuikTrader(this.Path.Text);

					this.Portfolios.Trader = _trader;

					_trader.Connected += () =>
					{
						_candleManager = new CandleManager(_trader);

						_trader.NewSecurities += securities => this.GuiAsync(() =>
						{
							// находим нужную бумагу
							var lkoh = securities.FirstOrDefault(s => s.Code == "LKOH");

							if (lkoh != null)
							{
								_lkoh = lkoh;

								this.GuiAsync(() =>
								{
									this.Start.IsEnabled = true;
								});
							}
						});

						_trader.NewMyTrades += trades => this.GuiAsync(() =>
						{
							if (_strategy != null)
							{
								// найти те сделки, которые совершила стратегия скользящей средней
								trades = trades.Where(t => _strategy.Orders.Any(o => o == t.Order));

								_trades.Trades.AddRange(trades);
							}
						});

						_candleManager.NewCandles += (token, candles) =>
						{
							DrawCandles(candles);

							// если скользящие за сегодняшний день отрисованы, то рисуем в реальном времени текущие скользящие
							if (_isTodaySmaDrawn)
								DrawSma();
						};
						_candleManager.CandlesChanged += (token, candles) => DrawCandles(candles);
						//_trader.ProcessDataError += ex => this.Sync(() => MessageBox.Show(this, ex.ToString()));
						_trader.ConnectionError += ex =>
						{
							if (ex != null)
								this.GuiAsync(() => MessageBox.Show(this, ex.ToString()));
						};

						this.GuiAsync(() =>
						{
							this.ConnectBtn.IsEnabled = false;
							this.ExportDde.IsEnabled = true;
							this.Report.IsEnabled = true;
						});
					};
				}

				_trader.Connect();
			}
			else
				_trader.Disconnect();
		}

		private void OnNewOrder(Order order)
		{
			_orders.Orders.Add(order);
			this.GuiAsync(() => _chart.Orders.Add(order));
		}

		private void OnLog(Strategy strategy, StrategyErrorStates errorState, string message)
		{
			// если стратегия вывела не просто сообщение, то вывести на экран.
			if (errorState != StrategyErrorStates.None)
				this.GuiAsync(() => MessageBox.Show(this, message));
		}

		private void DrawCandles(IEnumerable<Candle> candles)
		{
			this.GuiAsync(() => _chart.Candles.AddRange(candles));
		}

		private void DrawSma()
		{
			// нас не интересует текущая свечка, так как она еще не сформировалась
			// и из нее нельзя брать цену закрытия

			// вычисляем временные отрезки текущей свечки
			var bounds = _timeFrame.GetCandleBounds(_trader);

			// если появились новые полностью сформированные свечки
			if ((_lastCandleTime + _timeFrame) < bounds.Min)
			{
				// отступ с конца интервала, чтобы не захватить текущую свечку.
				var endOffset = TimeSpan.FromSeconds(1);

				bounds = new Range<DateTime>(_lastCandleTime + _timeFrame, bounds.Min - endOffset);

				// получаем эти свечки
				var candles = _candleManager.GetTimeFrameCandles(_strategy.Security, _timeFrame, bounds);

				if (candles.Count() > 0)
				{
					// получаем время самой последней свечки и запоминаем его как новое начало
					_lastCandleTime = candles.Max(c => c.Time);

					DrawSmaLines(bounds.Min);
				}
			}
		}

		private void DrawSmaLines(DateTime time)
		{
			this.GuiSync(() =>
			{
				_longSmaGraph.Add(new CustomChartIndicator
				{
					Time = time,
					Value = (double)_strategy.LongSma.Value
				});
				_shortSmaGraph.Add(new CustomChartIndicator
				{
					Time = time,
					Value = (double)_strategy.ShortSma.Value
				});
			});
		}

		private void OnStrategyPropertyChanged(object sender, PropertyChangedEventArgs e)
		{
			this.GuiAsync(() =>
			{
				this.Status.Content = _strategy.ProcessState;
				this.PnL.Content = _strategy.PnLManager.PnL;
				this.Slippage.Content = _strategy.SlippageManager.Slippage;
				this.Position.Content = _strategy.PositionManager.Position;
				this.Latency.Content = _strategy.LatencyManager.Latency;
			});
		}

		private void StartDde()
		{
			_trader.StartExport();
			_isDdeStarted = true;
		}

		private void StopDde()
		{
			_trader.StopExport();
			_isDdeStarted = false;
		}

		private void ExportDde_Click(object sender, RoutedEventArgs e)
		{
			if (_isDdeStarted)
				StopDde();
			else
				StartDde();
		}

		private void CancelOrders_Click(object sender, RoutedEventArgs e)
		{
			_orders.SelectedOrders.ForEach(_trader.CancelOrder);
		}

		private void Start_Click(object sender, RoutedEventArgs e)
		{
			if (_strategy == null)
			{
				if (this.Portfolios.SelectedPortfolio == null)
				{
					MessageBox.Show(this, "Портфель не выбран.");
					return;
				}

				var candles = File.ReadAllLines("LKOH_history.txt").Select(line =>
				{
					var parts = line.Split(',');
					var time = DateTime.ParseExact(parts[0] + parts[1], "yyyyMMddHHmmss", CultureInfo.InvariantCulture);
					return new TimeFrameCandle
					{
						OpenPrice = parts[2].To<decimal>(),
						HighPrice = parts[3].To<decimal>(),
						LowPrice = parts[4].To<decimal>(),
						ClosePrice = parts[5].To<decimal>(),
						TimeFrame = _timeFrame,
						Time = time,
						TotalVolume = parts[6].To<int>(),
						Security = _lkoh,
					};
				});

				DrawCandles(candles.Cast<Candle>());

				// создаем торговую стратегию, скользящие средние на 80 5-минуток и 10 5-минуток
				_strategy = new SmaStrategy(_candleManager, new Sma(80), new Sma(10), _timeFrame)
				{
					Volume = 1,
					Security = _lkoh,
					Portfolio = this.Portfolios.SelectedPortfolio,
					Trader = _trader,
				};
				_strategy.Log += OnLog;
				_strategy.NewOrder += OnNewOrder;
				_strategy.PropertyChanged += OnStrategyPropertyChanged;

				var index = 0;

				// начинаем вычислять скользящие средние
				foreach (var candle in candles)
				{
					_strategy.LongSma.Add(candle);
					_strategy.ShortSma.Add(candle);

					// если все скользящие сформировались, то начинаем их отрисовывать
					if (index >= _strategy.LongSma.Length)
						DrawSmaLines(candle.Time);

					index++;

					_lastCandleTime = candle.Time;
				}

				// регистрируем наш тайм-фрейм
				_candleManager.RegisterTimeFrameCandles(_lkoh, _timeFrame);

				// вычисляем временные отрезки текущей свечки
				var bounds = _timeFrame.GetCandleBounds(_trader);

				candles = _candleManager.GetTimeFrameCandles(_strategy.Security, _timeFrame, new Range<DateTime>(_lastCandleTime + _timeFrame, bounds.Min));

				foreach (var candle in candles)
				{
					_strategy.LongSma.Add(candle);
					_strategy.ShortSma.Add(candle);

					DrawSmaLines(candle.Time);

					_lastCandleTime = candle.Time;
				}

				_isTodaySmaDrawn = true;

				this.Report.IsEnabled = true;
			}

			if (_strategy.ProcessState == StrategyProcessStates.Stopped)
			{
				// запускаем процесс получения стакана, необходимый для работы алгоритма котирования
				_trader.RegisterQuotes(_strategy.Security);
				_strategy.Start();
				this.Start.Content = "Стоп";
			}
			else
			{
				_trader.UnRegisterQuotes(_strategy.Security);
				_strategy.Stop();
				this.Start.Content = "Старт";
			}
		}

		private void Report_Click(object sender, RoutedEventArgs e)
		{
			// сгерерировать отчет по прошедшему тестированию
			new ExcelStrategyReport(_strategy, "sma.xlsx").Generate();

			// открыть отчет
			Process.Start("sma.xlsx");
		}
	}
}