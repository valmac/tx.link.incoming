namespace SampleRealTimeTesting
{
	using System;
	using System.Collections.Generic;
	using System.ComponentModel;
	using System.Diagnostics;
	using System.Linq;
	using System.Windows;

	using AmCharts.Windows.Stock;

	using Ecng.Collections;
	using Ecng.Common;
	using Ecng.ComponentModel;
	using Ecng.Xaml;

	using StockSharp.Algo.Candles;
	using StockSharp.Algo.Reporting;
	using StockSharp.Algo.Strategies;
	using StockSharp.Algo.Testing;
	using StockSharp.BusinessEntities;
	using StockSharp.Smart;
	using StockSharp.Xaml;

	public partial class MainWindow
	{
		private bool _isConnected;
		private readonly TimeSpan _timeFrame = (TimeSpan)SmartTimeFrames.Minute5;
		private readonly SynchronizedList<TimeFrameCandle> _historyCandles = new SynchronizedList<TimeFrameCandle>();
		private CandleManager _candleManager;
		private RealTimeEmulationTrader<SmartTrader> _trader;
		private SmaStrategy _strategy;
		private readonly ICollection<CustomChartIndicator> _longSmaGraph;
		private readonly ICollection<CustomChartIndicator> _shortEmaGraph;
		private DateTime _lastHistoryCandle;
		private DateTime _lastCandleTime;
		private Security _lkoh;

		public MainWindow()
		{
			InitializeComponent();

			_longSmaGraph = _chart.CreateTrend("Длинная", GraphType.Line);
			_shortEmaGraph = _chart.CreateTrend("Короткая", GraphType.Line);
		}

		protected override void OnClosing(CancelEventArgs e)
		{
			if (_trader != null)
				_trader.Dispose();

			base.OnClosing(e);
		}

		private void Connect_Click(object sender, RoutedEventArgs e)
		{
			if (!_isConnected)
			{
				if (_trader == null)
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
					_trader = new RealTimeEmulationTrader<SmartTrader>(new SmartTrader(this.Login.Text, this.Password.Password, this.Address.SelectedAddress));

					this.Portfolios.Trader = _trader;

					// очищаем из текстового поля в целях безопасности
					this.Password.Clear();

					// подписываемся на событие успешного соединения
					_trader.Connected += () =>
					{
						// возводим флаг, что соединение установлено
						_isConnected = true;

						// разблокируем кнопку Экспорт
						this.GuiAsync(() => ChangeConnectStatus(true));

						_candleManager = new CandleManager(_trader);

						_trader.Trader.NewHistoryCandles += (token, candles) => _historyCandles.SyncDo(col =>
						{
							_historyCandles.AddRange(candles);

							DrawCandles(candles.Cast<Candle>());

							foreach (var c in candles)
							{
								var candle = c;

								_strategy.ShortMa.Add(candle);
								_strategy.LongMa.Add(candle);

								// если было собрано достаточно истории, начинаем отрисовывать скользящие
								if (_historyCandles.Count >= _strategy.LongMa.Length)
								{
									this.GuiSync(() =>
									{
										DrawMaLines(candle.Time);
										this.Start.IsEnabled = true;
									});
								}
							}
						});

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

						// подписываемся на событие о неудачной регистрации заявок
						_trader.OrdersFailed += OrdersFailed;

						_candleManager.NewCandles += (t, candlesLocal) =>
						{
							DrawRealTimeCandles(t, candlesLocal);

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

									this.GuiSync(() => DrawMaLines(bounds.Min));
								}
							}
						};
						_candleManager.CandlesChanged += DrawRealTimeCandles;

						_trader.StartExport();

						this.GuiAsync(() =>
						{
							this.ConnectBtn.IsEnabled = false;
						});
					};

					// подписываемся на событие разрыва соединения
					_trader.ConnectionError += error => this.GuiAsync(() =>
					{
						// заблокируем кнопку Экспорт (так как соединение было потеряно)
						ChangeConnectStatus(false);

						MessageBox.Show(this, error.ToString(), "Ошибка соединения");
					});

					_trader.ProcessDataError += error => this.GuiAsync(() => MessageBox.Show(this, error.ToString(), "Ошибка обработки данных"));
				}

				_trader.Connect();
			}
			else
			{
				_trader.Disconnect();
			}
		}

		private void OnNewOrder(Order order)
		{
			_orders.Orders.Add(order);
			this.GuiAsync(() => _chart.Orders.Add(order));
		}

		private void OrdersFailed(IEnumerable<OrderFail> fails)
		{
			this.GuiAsync(() =>
			{
				foreach (var fail in fails)
					MessageBox.Show(this, fail.Error.ToString(), "Ошибка регистрации заявки");
			});
		}

		private void ChangeConnectStatus(bool isConnected)
		{
			_isConnected = isConnected;
			this.ConnectBtn.Content = isConnected ? "Отключиться" : "Подключиться";
		}

		private void OnLog(Strategy strategy, StrategyErrorStates errorState, string message)
		{
			// если стратегия вывела не просто сообщение, то вывести на экран.
			if (errorState != StrategyErrorStates.None)
				this.GuiAsync(() => MessageBox.Show(this, message));
		}

		private void DrawRealTimeCandles(CandleToken token, IEnumerable<Candle> candles)
		{
			// выводим только те свечки, которые не были отрисованы как исторические
			DrawCandles(candles.Where(c => c.Time > _lastHistoryCandle));
		}

		private void DrawCandles(IEnumerable<Candle> candles)
		{
			this.GuiAsync(() => _chart.Candles.AddRange(candles));
		}

		private void DrawMaLines(DateTime time)
		{
			_lastCandleTime = time;
			_longSmaGraph.Add(new CustomChartIndicator { Time = time, Value = (double)_strategy.LongMa.Value });
			_shortEmaGraph.Add(new CustomChartIndicator { Time = time, Value = (double)_strategy.ShortMa.Value });
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

		private void Start_Click(object sender, RoutedEventArgs e)
		{
			// если были получены и инструмент, и портфель
			if (_strategy == null)
			{
				if (this.Portfolios.SelectedPortfolio == null)
				{
					MessageBox.Show(this, "Портфель не выбран.");
					return;
				}

				// создаем скользящие средние, на 80 5-минуток и 10 5-минуток
				var longSma = new Sma(80);
				var shortSma = new Ema(10);

				// создаем торговую стратегию
				_strategy = new SmaStrategy(_candleManager, longSma, shortSma, _timeFrame)
				{
					Volume = 1,
					Security = _lkoh,
					Portfolio = this.Portfolios.SelectedPortfolio,
					Trader = _trader,
				};
				_strategy.Log += OnLog;
				_strategy.NewOrder += OnNewOrder;
				_strategy.PropertyChanged += OnStrategyPropertyChanged;

				// начинаем получать текущие сделки (для построения свечек реального времени)
				_trader.RegisterTrades(_lkoh);

				// регистрируем запрос в SmartCOM для получения исторических данных по 5-минуткам для Лукойла за период в 5 дней
				_trader.Trader.RegisterHistoryCandles(_lkoh, SmartTimeFrames.Minute5,
									 new Range<DateTime>(DateTime.Today - TimeSpan.FromDays(5), _trader.MarketTime));

				_lastHistoryCandle = _timeFrame.GetCandleBounds(_trader).Min;

				// регистрируем наш тайм-фрейм
				_candleManager.RegisterTimeFrameCandles(_lkoh, _timeFrame);

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