namespace SampleEmulationTesting
{
	using System;
	using System.Collections.Generic;
	using System.Diagnostics;
	using System.Windows;
	using System.Windows.Media;

	using Ecng.Xaml;
	using Ecng.Collections;

	using StockSharp.Algo.Candles;
	using StockSharp.Algo.Equity;
	using StockSharp.Algo.Logging;
	using StockSharp.Algo.Reporting;
	using StockSharp.Algo.Testing;
	using StockSharp.BusinessEntities;

	public partial class MainWindow
	{
		private SmaStrategy _strategy;

		private ICollection<EquityData> _curveItems;
		private EmulationTrader _trader;

		private readonly StrategyLogManager _logManager = new StrategyLogManager();

		private DateTime _lastUpdateDate;
		private DateTime _startEmulationTime;

		public MainWindow()
		{
			InitializeComponent();

			_logManager.Listeners.Add(new FileLogListener("log.txt"));
		}

		private void StartBtn_Click(object sender, RoutedEventArgs e)
		{
			// если процесс был запущен, то его останавливаем
			if (_trader != null)
			{
				_strategy.Stop();
				_trader.Stop();
				_logManager.Strategies.Clear();

				_trader = null;
				return;
			}

			// создаем тестовый инструмент, на котором будет производится тестирование
			var security = new Security
			{
				Id = "RIU9@RTS",
				Code = "RIU9",
				Name = "RTS-9.09",
				MinStepSize = 5,
				MinStepPrice = 2,
				Decimals = 0,
				Exchange = Exchange.Test,
			};

			// тестовый портфель
			var portfolio = new Portfolio { Name = "test account" };

			var timeFrame = TimeSpan.FromMinutes(5);

			var startTime = new DateTime(2009, 6, 1);
			var stopTime = new DateTime(2009, 9, 1);

			// создаем шлюз для эмуляции
			_trader = new EmulationTrader(
				new[] { security },
				new[] { portfolio })
			{
				MarketTimeChangedInterval = timeFrame
			};

			// стартовое значения для генерации случайных данных
			security.LastTrade.Price = 155000;

			_trader.TradeGenerators[security] = new RandomWalkTradeGenerator(security);

			_trader.DepthGenerators[security] = new TrendMarketDepthGenerator(security);

			var candleManager = new CandleManager(new SyncTraderCandleSource(_trader));

			candleManager.RegisterTimeFrameCandles(security, timeFrame);

			// создаем торговую стратегию, скользящие средние на 80 5-минуток и 10 5-минуток
			_strategy = new SmaStrategy(candleManager, new Sma(80), new Sma(10), timeFrame)
			{
				Volume = 1,
				Security = security,
				Portfolio = portfolio,
				Trader = _trader,
			};

			// копируем параметры на визуальную панель
			this.ParametersPanel.Parameters.Clear();
			this.ParametersPanel.Parameters.AddRange(_strategy.EquityManager.Parameters);

			_strategy.EquityManager.NewEquityData += data => this.GuiAsync(() => _curveItems.Add(data));

			_logManager.Strategies.Add(_strategy);

			// и подписываемся на событие изменения времени, чтобы обновить ProgressBar
			_trader.MarketTimeChanged += () =>
			{
				// в целях оптимизации обновляем ProgressBar только при начале нового дня
				if (_trader.MarketTime.Date != _lastUpdateDate || _trader.MarketTime >= stopTime)
				{
					_lastUpdateDate = _trader.MarketTime.Date;
					this.GuiAsync(() => this.TestingProcess.Value = (_trader.MarketTime - startTime).Ticks);
				}
			};

			_trader.StateChanged += () =>
			{
				if (_trader.State == EmulationStates.Stopped)
				{
					this.GuiAsync(() =>
					{
						this.Report.IsEnabled = true;

						if (this.TestingProcess.Value == this.TestingProcess.Maximum)
							MessageBox.Show("Закончено за " + (DateTime.Now - _startEmulationTime));
						else
							MessageBox.Show("Отменено");
					});
				}
				else if (_trader.State == EmulationStates.Started)
				{
					// запускаем стратегию когда эмулятор запустился
					_strategy.Start();
				}
			};

			if (_curveItems == null)
				_curveItems = this.Curve.CreateCurve(_strategy.Name, Colors.DarkGreen);
			else
				_curveItems.Clear();

			// устанавливаем в визуальный элемент ProgressBar максимальное количество итераций)
			this.TestingProcess.Maximum = (stopTime - startTime).Ticks;
			this.TestingProcess.Value = 0;
			this.Report.IsEnabled = false;

			_startEmulationTime = DateTime.Now;

			// соединяемся с трейдером и запускаем экспорт,
			// чтобы инициализировать переданными инструментами и портфелями необходимые свойства EmulationTrader
			_trader.Connect();
			_trader.StartExport();

			// запускаем эмуляцию
			_trader.Start(new DateTime(2009, 6, 1), new DateTime(2009, 9, 1));
		}

		private void Report_Click(object sender, RoutedEventArgs e)
		{
			// сгерерировать отчет по прошедшему тестированию
			// Внимание! сделок и заявок может быть большое количество,
			// поэтому Excel отчет может тормозить
			new ExcelStrategyReport(_strategy, "sma.xlsx").Generate();

			// открыть отчет
			Process.Start("sma.xlsx");
		}
	}
}