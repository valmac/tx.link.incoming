namespace SampleHistoryTesting
{
	using System;
	using System.Collections.Generic;
	using System.Diagnostics;
	using System.IO;
	using System.Windows;
	using System.Windows.Forms;
	using System.Windows.Media;
	using MessageBox = System.Windows.MessageBox;

	using Ecng.Common;
	using Ecng.Serialization;
	using Ecng.Xaml;
	using Ecng.Collections;

	using StockSharp.Algo.Candles;
	using StockSharp.Algo.Reporting;
	using StockSharp.Algo.Storages;
	using StockSharp.Algo.Strategies;
	using StockSharp.Algo.Testing;
	using StockSharp.Algo.Logging;
	using StockSharp.Algo.Equity;
	using StockSharp.BusinessEntities;
	
	public partial class MainWindow
	{
		private Strategy _strategy;

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

		private void FindPath_Click(object sender, RoutedEventArgs e)
		{
			var dlg = new FolderBrowserDialog();

			if (!this.HistoryPath.Text.IsEmpty())
				dlg.SelectedPath = this.HistoryPath.Text;

			if (dlg.ShowDialog() == System.Windows.Forms.DialogResult.OK)
			{
				this.HistoryPath.Text = dlg.SelectedPath;
			}
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

			if (this.HistoryPath.Text.IsEmpty() || !Directory.Exists(this.HistoryPath.Text))
			{
				MessageBox.Show(this, "Неправильный путь.");
				return;
			}

			// создаем тестовый инструмент, на котором будет производится тестирование
			var security = new Security
			{
				Id = "RIU9@RTS", // по идентификатору инструмента будет искаться папка с историческими маркет данными
				Code = "RIU9",
				Name = "RTS-9.09",
				MinStepSize = 5,
				MinStepPrice = 2,
				Exchange = Exchange.Test,
			};

			// тестовый портфель
			var portfolio = new Portfolio { Name = "test account", BeginAmount = 1000000m };

			// хранилище, через которое будет производиться доступ к тиковой и котировочной базе
			var storage = new TradingStorage(new InMemoryStorage())
			{
				BasePath = this.HistoryPath.Text
			};

			var timeFrame = TimeSpan.FromMinutes(5);

			// в реальности период может быть другим, и это зависит от объема данных,
			// хранящихся по пути HistoryPath, 
			var startTime = new DateTime(2009, 6, 1);
			var stopTime = new DateTime(2009, 9, 1);
	
			_trader = new EmulationTrader(
				new[] { security },
				new[] { portfolio })
			{
				MarketTimeChangedInterval = timeFrame,
				Storage = storage,
				WorkingTime = Exchange.Rts.WorkingTime
			};
			
			_trader.DepthGenerators[security] = new TrendMarketDepthGenerator(security)
			{
				// стакан для инструмента в истории обновляется раз в секунду
				Interval = TimeSpan.FromSeconds(1),
				Settings = { GenerateDepthOnEachTrade = false }
			};

			var candleManager = new CandleManager(new SyncTraderCandleSource(_trader));

			candleManager.RegisterTimeFrameCandles(security, timeFrame);

			// создаем торговую стратегию, скользящие средние на 80 5-минуток и 10 5-минуток
			_strategy = new SmaStrategy(candleManager, new Sma(80), new Sma(10), timeFrame)
			{
				Volume = 1,
				Portfolio = portfolio,
				Security = security,
				Trader = _trader,
			};

			// копируем параметры на визуальную панель
			this.ParametersPanel.Parameters.Clear();
			this.ParametersPanel.Parameters.AddRange(_strategy.EquityManager.Parameters);

			if (_curveItems == null)
				_curveItems = this.Curve.CreateCurve(_strategy.Name, Colors.DarkGreen);
			else
				_curveItems.Clear();

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

			// устанавливаем в визуальный элемент ProgressBar максимальное количество итераций)
			this.TestingProcess.Maximum = (stopTime - startTime).Ticks;
			this.TestingProcess.Value = 0;
			this.Report.IsEnabled = false;

			_startEmulationTime = DateTime.Now;

			// соединяемся с трейдером и запускаем экспорт,
			// чтобы инициализировать переданными инструментами и портфелями необходимые свойства EmulationTrader
			_trader.Connect();
			_trader.StartExport();

			// запускаем эмуляцию, задавая период тестирования (startTime, stopTime).
			_trader.Start(startTime, stopTime);
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