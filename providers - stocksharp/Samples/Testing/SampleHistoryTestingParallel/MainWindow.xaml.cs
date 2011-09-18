namespace SampleHistoryTestingParallel
{
	using System;
	using System.IO;
	using System.Linq;
	using System.Windows;
	using System.Windows.Forms;
	using System.Windows.Media;
	using MessageBox = System.Windows.MessageBox;

	using Ecng.Xaml;
	using Ecng.Common;
	using Ecng.Serialization;

	using StockSharp.Algo;
	using StockSharp.Algo.Candles;
	using StockSharp.Algo.Storages;
	using StockSharp.Algo.Testing;
	using StockSharp.BusinessEntities;

	using Wintellect.PowerCollections;

	public partial class MainWindow
	{
		private DateTime _lastUpdateDate;
		private DateTime _startEmulationTime;

		public MainWindow()
		{
			InitializeComponent();
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
			if (this.HistoryPath.Text.IsEmpty() || !Directory.Exists(this.HistoryPath.Text))
			{
				MessageBox.Show(this, "Неправильный путь.");
				return;
			}

			if (this.TestingProcess.Value != 0)
			{
				MessageBox.Show(this, "Уже запущен.");
				return;
			}

			// создаем длины скользящих средник
			var periods = new[]
			{
				new Triple<int, int, Color>(80, 10, Colors.DarkGreen),
				new Triple<int, int, Color>(70, 8, Colors.Red),
				new Triple<int, int, Color>(60, 6, Colors.DarkBlue),
			};

			// хранилище, через которое будет производиться доступ к тиковой и котировочной базе
			var storage = new TradingStorage(new InMemoryStorage())
			{
				BasePath = this.HistoryPath.Text
			};

			var timeFrame = TimeSpan.FromMinutes(5);

			var multiTrader = new MultiTrader
			{
				// не поддерживать контроль уникальности сделок
				// (уменьшает потребление памяти)
				SupportTradesUnique = false,
			};

			var startTime = new DateTime(2009, 6, 1);
			var stopTime = new DateTime(2009, 9, 1);

			foreach (var period in periods)
			{
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
				var portfolio = new Portfolio { Name = "test account" };

				// создаем шлюз для эмуляции
				// инициализируем настройки (инструмент в истории обновляется раз в секунду)
				var trader = new EmulationTrader(
					new [] { security },
					new[] { portfolio },
					storage)
				{
					MarketTimeChangedInterval = timeFrame,
					Storage = storage,
					WorkingTime = Exchange.Rts.WorkingTime
				};

				trader.DepthGenerators[security] = new TrendMarketDepthGenerator(security)
				{
					// стакан для инструмента в истории обновляется раз в секунду
					Interval = TimeSpan.FromSeconds(1),
					Settings = { GenerateDepthOnEachTrade = false }
				};

				var candleManager = new CandleManager(new SyncTraderCandleSource(trader));

				candleManager.RegisterTimeFrameCandles(security, timeFrame);

				// создаем торговую стратегию
				var strategy = new SmaStrategy(candleManager, new Sma(period.First), new Sma(period.Second), timeFrame)
				{
					Volume = 1,
					Security = security,
					Portfolio = portfolio,
					Trader = trader,
				};

				var curveItems = this.Curve.CreateCurve("Длинная {0} Короткая {1}".Put(period.First, period.Second), period.Third);
				strategy.EquityManager.NewEquityData += data => this.GuiAsync(() => curveItems.Add(data));

				// полосу прогресса будет обновлять только первый период
				if (periods.IndexOf(period) == 0)
				{
					// и подписываемся на событие изменения времени, чтобы обновить ProgressBar
					trader.MarketTimeChanged += () =>
					{
						// в целях оптимизации обновляем ProgressBar только при начале нового дня
						if (trader.MarketTime.Date != _lastUpdateDate || trader.MarketTime >= stopTime)
						{
							_lastUpdateDate = trader.MarketTime.Date;
							this.GuiAsync(() => this.TestingProcess.Value = (trader.MarketTime - startTime).Ticks);
						}
					};
				}

				trader.StateChanged += () =>
				{
					if (trader.State == EmulationStates.Stopped)
					{
						this.GuiAsync(() =>
						{
							if (this.TestingProcess.Value == this.TestingProcess.Maximum)
								MessageBox.Show("Закончено за " + (DateTime.Now - _startEmulationTime));
							else
								MessageBox.Show("Отменено");
						});
					}
					else if (trader.State == EmulationStates.Started)
					{
						// запускаем стратегию когда эмулятор запустился
						strategy.Start();
					}
				};

				multiTrader.AggregatedTraders.Add(trader);
			}

			// устанавливаем в визуальный элемент ProgressBar максимальное количество итераций)
			this.TestingProcess.Maximum = (stopTime - startTime).Ticks;
			this.TestingProcess.Value = 0;

			_startEmulationTime = DateTime.Now;

			// соединяемся с трейдером и запускаем экспорт,
			// чтобы инициализировать переданными инструментами и портфелями необходимые свойства EmulationTrader
			foreach (var trader in multiTrader.AggregatedTraders.Cast<IEmulationTrader>())
			{
				trader.Connect();
				trader.StartExport();
			}
			
			// запускаем эмуляцию
			foreach (var trader in multiTrader.AggregatedTraders.Cast<IEmulationTrader>())
			{
				// указываем даты начала и конца тестирования
				trader.Start(startTime, stopTime);
			}
		}
	}
}