namespace StockSharp.Hydra.Core
{
	using System;
	using System.Collections.Generic;
	using System.Linq;
	using System.Threading;

	using Ecng.Common;
	using Ecng.Collections;

	using StockSharp.Algo;
	using StockSharp.Algo.History;
	using StockSharp.Algo.Storages;
	using StockSharp.BusinessEntities;

	/// <summary>
	/// Обертка над шлюзом <see cref="ITrader"/> для получения маркет-данных в реальном времени.
	/// </summary>
	public class MarketDataTrader : Disposable, ISecuritySource
	{
		private sealed class MarketDataEntityFactory : EntityFactory
		{
			private readonly ISecurityStorage _securityStorage;

			public MarketDataEntityFactory(ISecurityStorage securityStorage)
			{
				if (securityStorage == null)
					throw new ArgumentNullException("securityStorage");

				_securityStorage = securityStorage;
			}

			public override Security CreateSecurity(string id)
			{
				return _securityStorage.LoadBy("Id", id) ?? base.CreateSecurity(id);
			}
		}

		private readonly Func<ITrader> _createTrader;
		private readonly MarketDataBuffer<Trade> _tradesBuffer = new MarketDataBuffer<Trade>();
		private readonly MarketDataBuffer<MarketDepth> _depthsBuffer = new MarketDataBuffer<MarketDepth>();
		private readonly ISecurityStorage _securityStorage;
		private readonly AutoResetEvent _connectedWait = new AutoResetEvent(false);

		/// <summary>
		/// Создать <see cref="MarketDataTrader"/>.
		/// </summary>
		/// <param name="securityStorage">Интерфейс для доступа к хранилищу информации об инструментах.</param>
		/// <param name="createTrader">Обработчик, создающий шлюз к торговой системе.</param>
		public MarketDataTrader(ISecurityStorage securityStorage, Func<ITrader> createTrader)
		{
			if (securityStorage == null)
				throw new ArgumentNullException("securityStorage");

			if (createTrader == null)
				throw new ArgumentNullException("createTrader");

			_securityStorage = securityStorage;
			_createTrader = createTrader;
		}

		/// <summary>
		/// Ошибка в шлюзе.
		/// </summary>
		public event Action<Exception> TraderError;

		/// <summary>
		/// Шлюз к торговой системе.
		/// </summary>
		public ITrader Trader { get; private set; }

		/// <summary>
		/// Информация о последней ошибке.
		/// </summary>
		public Exception LastError { get; set; }

		/// <summary>
		/// Записывать локальное время получения сделки, а не биржевое.
		/// </summary>
		public bool UseLocalTradeTime { get; set; }

		/// <summary>
		/// Число сделок загруженных из источника.
		/// </summary>
		public int TotalTrades { get; private set; }

		/// <summary>
		/// Число стаканов загруженных из источника.
		/// </summary>
		public int TotalDepths { get; private set; }

		/// <summary>
		/// Получить накопленные стаканы.
		/// </summary>
		/// <param name="security">Инструмент, стаканы которого необходимо получить.</param>
		/// <returns>Накопленные стаканы.</returns>
		public IEnumerable<MarketDepth> GetMarketDepths(Security security)
		{
			return _depthsBuffer.Get(security);
		}

		/// <summary>
		/// Получить накопленные тиковые сделки.
		/// </summary>
		/// <param name="security">Инструмент, сделки которого необходимо получить.</param>
		/// <returns>Накопленные сделки.</returns>
		public IEnumerable<Trade> GetTrades(Security security)
		{
			return _tradesBuffer.Get(security);
		}

		private void OnConnected()
		{
			_connectedWait.Set();
			Trader.StartExport();
		}

		private void OnNewTrades(IEnumerable<Trade> trades)
		{
			TotalTrades += trades.Count();
			trades.ForEach(t =>
			{
				if (UseLocalTradeTime)
					t.Time = Trader.MarketTime;

				_tradesBuffer.Add(t.Security, t);
			});
		}

		private void OnQuotesChanged(IEnumerable<MarketDepth> depths)
		{
			depths.ForEach(d =>
			{
				//Trace.WriteLine("MDA " +dc.Security+":"+dc._DebugId+":"+ dc.LastChangeTime.ToString("HHmmss.fff"));
				if (d.Bids.Length > 0 && d.Asks.Length > 0)
				{
					TotalDepths += depths.Count();
					_depthsBuffer.Add(d.Security, d.Clone());
				}
			});
		}

		private void OnError(Exception error)
		{
			LastError = error;
			TraderError.SafeInvoke(error);
			Log.SafeInvoke("Ошибка: {0}{1}{2}".Put(error, Environment.NewLine, error.StackTrace));
		}

		/// <summary>
		/// Запустить накопление маркет-данных.
		/// </summary>
		public void Start()
		{
			Trader = _createTrader();

			try
			{
				((BaseTrader)Trader).EntityFactory = new MarketDataEntityFactory(_securityStorage);

				Trader.ProcessDataError += OnError;
				Trader.Connected += OnConnected;
				Trader.NewTrades += OnNewTrades;
				Trader.QuotesChanged += OnQuotesChanged;

				Trader.Connect();

				if (!_connectedWait.WaitOne(TimeSpan.FromSeconds(20)))
					throw new TimeoutException("Ожидание подключения превысило максимально допустимый интервал.");
			}
			catch
			{
				Trader.Dispose();
				throw;
			}
		}
		
		/// <summary>
		/// Остановить накопление маркет-данных.
		/// </summary>
		public void Stop()
		{
			Trader.ProcessDataError -= OnError;
			Trader.Connected -= OnConnected;
			Trader.NewTrades -= OnNewTrades;
			Trader.QuotesChanged -= OnQuotesChanged;

			Trader.Dispose();

			LastError = null;
		}

		private sealed class SecurityUpdate : Disposable
		{
			private readonly ITrader _trader;
			private readonly TimeSpan _initialDelay;
			private readonly TimeSpan _noDataDelay;

			private DateTime _lastReceived;
			private DateTime _started;

			public SecurityUpdate(ITrader trader)
			{
				_trader = trader;
				_initialDelay = TimeSpan.FromSeconds(180);
				_noDataDelay = TimeSpan.FromSeconds(30);
				_trader.NewSecurities += OnNewSecurities;
			}

			public void Run()
			{
				_started = DateTime.Now;
				_lastReceived = DateTime.MinValue;

				while (true)
				{
					Thread.Sleep(1000);

					if (_lastReceived == DateTime.MinValue)
					{
						if (DateTime.Now - _started > _initialDelay)
							break;
					}
					else
					{
						if (DateTime.Now - _lastReceived > _noDataDelay)
							break;
					}
				}
			}

			void OnNewSecurities(IEnumerable<Security> securities)
			{
				_lastReceived = DateTime.Now;
			}

			protected override void DisposeManaged()
			{
				_trader.NewSecurities -= OnNewSecurities;
			}
		}

		/// <summary>
		/// Получить новые инструменты.
		/// </summary>
		/// <returns>Новые инструменты.</returns>
		public IEnumerable<Security> GetNewSecurities()
		{
			Start();

			/*using (var evt = new AutoResetEvent(false))
				evt.WaitOne(TimeSpan.FromSeconds(20));*/

			using (var su = new SecurityUpdate(Trader))
				su.Run();

			Stop();

			var securities = Trader.Securities;
			securities = securities.Where(s => s.ExtensionInfo == null || !s.ExtensionInfo.ContainsKey("Source")).ToArray();

			Log.SafeInvoke("Получены новые инструменты:");
			securities.ForEach(security => Log.SafeInvoke(security.Id));

			securities.ForEach(s => _securityStorage.Save(s));
			return securities;
		}

		/// <summary>
		/// Освободить занятые ресурсы.
		/// </summary>
		protected override void DisposeManaged()
		{
			Stop();
			base.DisposeManaged();
		}

		/// <summary>
		/// Событие о новом сообщении от <see cref="MarketDataTrader"/>.
		/// </summary>
		public event Action<string> Log;
	}
}
