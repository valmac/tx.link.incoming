namespace StockSharp.Hydra.Quik
{
	using System;
	using System.Collections.Generic;
	using System.ComponentModel;

	using Ecng.Collections;
	using Ecng.Common;
	using Ecng.Xaml;

	using StockSharp.BusinessEntities;
	using StockSharp.Hydra.Core;
	using StockSharp.Quik;

	class QuikDepthSource : IMarketDepthSource, ISecuritySource, ITradeSource
	{
		private sealed class QuikSettings : MarketDataSourceSettings
		{
			public QuikSettings(MarketDataSourceSettings settings)
			{
				if (settings == null)
					throw new ArgumentNullException("settings");

				RealSettings = settings;
			}

			public override bool IsEnabled
			{
				get { return RealSettings.IsEnabled; }
				set { RealSettings.IsEnabled = value; }
			}

			public override TimeSpan WorkingFrom
			{
				get { return RealSettings.WorkingFrom; }
				set { RealSettings.WorkingFrom = value; }
			}

			public override TimeSpan WorkingTo
			{
				get { return RealSettings.WorkingTo; }
				set { RealSettings.WorkingTo = value; }
			}

			public override TimeSpan Interval
			{
				get { return RealSettings.Interval; }
				set { RealSettings.Interval = value; }
			}

			public override string DumpFolder
			{
				get { return RealSettings.DumpFolder; }
				set { RealSettings.DumpFolder = value; }
			}

			[Category("Quik")]
			[DisplayName("Путь")]
			public string Path
			{
				get { return (string)RealSettings.ExtensionInfo["Path"]; }
				set { RealSettings.ExtensionInfo["Path"] = value; }
			}

			[Category("Quik")]
			[DisplayName("ДДЕ сервер")]
			public string DdeServer
			{
				get { return (string)RealSettings.ExtensionInfo["DdeServer"]; }
				set { RealSettings.ExtensionInfo["DdeServer"] = value; }
			}

			internal MarketDataSourceSettings RealSettings { get; private set; }
		}

		private readonly Guid _id = "CB45C71C-DCBE-431B-B28C-93FAB7337759".To<Guid>();
		private readonly HydraStorage _storage;
		private readonly QuikSettings _settings;
		private readonly MarketDataTrader _trader;

		public QuikDepthSource(HydraStorage storage)
		{
			if (storage == null)
				throw new ArgumentNullException("storage");

			_storage = storage;

			var settings = _storage.SourcesSettings.LoadBySourceId(_id);

			if (settings == null)
			{
				settings = new MarketDataSourceSettings
				{
					SourceId = _id,
					WorkingFrom = TimeSpan.Zero,
					WorkingTo = TimeSpan.FromTicks(TimeSpan.TicksPerDay - 1),
					ExtensionInfo = new Dictionary<object, object>(),
					DumpFolder = string.Empty,
				};

				_settings = new QuikSettings(settings)
				{
					Path = QuikTerminal.GetDefaultPath() ?? string.Empty,
					DdeServer = "hydra",
				};

				SaveSettings();
			}
			else
				_settings = new QuikSettings(settings);

			_trader = new MarketDataTrader(new SecurityStorage(this), () => new HydraQuikTrader(_settings.Path, _settings.DdeServer));
		}

		public int TotalDepths { get { return _trader.TotalDepths; } }

		public int TotalTrades { get { return _trader.TotalTrades; } }

		Uri IMarketDataSource.Icon
		{
			get { return "quik_logo.png".GetResourceUrl(GetType()); }
		}

		Guid IMarketDataSource.Id
		{
			get { return _id; }
		}

		string IMarketDataSource.Name
		{
			get { return "Quik"; }
		}

		HydraStorage IMarketDataSource.Storage
		{
			get { return _storage; }
		}

		MarketDataSourceSettings IMarketDataSource.Settings
		{
			get { return _settings; }
		}

		public event Action<string> Log;

		public void SaveSettings()
		{
			_storage.SourcesSettings.Save(_settings.RealSettings);
		}

		public void Start(IEnumerable<Security> securities)
		{
			_trader.Start();
			securities.ForEach(_trader.Trader.RegisterQuotes);
		}

		public void Stop()
		{
			_trader.Stop();
		}

		public void CommitLoad(Security security)
		{
		}

		public IEnumerable<Security> GetNewSecurities()
		{
			return _trader.GetNewSecurities();
		}

		IEnumerable<MarketDepth> IMarketDepthSource.Load(Security security)
		{
			if (_trader.LastError != null)
			{
				var copy = _trader.LastError;
				_trader.LastError = null;
				throw copy;
			}

			return _trader.GetMarketDepths(security);
		}

		IEnumerable<Trade> ITradeSource.Load(Security security)
		{
			return _trader.GetTrades(security);
		}
	}
}