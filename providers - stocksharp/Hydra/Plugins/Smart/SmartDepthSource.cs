namespace StockSharp.Hydra.Smart
{
	using System;
	using System.Collections.Generic;
	using System.ComponentModel;
	using System.Threading;
	using System.Net;

	using Ecng.Collections;
	using Ecng.Common;
	using Ecng.Xaml;

	using StockSharp.BusinessEntities;
	using StockSharp.Hydra.Core;
	using StockSharp.Smart;

	class SmartDepthSource : IMarketDepthSource, ISecuritySource, ITradeSource
	{
		private sealed class SmartSettings : MarketDataSourceSettings
		{
			public SmartSettings(MarketDataSourceSettings settings)
			{
				if (settings == null)
					throw new ArgumentNullException("settings");

				RealSettings = settings;

				if (!RealSettings.ExtensionInfo.ContainsKey("Login"))
					Login = string.Empty;

				if (!RealSettings.ExtensionInfo.ContainsKey("Password"))
					Password = string.Empty;

				if (!RealSettings.ExtensionInfo.ContainsKey("Address"))
					Address = SmartAddresses.Major.To<string>();

				if (!RealSettings.ExtensionInfo.ContainsKey("UseLocalTradeTime"))
					UseLocalTradeTime = false;
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

			[Category("Smart")]
			[DisplayName("Логин")]
			public string Login
			{
				get { return (string)RealSettings.ExtensionInfo["Login"]; }
				set { RealSettings.ExtensionInfo["Login"] = value; }
			}

			[Category("Smart")]
			[DisplayName("Пароль")]
			public string Password
			{
				get { return (string)RealSettings.ExtensionInfo["Password"]; }
				set { RealSettings.ExtensionInfo["Password"] = value; }
			}

			[Category("Smart")]
			[DisplayName("Адрес")]
			public string Address
			{
				get { return RealSettings.ExtensionInfo["Address"].To<string>(); }
				set { RealSettings.ExtensionInfo["Address"] = value; }
			}

			[Category("Smart")]
			[DisplayName("Локальное время сделок")]
			public bool UseLocalTradeTime
			{
				get { return RealSettings.ExtensionInfo["UseLocalTradeTime"].To<bool>(); }
				set { RealSettings.ExtensionInfo["UseLocalTradeTime"] = value; }
			}
			
			internal MarketDataSourceSettings RealSettings { get; private set; }
		}

		private readonly Guid _id = "1ED92215-3E57-4EC9-89E5-22EC65786C67".To<Guid>();
		private readonly HydraStorage _storage;
		private readonly SmartSettings _settings;
		private readonly MarketDataTrader _trader;

		public SmartDepthSource(HydraStorage storage)
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

				_settings = new SmartSettings(settings)
				{
					Address = SmartAddresses.Major.ToString(),
				};

				SaveSettings();
			}
			else
				_settings = new SmartSettings(settings);

			_trader = new MarketDataTrader(new SecurityStorage(this), () =>
				new SmartTrader(_settings.Login, _settings.Password, _settings.Address.To<IPAddress>()))
			{
				UseLocalTradeTime = _settings.UseLocalTradeTime
			};
		}

		public int TotalDepths { get { return _trader.TotalDepths; } }

		public int TotalTrades { get { return _trader.TotalTrades; } }

		Uri IMarketDataSource.Icon
		{
			get { return "smart_logo.png".GetResourceUrl(GetType()); }
		}

		Guid IMarketDataSource.Id
		{
			get { return _id; }
		}

		public string Name
		{
			get { return "Smart"; }
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
			
			// ждем пока обновит Security
			using (var evt = new AutoResetEvent(false))
				evt.WaitOne(TimeSpan.FromSeconds(20));

			securities.ForEach(_trader.Trader.RegisterQuotes);
			securities.ForEach(_trader.Trader.RegisterTrades);
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
