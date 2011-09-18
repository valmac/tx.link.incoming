namespace StockSharp.Hydra.Finam
{
	using System;
	using System.Collections.Generic;
	using System.ComponentModel;

	using Ecng.Common;
	using Ecng.Xaml;

	using StockSharp.Algo.History.Finam;
	using StockSharp.BusinessEntities;
	using StockSharp.Hydra.Core;

	class FinamTradeSource : ITradeSource, ISecuritySource
	{
		private sealed class FinamSettings : MarketDataSourceSettings
		{
			public FinamSettings(MarketDataSourceSettings settings)
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

			[Category("Финам")]
			[DisplayName("Временной отступ в днях")]
			public int FinamOffset
			{
				get { return RealSettings.ExtensionInfo["FinamOffset"].To<int>(); }
				set { RealSettings.ExtensionInfo["FinamOffset"] = value; }
			}

			[Category("Финам")]
			[DisplayName("Начальная дата")]
			public DateTime StartFrom
			{
				get { return RealSettings.ExtensionInfo["StartFrom"].To<DateTime>(); }
				set { RealSettings.ExtensionInfo["StartFrom"] = value; }
			}

			internal MarketDataSourceSettings RealSettings { get; private set; }
		}

		private readonly Guid _id = "CDDCA9BD-8D2A-4194-B2BF-364DAFABFFB6".To<Guid>();

		private readonly HydraStorage _storage;
		private readonly FinamSettings _settings;
		private readonly FinamHistorySource _source;

		public FinamTradeSource(HydraStorage storage)
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
					Interval = TimeSpan.FromMinutes(1),
					DumpFolder = string.Empty,
				};

				_settings = new FinamSettings(settings)
				{
					FinamOffset = 7,
					StartFrom = new DateTime(2005, 1, 1),
				};

				SaveSettings();
			}
			else
				_settings = new FinamSettings(settings);

			_source = new FinamHistorySource(new FinamSecurityStorage(this)) { DumpFolder = _settings.DumpFolder };
		}

		public int TotalTrades { get { return 0; } }

		public HydraStorage Storage
		{
			get { return _storage; }
		}

		public Uri Icon
		{
			get { return "finam_logo.png".GetResourceUrl(GetType()); }
		}

		public Guid Id
		{
			get { return _id; }
		}

		public MarketDataSourceSettings Settings
		{
			get { return _settings; }
		}

		public string Name
		{
			get { return "Finam"; }
		}

		public event Action<string> Log;

		public void SaveSettings()
		{
			_storage.SourcesSettings.Save(_settings.RealSettings);

			if (_source != null)
				_source.DumpFolder = _settings.DumpFolder;
		}

		public void Start(IEnumerable<Security> securities)
		{
		}

		public void Stop()
		{
		}

		public IEnumerable<Trade> Load(Security security)
		{
			if (security == null)
				throw new ArgumentNullException("security");

			if (!security.ExtensionInfo.ContainsKey("FinamLastTradeTime"))
				security.ExtensionInfo.Add("FinamLastTradeTime", _settings.StartFrom.To<long>());
           
			var date = security.ExtensionInfo["FinamLastTradeTime"].To<DateTime>();
            if (date < _settings.StartFrom)
            {
                date = _settings.StartFrom;
                security.ExtensionInfo["FinamLastTradeTime"] = date;
            }

			if ((date + TimeSpan.FromDays(_settings.FinamOffset)) < DateTime.Today)
			{
				Log.SafeInvoke("Старт с {0} для {1}.".Put(date.ToShortDateString(), security.Id));
				return _source.GetTrades(security, date);
			}
			else
			{
				Log.SafeInvoke("Дата {0} вне диапазона для {1}.".Put(date.ToShortDateString(), security.Id));
				return null;
			}
		}

		public void CommitLoad(Security security)
		{
			if (security == null)
				throw new ArgumentNullException("security");

			var date = security.ExtensionInfo["FinamLastTradeTime"].To<DateTime>();
			security.ExtensionInfo["FinamLastTradeTime"] = (date + TimeSpan.FromDays(1)).To<long>();
			_storage.Securities.Save(security);
		}

		public IEnumerable<Security> GetNewSecurities()
		{
			return _source.GetNewSecurities();
		}
	}
}