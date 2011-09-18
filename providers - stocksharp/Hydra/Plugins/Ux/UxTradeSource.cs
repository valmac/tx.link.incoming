namespace StockSharp.Hydra.Ux
{
	using System;
	using System.Collections.Generic;
	using System.ComponentModel;
	using System.Linq;

	using Ecng.Common;
	using Ecng.Xaml;

	using StockSharp.Algo.History.Rts;
	using StockSharp.BusinessEntities;
	using StockSharp.Hydra.Core;

	class UxTradeSource : ITradeSource
	{
		private sealed class RtsSettings : MarketDataSourceSettings
		{
			public RtsSettings(MarketDataSourceSettings settings)
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

			[Category("RTS")]
			[DisplayName("Временной отступ в днях")]
			public int Offset
			{
				get { return RealSettings.ExtensionInfo["Offset"].To<int>(); }
				set { RealSettings.ExtensionInfo["Offset"] = value; }
			}

			[Category("РТС")]
			[DisplayName("Начальная дата")]
			public DateTime StartFrom
			{
				get { return RealSettings.ExtensionInfo["StartFrom"].To<DateTime>(); }
				set { RealSettings.ExtensionInfo["StartFrom"] = value; }
			}

			internal MarketDataSourceSettings RealSettings { get; private set; }
		}

		private readonly Guid _id = "72E0395E-1C95-4C5C-9C19-5913EBDD1CEA".To<Guid>();
		private readonly HydraStorage _storage;
		private readonly RtsSettings _settings;
		private readonly RtsHistorySource _source;

		private readonly DateTime _startDate = new DateTime(2010, 5, 27);

		public UxTradeSource(HydraStorage storage)
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

				_settings = new RtsSettings(settings)
				{
					Offset = 3,
					StartFrom = _startDate,
				};

				SaveSettings();
			}
			else
			{
				_settings = new RtsSettings(settings);
			}

			_source = new RtsHistorySource(new SecurityStorage(this))
			{
				Host = "ftp.ux.ua",
				Exchange = Exchange.Ux,
				DbfDirectory = "pub/info/statforts/",
				TxtDirectory = string.Empty, //"pub/info/stats/history/",
				DumpFolder = _settings.DumpFolder,
			};
		}

		public int TotalTrades { get { return 0; } }

		public Uri Icon
		{
			get { return "ux_logo.png".GetResourceUrl(GetType()); }
		}

		public Guid Id
		{
			get { return _id; }
		}

		public HydraStorage Storage
		{
			get { return _storage; }
		}

		public MarketDataSourceSettings Settings
		{
			get { return _settings; }
		}

		public void SaveSettings()
		{
			_storage.SourcesSettings.Save(_settings.RealSettings);

			if (_source != null)
				_source.DumpFolder = _settings.DumpFolder;
		}

		public string Name
		{
			get { return "UX"; }
		}

		public event Action<string> Log;

		public void Start(IEnumerable<Security> securities)
		{
		}

		public void Stop()
		{
		}

		public IEnumerable<Trade> Load(Security security)
		{
			if ((_settings.StartFrom + TimeSpan.FromDays(_settings.Offset)) < DateTime.Today)
			{
				Log.SafeInvoke("Старт с {0}.".Put(_settings.StartFrom.ToShortDateString()));
				return _source.GetTrades(_settings.StartFrom).SelectMany(p => p.Value);
			}
			else
			{
				Log.SafeInvoke("Дата {0} вне диапазона.".Put(_settings.StartFrom.ToShortDateString()));
				return null;
			}
		}

		public void CommitLoad(Security security)
		{
			_settings.StartFrom += TimeSpan.FromDays(1);
			SaveSettings();
		}
	}
}