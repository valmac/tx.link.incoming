namespace StockSharp.Hydra
{
	using System;
	using System.Collections.Generic;
	using System.Linq;
	using System.Threading;

	using Ecng.Collections;
	using Ecng.Common;

	using StockSharp.Hydra.Core;

	class Worker : Disposable
	{
		private readonly HydraStorage _storage;
		private readonly IEnumerable<IMarketDataSource> _sources;
		private readonly int _maxErrorCount;
		private int _currentErrorCount;
		private Timer _timer;
		private bool _isDownloading;
		private readonly Dictionary<string, List<VisualSecurity>> _securities = new Dictionary<string, List<VisualSecurity>>();

		public Worker(HydraStorage storage, IEnumerable<IMarketDataSource> sources, int maxErrorCount)
		{
			if (storage == null)
				throw new ArgumentNullException("storage");

			if (sources == null)
				throw new ArgumentNullException("sources");

			if (maxErrorCount < 0)
				throw new ArgumentOutOfRangeException("maxErrorCount");

			_storage = storage;
			_sources = sources;
			_maxErrorCount = maxErrorCount;
		}

		private IEnumerable<IMarketDataSource> EnabledSources
		{
			get { return _sources.Where(s => s.Settings.IsEnabled); }
		}

		public event Action<IMarketDataSource, Exception> Error;
		public event Action<IMarketDataSource, string> Log;
		public event Action Stopped;

		public void Start(IEnumerable<VisualSecurity> securities)
		{
			if (securities == null)
				throw new ArgumentNullException("securities");

			if (_timer != null)
				throw new InvalidOperationException();

			_securities.Clear();

			foreach (var group in securities.GroupBy(s => s.TradeSource)
				.Concat(securities.GroupBy(s => s.DepthSource))
				.Where(g => !g.Key.IsEmpty()))
			{
				_securities.SafeAdd(group.Key).AddRange(group);
			}

			_isDownloading = false;

			foreach (var source in EnabledSources)
			{
				var sourceSecurities = _securities.TryGetValue(source.Name);

				if (sourceSecurities != null)
					source.Start(sourceSecurities.Select(s => s.Security));
			}

			_currentErrorCount = 0;

			if (EnabledSources.Count() > 0)
				_timer = new Timer(arg => Download(), null, TimeSpan.Zero, EnabledSources.Min(s => s.Settings.Interval).Max(TimeSpan.FromSeconds(1)));
		}

		public void Stop()
		{
			if (_timer != null)
			{
				EnabledSources.ForEach(s => s.Stop());

				_timer.Dispose();
				_timer = null;	
			}

			if (!_isDownloading)
				Stopped.SafeInvoke();
		}

		private void Download()
		{
			if (_isDownloading)
				return;

			_isDownloading = true;

			EnabledSources.ForEach(source =>
			{
				try
				{
					
					if (DateTime.Now.TimeOfDay < source.Settings.WorkingFrom || DateTime.Now.TimeOfDay > source.Settings.WorkingTo)
					{
						Log.SafeInvoke(source, "Вне диапазона времени.");
						return;
					}

					IEnumerable<VisualSecurity> securities;

					if (source is ISecuritySource)
					{
						securities = _securities.TryGetValue(source.Name) ?? Enumerable.Empty<VisualSecurity>();
						Log.SafeInvoke(source, "Стартовал для {0} инструментов.".Put(securities.Count()));
					}
					else
					{
						securities = new VisualSecurity[1];
						Log.SafeInvoke(source, "Стартовал.");
					}


					var tradeSource = source as ITradeSource;
					var depthSource = source as IMarketDepthSource;

					foreach (var s in securities)
					{
						var targetSecurity = source is ISecuritySource ? s.Security : null;

						if (tradeSource != null)
						{
							var loaded = tradeSource.Load(targetSecurity);

							if (loaded != null)
							{
								if (loaded.Count() > 0)
								{
									Log.SafeInvoke(tradeSource, "Загружено {0} сделок.".Put(loaded.Count()));

									var first = loaded.First();
									var last = loaded.Last();

									loaded = loaded.OrderBy(t => t.Id);

									Log.SafeInvoke(tradeSource, "Первая сделка {0} для {1} за {2}.".Put(first.Id, first.Security.Id, first.Time));
									Log.SafeInvoke(tradeSource, "Последняя сделка {0} для {1} за {2}.".Put(last.Id, last.Security.Id, last.Time));

									foreach (var group in loaded.GroupBy(t => t.Security))
									{
										var security = group.Key;
										var trades = group;

										Log.SafeInvoke(tradeSource, "Для инструмента '{0}' загружено {1} сделок.".Put(security.Id, trades.Count()));

										_storage.GetTradeStorage(security).Save(trades);

										var visualSec = security.ToVisualSecurity();
										visualSec.TradeCount += trades.Count();
										visualSec.LastUpdateTime = DateTime.Now;
										_storage.Securities.Save(security);
									}
								}

								tradeSource.CommitLoad(s != null ? s.Security : null);
							}
							//else
							//	Log.SafeInvoke(tradeSource, "Сделки не были загружены.");
						}

						if (depthSource != null)
						{
							var loaded = depthSource.Load(targetSecurity);

							if (loaded.Count() > 0)
							{
								Log.SafeInvoke(depthSource, "Загружено {0} стаканов.".Put(loaded.Count()));

								var first = loaded.First();
								var last = loaded.Last();
								Log.SafeInvoke(depthSource, "Первый стакан для {0} за {1}.".Put(first.Security.Id, first.LastChangeTime));
								Log.SafeInvoke(depthSource, "Последный стакан для {0} за {1}.".Put(last.Security.Id, last.LastChangeTime));

								foreach (var group in loaded.GroupBy(t => t.Security))
								{
									var security = group.Key;
									var depths = group;

									Log.SafeInvoke(depthSource, "Для инструмента '{0}' загружено {1} стаканов.".Put(security.Id, depths.Count()));

									_storage.GetMarketDepthStorage(security).Save(depths);

									var visualSec = security.ToVisualSecurity();
									visualSec.DepthCount += depths.Count();
									visualSec.LastUpdateTime = DateTime.Now;
									_storage.Securities.Save(security);
								}

								depthSource.CommitLoad(s != null ? s.Security : null);
							}
							//else
							//	Log.SafeInvoke(depthSource, "Стаканы не были загружены.");
						}
					}
				}
				catch (Exception ex)
				{
					Error.SafeInvoke(source, ex);

					if (_maxErrorCount != 0)
					{
						_currentErrorCount++;

						if (_currentErrorCount == _maxErrorCount)
							Stop();
					}
				}
				finally
				{
					_storage.Storage.ClearCache();

					GC.Collect();
					GC.WaitForPendingFinalizers();
					GC.Collect();
				}
			});

			_isDownloading = false;

			if (_timer == null)
				Stopped.SafeInvoke();
		}

		protected override void DisposeManaged()
		{
			Stop();
			base.DisposeManaged();
		}
	}
}
