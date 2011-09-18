namespace StockSharp.Hydra.Finam
{
	using System;

	using Ecng.Collections;

	using StockSharp.Algo.History.Finam;
	using StockSharp.BusinessEntities;
	using StockSharp.Hydra.Core;

	class FinamSecurityStorage : SecurityStorage
	{
		private readonly SynchronizedDictionary<long, Security> _cacheByFinamId = new SynchronizedDictionary<long, Security>();

		public FinamSecurityStorage(FinamTradeSource source)
			: base(source)
		{
		}

		public override Security LoadBy(string fieldName, object fieldValue)
		{
			if (_cacheByFinamId.Count == 0)
			{
				foreach (var security in Source.Storage.Securities)
					TryAddToCache(security);
			}

			return _cacheByFinamId.TryGetValue((long)fieldValue);
		}

		public override void Save(Security security)
		{
			base.Save(security);
			TryAddToCache(security);
		}

		private void TryAddToCache(Security security)
		{
			if (security == null)
				throw new ArgumentNullException("security");

			var finamId = security.ExtensionInfo.TryGetValue(FinamHistorySource.FinamSecurityIdField);

			if (finamId != null)
				_cacheByFinamId.SafeAdd((long)finamId, key => security);
		}
	}
}
