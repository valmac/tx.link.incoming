namespace StockSharp.Hydra.Core
{
	using Ecng.Serialization;

	using StockSharp.Algo.Storages;

	/// <summary>
	/// Корневой объект для доступа к базе данных Hydra.
	/// </summary>
	public class HydraStorage : TradingStorage
	{
		/// <summary>
		/// Создать <see cref="HydraStorage"/>.
		/// </summary>
		/// <param name="storage">Специальный интерфейс для прямого доступа к хранилищу.</param>
		public HydraStorage(IStorage storage)
			: base(storage)
		{
			SourcesSettings = new MarketDataSourceSettingsList(storage);
		}

		/// <summary>
		/// Настройки источников <see cref="IMarketDataSource"/>.
		/// </summary>
		public MarketDataSourceSettingsList SourcesSettings { get; private set; }
	}
}