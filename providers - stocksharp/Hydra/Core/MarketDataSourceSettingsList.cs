namespace StockSharp.Hydra.Core
{
	using System;

	using Ecng.Serialization;

	using StockSharp.Algo.Storages;

	/// <summary>
	/// Настройки источника <see cref="IMarketDataSource"/>.
	/// </summary>
	public class MarketDataSourceSettingsList : BaseStorageEntityList<MarketDataSourceSettings>
	{
		private static readonly Field _sourceIdField = SchemaManager.GetSchema<MarketDataSourceSettings>().Fields["SourceId"];

		/// <summary>
		/// Создать <see cref="MarketDataSourceSettingsList"/>.
		/// </summary>
		/// <param name="storage">Специальный интерфейс для прямого доступа к хранилищу.</param>
		public MarketDataSourceSettingsList(IStorage storage)
			: base(storage)
		{
		}

		/// <summary>
		/// Загрузить настройки по идентификатору источника <see cref="IMarketDataSource.Id"/>.
		/// </summary>
		/// <param name="id">Идентификатор источника.</param>
		/// <returns>Настройки.</returns>
		public MarketDataSourceSettings LoadBySourceId(Guid id)
		{
			return Read(_sourceIdField, id);
		}
	}
}