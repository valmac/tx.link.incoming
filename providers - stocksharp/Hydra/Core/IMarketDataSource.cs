namespace StockSharp.Hydra.Core
{
	using System;
	using System.Collections.Generic;

	using StockSharp.BusinessEntities;

	/// <summary>
	/// Интерфейс источника маркет-данных.
	/// </summary>
	public interface IMarketDataSource
	{
		/// <summary>
		/// Уникальный идентификатор.
		/// </summary>
		Guid Id { get; }

		/// <summary>
		/// Имя источника.
		/// </summary>
		string Name { get; }

		/// <summary>
		/// Адрес иконки, для визуального обозначения источника.
		/// </summary>
		Uri Icon { get; }

		/// <summary>
		/// Интерфейс для доступа к хранилищу Hydra.
		/// </summary>
		HydraStorage Storage { get; }

		/// <summary>
		/// Настройки источника <see cref="IMarketDataSource"/>.
		/// </summary>
		MarketDataSourceSettings Settings { get; }

		/// <summary>
		/// Сохранить настройки источника.
		/// </summary>
		void SaveSettings();

		/// <summary>
		/// Запустить загрузку данных.
		/// </summary>
		/// <param name="securities">Инструменты, для которых необходимо загрузить маркет-данные.</param>
		void Start(IEnumerable<Security> securities);

		/// <summary>
		/// Остановить загрузку данных.
		/// </summary>
		void Stop();

		/// <summary>
		/// Подтвердить успешное сохранения загруженных маркет-данных.
		/// </summary>
		/// <param name="security">Инструмент, для которого были загружены маркет-данные.</param>
		void CommitLoad(Security security);

		/// <summary>
		/// Вывести сообщение в лог.
		/// </summary>
		event Action<string> Log;
	}

	/// <summary>
	/// Интерфейс источника тиковых сделок.
	/// </summary>
	public interface ITradeSource : IMarketDataSource
	{
		/// <summary>
		/// Загрузить тиковые сделки.
		/// </summary>
		/// <param name="security">Инструмент, для которого необходимо загрузить тиковые сделки.</param>
		/// <returns>Тиковые сделки.</returns>
		IEnumerable<Trade> Load(Security security);

		/// <summary>
		/// Число сделок загруженных из источника.
		/// </summary>
		int TotalTrades { get; }
	}

	/// <summary>
	/// Интерфейс источника стаканов.
	/// </summary>
	public interface IMarketDepthSource : IMarketDataSource
	{
		/// <summary>
		/// Загрузить стаканы.
		/// </summary>
		/// <param name="security">Инструмент, для которого необходимо загрузить стаканы.</param>
		/// <returns>Стаканы.</returns>
		IEnumerable<MarketDepth> Load(Security security);

		/// <summary>
		/// Число стаканов загруженных из источника.
		/// </summary>
		int TotalDepths { get; }
	}
}