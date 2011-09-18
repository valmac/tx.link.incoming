namespace StockSharp.Hydra.Core
{
	using System;
	using System.Collections.Generic;

	using Ecng.Collections;
	using Ecng.Common;

	using StockSharp.Algo.History;
	using StockSharp.BusinessEntities;

	/// <summary>
	/// Базовый класс для доступа к хранилищу информации об инструментах.
	/// </summary>
	public class SecurityStorage : ISecurityStorage
	{
		private readonly SynchronizedDictionary<string, Security> _cacheById = new SynchronizedDictionary<string, Security>();
		//private readonly SynchronizedDictionary<string, Security> _cacheByCode = new SynchronizedDictionary<string, Security>();

		/// <summary>
		/// Создать <see cref="SecurityStorage"/>.
		/// </summary>
		/// <param name="source">Интерфейс источника исторических маркет-данных.</param>
		public SecurityStorage(IMarketDataSource source)
		{
			if (source == null)
				throw new ArgumentNullException("source");

			Source = source;
		}

		/// <summary>
		/// Интерфейс источника исторических маркет-данных.
		/// </summary>
		protected IMarketDataSource Source { get; private set; }

		/// <summary>
		/// Загрузить информацию об инструменте из хранилища по полю и его значению.
		/// </summary>
		/// <param name="fieldName">Название поля, по которому необходимо загрузить информацию об инструменте.</param>
		/// <param name="fieldValue">Значение поля fieldName.</param>
		/// <returns>Информация об инструменте.</returns>
		public virtual Security LoadBy(string fieldName, object fieldValue)
		{
			if (_cacheById.Count == 0)
			{
				foreach (var security in Source.Storage.Securities)
					AddToCache(security);
			}

			switch (fieldName)
			{
				case "Id":
					var secId = ((string)fieldValue).ToLowerInvariant();
					return _cacheById.TryGetValue(secId);
				//case "Code":
				//    var secCode = ((string)fieldValue).ToLowerInvariant();
				//    return _cacheByCode.TryGetValue(secCode);
				default:
					throw new NotSupportedException("Неподдерживаемое поле {0}.".Put(fieldName));
			}
		}

		/// <summary>
		/// Получить все инструменты.
		/// </summary>
		public IEnumerable<Security> Securities
		{
			get { return Source.Storage.Securities; }
		}

		/// <summary>
		/// Сохранить информацию об инструменте в хранилище.
		/// </summary>
		/// <param name="security">Информация об инструменте.</param>
		public virtual void Save(Security security)
		{
			security.ExtensionInfo["Source"] = Source.Name;
			Source.Storage.Securities.Save(security);

			if (!_cacheById.ContainsKey(security.Id.ToLowerInvariant()))
				AddToCache(security);
		}

		private void AddToCache(Security security)
		{
			if (security == null)
				throw new ArgumentNullException("security");

			_cacheById.Add(security.Id.ToLowerInvariant(), security);
			//_cacheByCode.Add(security.Code.ToLowerInvariant(), security);
		}
	}
}