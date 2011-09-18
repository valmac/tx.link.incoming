namespace SampleStorage
{
	using System;
	using System.Collections.Generic;
	using System.Linq;

	using Ecng.Common;
	using Ecng.Serialization;

	using StockSharp.Algo.Storages;
	using StockSharp.Algo.Testing;
	using StockSharp.BusinessEntities;

	class Program
	{
		static void Main()
		{
			// создаем тестовый инструмент
			var security = new Security
			{
				Id = "TestId",
				MinStepSize = 0.1m,
				Decimals = 1,
			};

			var trades = new List<Trade>();

			// генерируем 1000 произвольных сделок
			//

			var tradeGenerator = new RandomWalkTradeGenerator(security)
			{
				IdGenerator = new IdGenerator
				{
					Current = DateTime.Now.Ticks
				},
				// стартовая цена
				LastTrade = new Trade
				{
					Price = 99
				}
			};

			for (var i = 0; i < 1000; i++)
			{
				var t = tradeGenerator.Generate(DateTime.Today + TimeSpan.FromMinutes(i));
				t.Id = i + 1;
				trades.Add(t);
			}

			var storage = new TradingStorage(new InMemoryStorage());

			// получаем хранилище для тиковых сделок
			var tradeStorage = storage.GetTradeStorage(security);

			// сохраняем сделки
			tradeStorage.Save(trades);

			// загружаем сделки
			Console.WriteLine("Количество загруженных сделок: " +
				tradeStorage.Load(DateTime.Today, DateTime.Today + TimeSpan.FromMinutes(1000)).Count());

			Console.ReadLine();

			// удаляем сделки (очищаем файл)
			tradeStorage.Delete(DateTime.Today, DateTime.Today + TimeSpan.FromMinutes(1000));
		}
	}
}