namespace SampleFewQuiks
{
	using System;
	using System.Collections.Generic;
	using System.Linq;
	using System.Threading;

	using StockSharp.BusinessEntities;
	using StockSharp.Quik;

	class Program
	{
		private volatile static Portfolio _portfolio1;
		private volatile static Portfolio _portfolio2;

		private volatile static Security _lkoh;
		private volatile static Security _riu0;

		static void Main()
		{
			try
			{
				Console.Write("Введите путь к директории Первого Квик: ");
				var path1 = Console.ReadLine();

				Console.Write("Введите номер счета, через который будет выставлена заявка в Первом Квике: ");
				var account1 = Console.ReadLine();

				Console.Write("Введите путь к директории Второго Квик: ");
				var path2 = Console.ReadLine();

				Console.Write("Введите номер счета, через который будет выставлена заявка во Втором Квике: ");
				var account2 = Console.ReadLine();

				using (var quikTrader1 = new QuikTrader(path1, "quik1"))
				using (var quikTrader2 = new QuikTrader(path2, "quik2", @"TRANS2QUIK_2.dll"))
					// если вторая dll находится в другой папке, то можно создать шлюз, указан путь к dll
					// var quikTrader2 = new QuikTrader(path2, "quik2", "Folder1\TRANS2QUIK_2.dll");
				{
					// подписываемся на событие ошибок обработки данных и разрыва соединения
					//
					quikTrader1.ProcessDataError += OnError;
					quikTrader2.ProcessDataError += OnError;

					quikTrader1.ConnectionError += OnError;
					quikTrader2.ConnectionError += OnError;

				
					var portfoliosWait = new ManualResetEvent(false);

					Action<IEnumerable<Portfolio>> newPortfolios = portfolios =>
					{
						if (_portfolio1 == null)
							_portfolio1 = portfolios.FirstOrDefault(p => p.Name == account1);

						if (_portfolio2 == null)
							_portfolio2 = portfolios.FirstOrDefault(p => p.Name == account2);

						// если оба инструмента появились
						if (_portfolio1 != null && _portfolio2 != null)
							portfoliosWait.Set();
					};

					// подписываемся на события новых портфелей
					quikTrader1.NewPortfolios += newPortfolios;
					quikTrader2.NewPortfolios += newPortfolios;


					var securitiesWait = new ManualResetEvent(false);

					Action<IEnumerable<Security>> newSecurities = securities =>
					{
						if (_lkoh == null)
							_lkoh = securities.FirstOrDefault(s => s.Code == "LKOH" && s.Trader == quikTrader1);

						if (_riu0 == null)
							_riu0 = securities.FirstOrDefault(s => s.Code == "RIU0" && s.Trader == quikTrader2);

						// если оба инструмента появились
						if (_lkoh != null && _riu0 != null)
							securitiesWait.Set();
					};

					// подписываемся на события новых инструментов
					quikTrader1.NewSecurities += newSecurities;
					quikTrader2.NewSecurities += newSecurities;


					// запускаем экспорты в Quik-ах, когда получим событие об успешном соединении
					//
					quikTrader1.Connected += () => quikTrader1.StartExport(quikTrader1.SecuritiesTable);
					quikTrader2.Connected += () => quikTrader2.StartExport(quikTrader2.SecuritiesTable);

					// производим подключение каждого из QuikTrader-а
					//
					quikTrader1.Connect();
					quikTrader2.Connect();

					Console.WriteLine("Дожидаемся появления инструментов и портфелей...");
					portfoliosWait.WaitOne();
					securitiesWait.WaitOne();

					Console.WriteLine("Информация появилась. Производим регистрацию заявок...");

					quikTrader1.RegisterOrder(new Order
					{
						Portfolio = _portfolio1,
						Volume = 1,
						Security = _lkoh,
						Price = _lkoh.BestBid.Price
					});
					Console.WriteLine("Заявка на LKOH зарегистрирована");

					quikTrader2.RegisterOrder(new Order
					{
						Portfolio = _portfolio2,
						Volume = 1,
						Security = _riu0,
						Price = _riu0.BestBid.Price
					});
					Console.WriteLine("Заявка на RIU0 зарегистрирована");
				}
			}
			catch (Exception ex)
			{
				Console.WriteLine(ex);
			}
		}

		private static void OnError(Exception error)
		{
			Console.WriteLine(error);
		}
	}
}