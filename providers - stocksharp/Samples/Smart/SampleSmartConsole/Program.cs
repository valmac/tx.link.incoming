namespace SampleSmartConsole
{
	using System;
	using System.Linq;
	using System.Threading;

	using Ecng.Collections;
	using Ecng.Common;

	using StockSharp.BusinessEntities;
	using StockSharp.Smart;
	using StockSharp.Algo;

	class Program
	{
		private static Security _lkoh;
		private static Portfolio _portfolio;

		static void Main()
		{
			try
			{
				// для теста выбираем бумагу Лукойл
				const string secCode = "LKOH";

				Console.Write("Введите логин: ");
				var login = Console.ReadLine();

				Console.Write("Введите пароль: ");
				var password = Console.ReadLine();

				Console.Write("Введите номер счета, через который будет выставлена заявка: ");
				var account = Console.ReadLine();

				using (var waitHandle = new AutoResetEvent(false))
				{
					// создаем шлюз к Smart-у
					using (var trader = new SmartTrader(login, password))
					{
						// подписываемся на событие успешного подключения
						// все действия необходимо производить только после подключения
						trader.Connected += () =>
						{
							Console.WriteLine("Подключение было произведено успешно.");

							// извещаем об успешном соединени
							waitHandle.Set();
						};

						Console.WriteLine("Производим подключение...");

						trader.Connect();

						// дожидаемся события об успешном соединении
						waitHandle.WaitOne();

						// подписываемся на все портфели-счета
						trader.NewPortfolios += portfolios =>
						{
							// необходимое условие работы в SmartCOM
							portfolios.ForEach(trader.RegisterPortfolio);

							if (_portfolio == null)
							{
								// находим Лукойл и присваиваем ее переменной lkoh
								_portfolio = portfolios.FirstOrDefault(p => p.Name == account);

								if (_portfolio != null)
								{
									Console.WriteLine("Портфель {0} появился.", account);

									if (_lkoh != null)
										waitHandle.Set();
								}
							}
						};

						// подписываемся на событие появление инструментов
						trader.NewSecurities += securities =>
						{
							if (_lkoh == null)
							{
								// находим Лукойл и присваиваем ее переменной lkoh
								_lkoh = securities.FirstOrDefault(sec => sec.Code == secCode && sec.Type == SecurityTypes.Equity);

								if (_lkoh != null)
								{
									Console.WriteLine("Инструмент Лукойл появился.");

									if (_portfolio != null)
										waitHandle.Set();
								}
							}
						};

						// подписываемся на событие появления моих новых сделок
						trader.NewMyTrades += myTrades =>
						{
							foreach (var myTrade in myTrades)
							{
								var trade = myTrade.Trade;
								Console.WriteLine("Сделка {0} по цене {1} по бумаге {2} по объему {3} в {4}.", trade.Id, trade.Price, trade.Security.Code, trade.Volume, trade.Time);
							}
						};

						Console.WriteLine("Дожидаемся появления в программе инструмента Лукойл и портфеля {0}...".Put(account));

						// запускаем экспорт по инструментам и портфелям
						trader.StartExport();

						// дожидаемся появления портфеля и инструмента
						waitHandle.WaitOne();

						trader.SecuritiesChanged += securities =>
						{
							// если инструмент хоть раз изменился (по нему пришли актуальные данные)
							if (securities.Contains(_lkoh))
								waitHandle.Set();
						};

						Console.WriteLine("Дожидаемся обновления данных по инструменту Лукойл...");

						// запускаем обновление по инструменту
						trader.RegisterSecurity(_lkoh);
						waitHandle.WaitOne();

						// 0.1% от изменения цены
						const decimal delta = 0.001m;

						// запоминаем первоначальное значение середины спреда
						var firstMid = _lkoh.BestPair.SpreadPrice / 2;
						Console.WriteLine("Первоначальное значение середины спреда {0:0.##}", _lkoh.BestBid.Price + firstMid);

						while (true)
						{
							var mid = _lkoh.BestPair.SpreadPrice / 2;

							// если спред вышел за пределы нашего диапазона
							if (
								((firstMid + firstMid * delta) <= mid) ||
								((firstMid - firstMid * delta) >= mid)
								)
							{
								var order = new Order
								{
									Portfolio = _portfolio,
									Price = _lkoh.ShrinkPrice(_lkoh.BestBid.Price + mid),
									Security = _lkoh,
									Volume = 1,
									Direction = OrderDirections.Buy,
								};
								trader.RegisterOrder(order);
								Console.WriteLine("Заявка {0} зарегистрирована.", order.Id);
								break;
							}
							else
								Console.WriteLine("Текущее значение середины спреда {0:0.##}", _lkoh.BestBid.Price + mid);

							// ждем 1 секунду
							Thread.Sleep(1000);
						}

						// останавливаем экспорт
						trader.StopExport();
					}
				}
			}
			catch (Exception ex)
			{
				Console.WriteLine(ex);
			}
		}
	}
}