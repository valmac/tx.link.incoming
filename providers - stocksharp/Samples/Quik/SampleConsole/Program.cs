namespace SampleConsole
{
	using System;
	using System.Linq;
	using System.Threading;

	using Ecng.Common;

	using StockSharp.BusinessEntities;
	using StockSharp.Quik;
	using StockSharp.Algo;

	class Program
	{
		private static Security _lkoh;
		private static Portfolio _portfolio;
		private static MarketDepth _depth;

		static void Main()
		{
			try
			{
				// для теста выбираем бумагу Лукойл
				const string secCode = "LKOH";

				var quikPath = QuikTerminal.GetDefaultPath();

				if (quikPath.IsEmpty())
				{
					Console.WriteLine("Не найден ни один запущенный Quik");
					return;
				}

				Console.WriteLine("Запущенный Quik найден по пути " + quikPath);

				Console.Write("Введите номер счета, через который будет выставлена заявка: ");
				var account = Console.ReadLine();

				using (var waitHandle = new AutoResetEvent(false))
				{
					// создаем шлюз к Quik-у
					using (var trader = new QuikTrader(quikPath))
					{
						// необходимо раскомментировать, если идет работа с РТС Стандарт
						//trader.FormatTransaction += builder => builder.RemoveInstruction(TransactionBuilder.ExecutionCondition);

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

						trader.NewPortfolios += portfolios =>
						{
							if (_portfolio == null)
							{
								// находим Лукойл и присваиваем ее переменной lkoh
								_portfolio = portfolios.FirstOrDefault(p => p.Name == account);

								if (_portfolio != null)
								{
									Console.WriteLine("Портфель {0} появился.", account);

									// если инструмент и стакан уже появились,
									// то извещаем об этом основной поток для выставления заявки
									if (_lkoh != null && _depth != null)
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
								_lkoh = securities.FirstOrDefault(sec => sec.Code == secCode);

								if (_lkoh != null)
								{
									Console.WriteLine("Инструмент Лукойл появился.");

									// запускаем экспорт стакана
									trader.RegisterQuotes(_lkoh);

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

						// подписываемся на событие обновления стакана
						trader.QuotesChanged += depths =>
						{
							if (_depth == null && _lkoh != null)
							{
								_depth = depths.FirstOrDefault(d => d.Security == _lkoh);

								if (_depth != null)
								{
									Console.WriteLine("Стакан Лукойла появился.");

									// если портфель уже появился, то извещаем об этом основной поток для выставления заявки
									if (_portfolio != null)
										waitHandle.Set();
								}
							}
						};

						Console.WriteLine("Дожидаемся появления в программе инструмента Лукойл и портфеля {0}...".Put(account));

						// запускаем экспорт по DDE
						trader.StartExport(trader.SecuritiesTable, trader.MyTradesTable, trader.EquityPositionsTable);

						// дожидаемся появления портфеля и инструмента
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
							if	(
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

						// останавливаем экспорт по DDE
						trader.StopExport(trader.SecuritiesTable, trader.MyTradesTable, trader.EquityPositionsTable);
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