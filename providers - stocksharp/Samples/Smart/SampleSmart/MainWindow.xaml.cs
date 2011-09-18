namespace SampleSmart
{
	using System;
	using System.Collections.Generic;
	using System.ComponentModel;
	using System.Windows;
	using MessageBox = System.Windows.MessageBox;

	using Ecng.Collections;
	using Ecng.Common;
	using Ecng.Xaml;

	using StockSharp.BusinessEntities;
	using StockSharp.Smart;

	public partial class MainWindow
	{
		private bool _isConnected;

		public SmartTrader Trader;

		private readonly SecuritiesWindow _securitiesWindow = new SecuritiesWindow();
		private readonly TradesWindow _tradesWindow = new TradesWindow();
		private readonly MyTradesWindow _myTradesWindow = new MyTradesWindow();
		private readonly OrdersWindow _ordersWindow = new OrdersWindow();
		private readonly PortfoliosWindow _portfoliosWindow = new PortfoliosWindow();
		private readonly PositionsWindow _positionsWindow = new PositionsWindow();
		private readonly StopOrdersWindow _stopOrdersWindow = new StopOrdersWindow();

		public MainWindow()
		{
			InitializeComponent();
			MainWindow.Instance = this;
		}

		protected override void OnClosing(CancelEventArgs e)
		{
			_ordersWindow.RealClose = _myTradesWindow.RealClose =
			_tradesWindow.RealClose = _securitiesWindow.RealClose =
			_stopOrdersWindow.RealClose = _portfoliosWindow.RealClose = 
			_positionsWindow.RealClose = true;
			
			_securitiesWindow.Close();
			_tradesWindow.Close();
			_myTradesWindow.Close();
			_stopOrdersWindow.Close();
			_ordersWindow.Close();
			_portfoliosWindow.Close();
			_positionsWindow.Close();

			if (this.Trader != null)
				this.Trader.Dispose();

			base.OnClosing(e);
		}

		public static MainWindow Instance { get; private set; }

		private void Connect_Click(object sender, RoutedEventArgs e)
		{
			if (!_isConnected)
			{
				if (this.Trader == null)
				{
					if (this.Login.Text.IsEmpty())
					{
						MessageBox.Show(this, "Не указан логин.");
						return;
					}
					else if (this.Password.Password.IsEmpty())
					{
						MessageBox.Show(this, "Не указан пароль.");
						return;
					}

					// создаем шлюз
					this.Trader = new SmartTrader(this.Login.Text, this.Password.Password, this.Address.SelectedAddress);

					// очищаем из текстового поля в целях безопасности
					this.Password.Clear();

					// инициализируем механизм переподключения (будет автоматически соединяться
					// каждые 10 секунд, если шлюз потеряется связь с сервером)
					this.Trader.ReConnectionSettings.Interval = TimeSpan.FromSeconds(10);
					this.Trader.ReConnectionSettings.WorkingTime = Exchange.Rts.WorkingTime;
					this.Trader.ReConnectionSettings.ConnectionRestored += () => this.GuiAsync(() =>
					{
						// разблокируем кнопку Экспорт (соединение было восстановлено)
						ChangeConnectStatus(true);
						MessageBox.Show(this, "Соединение восстановлено.");
					});

					// подписываемся на событие успешного соединения
					this.Trader.Connected += () =>
					{
						// возводим флаг, что соединение установлено
						_isConnected = true;

						// разблокируем кнопку Экспорт
						this.GuiAsync(() => ChangeConnectStatus(true));
					};

					// подписываемся на событие разрыва соединения
					this.Trader.ConnectionError += error => this.GuiAsync(() =>
					{
						// заблокируем кнопку Экспорт (так как соединение было потеряно)
						ChangeConnectStatus(false);

						MessageBox.Show(this, error.ToString(), "Ошибка соединения");	
					});

					this.Trader.ProcessDataError += error => this.GuiAsync(() => MessageBox.Show(this, error.ToString(), "Ошибка обработки данных"));

					this.Trader.NewSecurities += securities => this.GuiAsync(() => _securitiesWindow.AddSecurities(securities));
					this.Trader.NewMyTrades += trades => this.GuiAsync(() => _myTradesWindow.Trades.AddRange(trades));
					this.Trader.NewTrades += trades => this.GuiAsync(() => _tradesWindow.Trades.AddRange(trades));
					this.Trader.NewOrders += orders => this.GuiAsync(() => _ordersWindow.Orders.AddRange(orders));
					this.Trader.NewStopOrders += orders => this.GuiAsync(() => _stopOrdersWindow.Orders.AddRange(orders));
					this.Trader.NewPortfolios += portfolios =>
					{
						// регистрирует портфели на обновление данных
						portfolios.ForEach(this.Trader.RegisterPortfolio);

						_portfoliosWindow.Portfolios.AddRange(portfolios);
					};
					this.Trader.NewPositions += positions => this.GuiAsync(() => _positionsWindow.Positions.AddRange(positions));

					// подписываемся на событие о неудачной регистрации заявок
					this.Trader.OrdersFailed += OrdersFailed;

					// подписываемся на событие о неудачной регистрации стоп-заявок
					this.Trader.StopOrdersFailed += OrdersFailed;

					this.ShowSecurities.IsEnabled = this.ShowTrades.IsEnabled =
					this.ShowMyTrades.IsEnabled = this.ShowOrders.IsEnabled = 
					this.ShowPortfolios.IsEnabled = this.ShowStopOrders.IsEnabled = true;
				}

				this.Trader.Connect();
			}
			else
			{
				this.Trader.Disconnect();
			}
		}

		private void OrdersFailed(IEnumerable<OrderFail> fails)
		{
			this.GuiAsync(() =>
			{
				foreach (var fail in fails)
					MessageBox.Show(this, fail.Error.ToString(), "Ошибка регистрации заявки");
			});
		}

		private void ChangeConnectStatus(bool isConnected)
		{
			_isConnected = isConnected;
			this.ConnectBtn.Content = isConnected ? "Отключиться" : "Подключиться";
			this.Export.IsEnabled = isConnected;
		}

		private void ShowSecurities_Click(object sender, RoutedEventArgs e)
		{
			ShowOrHide(_securitiesWindow);
		}

		private void ShowTrades_Click(object sender, RoutedEventArgs e)
		{
			ShowOrHide(_tradesWindow);
		}

		private void ShowMyTrades_Click(object sender, RoutedEventArgs e)
		{
			ShowOrHide(_myTradesWindow);
		}

		private void ShowOrders_Click(object sender, RoutedEventArgs e)
		{
			ShowOrHide(_ordersWindow);
		}

		private void ShowPortfolios_Click(object sender, RoutedEventArgs e)
		{
			ShowOrHide(_portfoliosWindow);
			ShowOrHide(_positionsWindow);
		}

		private void ShowStopOrders_Click(object sender, RoutedEventArgs e)
		{
			ShowOrHide(_stopOrdersWindow);
		}

		private static void ShowOrHide(Window window)
		{
			if (window == null)
				throw new ArgumentNullException("window");

			if (window.Visibility == Visibility.Visible)
				window.Hide();
			else
				window.Show();
		}

		private void Export_Click(object sender, RoutedEventArgs e)
		{
			this.Trader.StartExport();
		}
	}
}
