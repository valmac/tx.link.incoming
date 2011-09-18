namespace SampleQuikSmart
{
	using System;
	using System.Collections.Generic;
	using System.ComponentModel;
	using System.Windows;
	using System.Windows.Forms;
	using MessageBox = System.Windows.MessageBox;

	using Ecng.Collections;
	using Ecng.Common;
	using Ecng.Xaml;

	using StockSharp.BusinessEntities;
	using StockSharp.Quik;
	using StockSharp.Smart;
	using StockSharp.Algo;

	public partial class MainWindow
	{
		private bool _isConnected;

		public MultiTrader Trader;

		private readonly SecuritiesWindow _securitiesWindow = new SecuritiesWindow();
		private readonly OrdersWindow _ordersWindow = new OrdersWindow();

		public MainWindow()
		{
			InitializeComponent();
			MainWindow.Instance = this;

			// попробовать сразу найти месторасположение Quik по запущенному процессу
			this.QuikPath.Text = QuikTerminal.GetDefaultPath();
		}

		protected override void OnClosing(CancelEventArgs e)
		{
			_ordersWindow.RealClose = _securitiesWindow.RealClose = true;
			
			_securitiesWindow.Close();
			_ordersWindow.Close();

			if (this.Trader != null)
				this.Trader.Dispose();

			base.OnClosing(e);
		}

		private void FindQuikPath_Click(object sender, RoutedEventArgs e)
		{
			var dlg = new FolderBrowserDialog();

			if (!this.QuikPath.Text.IsEmpty())
				dlg.SelectedPath = this.QuikPath.Text;

			if (dlg.ShowDialog() == System.Windows.Forms.DialogResult.OK)
			{
				this.QuikPath.Text = dlg.SelectedPath;
			}
		}

		private BaseTrader InitReconnectionSettings(BaseTrader trader)
		{
			// инициализируем механизм переподключения (будет автоматически соединяться
			// каждые 10 секунд, если шлюз потеряется связь с сервером)
			trader.ReConnectionSettings.Interval = TimeSpan.FromSeconds(10);
			trader.ReConnectionSettings.WorkingTime = Exchange.Rts.WorkingTime;
			trader.ReConnectionSettings.ConnectionRestored += () => this.GuiAsync(() =>
			{
				// разблокируем кнопку Экспорт (соединение было восстановлено)
				ChangeConnectStatus(true);
				MessageBox.Show(this, "Соединение восстановлено.");
			});

			return trader;
		}

		public static MainWindow Instance { get; private set; }

		private void Connect_Click(object sender, RoutedEventArgs e)
		{
			if (!_isConnected)
			{
				if (this.Trader == null)
				{
					if (this.SmartLogin.Text.IsEmpty())
					{
						MessageBox.Show(this, "Не указан логин Smart.");
						return;
					}
					else if (this.SmartPassword.Password.IsEmpty())
					{
						MessageBox.Show(this, "Не указан пароль Smart.");
						return;
					}
					else if (this.QuikPath.Text.IsEmpty())
					{
						MessageBox.Show(this, "Не указан путь Quik.");
						return;
					}

					// создаем агрегирующий шлюз
					this.Trader = new MultiTrader();

					// создаем шлюзы к SmartCOM и Quik (+ сразу инициализируем настройки переподключения)
					var smartTrader = InitReconnectionSettings(new SmartTrader(this.SmartLogin.Text, this.SmartPassword.Password, this.SmartAddress.SelectedAddress));
					var quikTrader = InitReconnectionSettings(new QuikTrader(this.QuikPath.Text));

					// добавляем шлюзы к SmartCOM и Quik
					this.Trader.AggregatedTraders.Add(smartTrader);
					this.Trader.AggregatedTraders.Add(quikTrader);

					// очищаем из текстового поля в целях безопасности
					this.SmartPassword.Clear();

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
					this.Trader.NewOrders += orders => this.GuiAsync(() => _ordersWindow.Orders.AddRange(orders));

					// подписываемся на событие о неудачной регистрации заявок
					this.Trader.OrdersFailed += OrdersFailed;

					// подписываемся на событие о неудачной регистрации стоп-заявок
					this.Trader.StopOrdersFailed += OrdersFailed;

					this.ShowSecurities.IsEnabled = this.ShowOrders.IsEnabled = true;
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

		private void ShowOrders_Click(object sender, RoutedEventArgs e)
		{
			ShowOrHide(_ordersWindow);
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