namespace Sample
{
	using System;
	using System.ComponentModel;
	using System.Windows;
	using System.Windows.Forms;
	using MessageBox = System.Windows.MessageBox;

	using Ecng.Collections;
	using Ecng.Common;
	using Ecng.Xaml;

	using StockSharp.BusinessEntities;
	using StockSharp.Quik;

	public partial class MainWindow
	{
		public QuikTrader Trader;

		private readonly SecuritiesWindow _securitiesWindow = new SecuritiesWindow();
		private readonly TradesWindow _tradesWindow = new TradesWindow();
		private readonly MyTradesWindow _myTradesWindow = new MyTradesWindow();
		private readonly OrdersWindow _ordersWindow = new OrdersWindow();
		private readonly PortfoliosWindow _portfoliosWindow = new PortfoliosWindow();
		private readonly PositionsWindow _positionsWindow = new PositionsWindow();
		private readonly StopOrderWindow _stopOrderWindow = new StopOrderWindow();

		public MainWindow()
		{
			InitializeComponent();
			MainWindow.Instance = this;

			// попробовать сразу найти месторасположение Quik по запущенному процессу
			this.Path.Text = QuikTerminal.GetDefaultPath();
		}

		protected override void OnClosing(CancelEventArgs e)
		{
			_ordersWindow.RealClose = _myTradesWindow.RealClose =
			_tradesWindow.RealClose = _securitiesWindow.RealClose =
			_stopOrderWindow.RealClose = _positionsWindow.RealClose = 
			_portfoliosWindow.RealClose = true;
			
			_securitiesWindow.Close();
			_tradesWindow.Close();
			_myTradesWindow.Close();
			_stopOrderWindow.Close();
			_ordersWindow.Close();
			_positionsWindow.Close();
			_portfoliosWindow.Close();

			if (this.Trader != null)
			{
				if (_isDdeStarted)
					StopDde();

				this.Trader.Dispose();
			}

			base.OnClosing(e);
		}

		public static MainWindow Instance { get; private set; }

		private void FindPath_Click(object sender, RoutedEventArgs e)
		{
			var dlg = new FolderBrowserDialog();

			if (!this.Path.Text.IsEmpty())
				dlg.SelectedPath = this.Path.Text;

			if (dlg.ShowDialog() == System.Windows.Forms.DialogResult.OK)
			{
				this.Path.Text = dlg.SelectedPath;
			}
		}

		private bool _isConnected;

		private void Connect_Click(object sender, RoutedEventArgs e)
		{
			if (!_isConnected)
			{
				if (this.Path.Text.IsEmpty())
					MessageBox.Show(this, "Путь к Quik не выбран");
				else
				{
					if (this.Trader == null)
					{
						// создаем шлюз
						this.Trader = new QuikTrader(this.Path.Text);

						// возводим флаг, что соединение установлено
						_isConnected = true;

						// инициализируем механизм переподключения (будет автоматически соединяться
						// каждые 10 секунд, если шлюз потеряется связь с сервером)
						this.Trader.ReConnectionSettings.Interval = TimeSpan.FromSeconds(10);

						// переподключение будет работать только во время работы биржи РТС
						// (чтобы отключить переподключение когда торгов нет штатно, например, ночью)
						this.Trader.ReConnectionSettings.WorkingTime = Exchange.Rts.WorkingTime;

						// подписываемся на событие об успешном восстановлении соединения
						this.Trader.ReConnectionSettings.ConnectionRestored += () => this.GuiAsync(() => MessageBox.Show(this, "Соединение восстановлено"));

						// подписываемся на событие разрыва соединения

						this.Trader.ConnectionError += error => this.GuiAsync(() => MessageBox.Show(this, error.ToString()));

						this.Trader.NewSecurities += securities => this.GuiAsync(() => _securitiesWindow.Securities.AddRange(securities));
						this.Trader.NewMyTrades += trades => this.GuiAsync(() => _myTradesWindow.Trades.AddRange(trades));
						this.Trader.NewTrades += trades => this.GuiAsync(() => _tradesWindow.Trades.AddRange(trades));
						this.Trader.NewOrders += orders => this.GuiAsync(() => _ordersWindow.Orders.AddRange(orders));
						this.Trader.NewStopOrders += orders => this.GuiAsync(() => _stopOrderWindow.Orders.AddRange(orders));
						this.Trader.NewPortfolios += portfolios => this.GuiAsync(() => _portfoliosWindow.Portfolios.AddRange(portfolios));
						this.Trader.NewPositions += positions => this.GuiAsync(() => _positionsWindow.Positions.AddRange(positions));
						this.Trader.ProcessDataError += ex => System.Diagnostics.Debug.WriteLine(ex);
						this.Trader.Connected += () => this.GuiAsync(() => this.ExportDde.IsEnabled = true);

						this.ShowSecurities.IsEnabled = this.ShowTrades.IsEnabled =
						this.ShowMyTrades.IsEnabled = this.ShowOrders.IsEnabled =
						this.ShowPortfolios.IsEnabled = this.ShowStopOrders.IsEnabled = true;
					}

					this.Trader.Connect();

					_isConnected = true;
					this.ConnectBtn.Content = "Отключиться";
				}
			}
			else
			{
				this.Trader.Disconnect();

				_isConnected = false;
				this.ConnectBtn.Content = "Подключиться";
			}
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
			ShowOrHide(_stopOrderWindow);
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

		private bool _isDdeStarted;

		private void StartDde()
		{
			this.Trader.StartExport();
			_isDdeStarted = true;
		}

		private void StopDde()
		{
			this.Trader.StopExport();
			_isDdeStarted = false;
		}

		private void ExportDde_Click(object sender, RoutedEventArgs e)
		{
			if (_isDdeStarted)
				StopDde();
			else
				StartDde();
		}
	}
}
