namespace SampleDdeExtendedInfo
{
	using System;
	using System.ComponentModel;
	using System.Windows;
	using System.Windows.Forms;
	using MessageBox = System.Windows.MessageBox;

	using Ecng.Collections;
	using Ecng.Common;
	using Ecng.Xaml;

	using StockSharp.Quik;

	public partial class MainWindow
	{
		public QuikTrader Trader;

		private readonly SecuritiesWindow _securitiesWindow = new SecuritiesWindow();

		public MainWindow()
		{
			InitializeComponent();
			MainWindow.Instance = this;

			// попробовать сразу найти месторасположение Quik по запущенному процессу
			this.Path.Text = QuikTerminal.GetDefaultPath();
		}

		protected override void OnClosing(CancelEventArgs e)
		{
			_securitiesWindow.RealClose = true;
			_securitiesWindow.Close();

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

						// подписываемся на событие ошибки соединения
						this.Trader.ConnectionError += error => this.GuiAsync(() => MessageBox.Show(this, error.ToString()));

						this.Trader.NewSecurities += securities => this.GuiAsync(() => _securitiesWindow.Securities.AddRange(securities));

						this.Trader.ProcessDataError += error => System.Diagnostics.Debug.WriteLine(error);

						// добавляем на экспорт необходимые колонки
						this.Trader.SecuritiesTable.Columns.Add(DdeSecurityColumns.Volatility);
						this.Trader.SecuritiesTable.Columns.Add(DdeSecurityColumns.TheorPrice);
						this.Trader.SecuritiesTable.Columns.Add(DdeSecurityColumns.UnderlyingSecurity);
						this.Trader.SecuritiesTable.Columns.Add(DdeSecurityColumns.MinStepPrice);

						// добавляем экспорт дополнительных колонок из стакана (своя продажа и покупка)
						this.Trader.QuotesTable.Columns.Add(DdeQuoteColumns.OwnAskVolume);
						this.Trader.QuotesTable.Columns.Add(DdeQuoteColumns.OwnBidVolume);

						this.Trader.Connected += () => this.GuiAsync(() =>
						{
							this.ShowSecurities.IsEnabled = true;
							this.ExportDde.IsEnabled = true;

							_isConnected = true;
							this.ConnectBtn.Content = "Отключиться";
						});

						this.Trader.Disconnected += () => this.GuiAsync(() =>
						{
							_isConnected = false;
							this.ConnectBtn.Content = "Подключиться";
						});
					}
					
					this.Trader.Connect();
				}
			}
			else
				this.Trader.Disconnect();
		}

		private void ShowSecurities_Click(object sender, RoutedEventArgs e)
		{
			ShowOrHide(_securitiesWindow);
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
			this.Trader.StartExport(this.Trader.SecuritiesTable);
			_isDdeStarted = true;
		}

		private void StopDde()
		{
			this.Trader.StopExport(this.Trader.SecuritiesTable);
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