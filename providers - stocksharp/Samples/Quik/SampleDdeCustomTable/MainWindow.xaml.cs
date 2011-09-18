namespace SampleDdeCustomTable
{
	using System;
	using System.ComponentModel;
	using System.Linq;
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

		private readonly CandlesWindow _candlesWindow = new CandlesWindow();
		private DdeCustomTable _table;

		public MainWindow()
		{
			InitializeComponent();
			MainWindow.Instance = this;

			// попробовать сразу найти месторасположение Quik по запущенному процессу
			this.Path.Text = QuikTerminal.GetDefaultPath();
		}

		protected override void OnClosing(CancelEventArgs e)
		{
			_candlesWindow.RealClose = true;
			_candlesWindow.Close();

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

						// добавляем тип QuikCandle для преобразования строчки из таблица Исторические свечи в объект QuikCandle
						_table = new DdeCustomTable(typeof(QuikCandle));
						this.Trader.CustomTables.Add(_table);

						this.Trader.NewCustomTables += (type, objects) =>
						{
							// нас интересует только QuikCandle
							if (type == typeof(QuikCandle))
								_candlesWindow.Candles.AddRange(objects.Cast<QuikCandle>());
						};

						this.Trader.Connected += () => this.GuiAsync(() =>
						{
							this.ShowCandles.IsEnabled = true;
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

		private void ShowCandles_Click(object sender, RoutedEventArgs e)
		{
			ShowOrHide(_candlesWindow);
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
			this.Trader.StartExport(_table);
			_isDdeStarted = true;
		}

		private void StopDde()
		{
			this.Trader.StopExport(_table);
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