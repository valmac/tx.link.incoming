namespace SampleAsyncTransactions
{
	using System.Collections.Generic;
	using System.ComponentModel;
	using System.Windows;
	using System.Windows.Forms;
    using MessageBox = System.Windows.MessageBox;

	using Ecng.Collections;
	using Ecng.Common;
	using Ecng.Xaml;

	using StockSharp.Quik;
	using StockSharp.BusinessEntities;

	public partial class MainWindow
	{
		private bool _isDdeStarted;
		private readonly SecuritiesWindow _securitiesWindow = new SecuritiesWindow();

		public MainWindow()
		{
			InitializeComponent();
			Instance = this;

			// попробовать сразу найти месторасположение Quik по запущенному процессу
			this.Path.Text = QuikTerminal.GetDefaultPath();
		}

		public QuikTrader Trader { get; private set; }

		public static MainWindow Instance { get; private set; }

		public Portfolio Portfolio
		{
			get { return this.Portfolios.SelectedPortfolio; }
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

		private void Connect_Click(object sender, RoutedEventArgs e)
		{
			if (this.Path.Text.IsEmpty())
				MessageBox.Show(this, "Путь к Quik не выбран");
			else
			{
				this.Trader = new QuikTrader(this.Path.Text) { IsAsyncMode = true };

				this.Portfolios.Trader = this.Trader;

				this.Trader.Connected += () => this.GuiAsync(() => this.ExportDde.IsEnabled = true);
				this.Trader.NewSecurities += securities => this.GuiAsync(() => _securitiesWindow.Securities.AddRange(securities));

				// подписываемся на событие о неудачной регистрации заявок
				this.Trader.OrdersFailed += OrdersFailed;

				// подписываемся на событие о неудачной регистрации стоп-заявок
				this.Trader.StopOrdersFailed += OrdersFailed;

				// добавляем экспорт дополнительных колонок из стакана (своя покупка и продажа)
				this.Trader.QuotesTable.Columns.Add(DdeQuoteColumns.OwnBidVolume);
				this.Trader.QuotesTable.Columns.Add(DdeQuoteColumns.OwnAskVolume);

				this.Trader.Connect();

				this.ShowSecurities.IsEnabled = true;
				this.ConnectBtn.IsEnabled = false;
			}
		}

		private void OrdersFailed(IEnumerable<OrderFail> fails)
		{
			this.GuiAsync(() =>
			{
				foreach (var fail in fails)
					MessageBox.Show(this, fail.Error.ToString(), "Заявка не зарегистрирована");
			});
		}

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

		private void ShowSecurities_Click(object sender, RoutedEventArgs e)
		{
			if (_securitiesWindow.Visibility == Visibility.Visible)
				_securitiesWindow.Hide();
			else
				_securitiesWindow.Show();
		}
	}
}