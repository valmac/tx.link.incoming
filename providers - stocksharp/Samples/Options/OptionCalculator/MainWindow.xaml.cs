namespace OptionCalculator
{
	using System.ComponentModel;
	using System.Linq;
	using System.Windows;
	using System.Windows.Controls;
	using MessageBox = System.Windows.MessageBox;
	using System.Windows.Forms;

	using Ecng.Collections;
	using Ecng.Common;
	using Ecng.Xaml;

	using StockSharp.BusinessEntities;
	using StockSharp.Quik;
	using StockSharp.Algo;

	public partial class MainWindow
	{
		private readonly ThreadSafeObservableCollection<Security> _options = new ThreadSafeObservableCollection<Security>();
		private QuikTrader _trader;

		public MainWindow()
		{
			InitializeComponent();

			this.Options.ItemsSource = _options;

			// попробовать сразу найти месторасположение Quik по запущенному процессу
			this.Path.Text = QuikTerminal.GetDefaultPath();
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

		protected override void OnClosing(CancelEventArgs e)
		{
			if (_trader != null)
			{
				_trader.Dispose();
			}

			base.OnClosing(e);
		}

		private void Connect_Click(object sender, RoutedEventArgs e)
		{
			if (this.Path.Text.IsEmpty())
				MessageBox.Show(this, "Путь к Quik не выбран");
			else
			{
				if (_trader == null)
				{
					// создаем шлюз
					_trader = new QuikTrader(this.Path.Text);

					// изменяем метаданные так, чтобы начали обрабатывать дополнительные колонки опционов
					var columns = _trader.SecuritiesTable.Columns;
					columns.Add(DdeSecurityColumns.Strike);
					columns.Add(DdeSecurityColumns.Volatility);
					columns.Add(DdeSecurityColumns.UnderlyingSecurity);
					columns.Add(DdeSecurityColumns.TheorPrice);
					columns.Add(DdeSecurityColumns.OptionType);
					columns.Add(DdeSecurityColumns.ExpiryDate);

					// добавляем в выпадающий список только опционы
					_trader.NewSecurities += securities =>
						this.GuiAsync(() => _options.AddRange(securities.Where(s => s.Type == SecurityTypes.Option)));

					// подписываемся на событие новых сделок чтобы обновить текущую цену фьючерса
					_trader.NewTrades += trades => this.GuiAsync(() =>
					{
						var option = this.SelectedOption;
						if (option != null)
							this.BaseActivePrice.Text = option.GetUnderlyingFuture().LastTrade.Price.ToString();
					});

					_trader.StartExport(_trader.SecuritiesTable, _trader.TradesTable);
				}
			}
		}

		private Security SelectedOption
		{
			get { return (Security)this.Options.SelectedItem; }
		}

		private void Calculate_Click(object sender, RoutedEventArgs e)
		{
			var option = this.SelectedOption;

			var volatility = this.Volatility.Text.To<decimal>() / 100;

			this.Delta.Text = option.Delta(volatility).ToString("0.000");
			this.Gamma.Text = option.Gamma(volatility).ToString("0.000000");
			this.Vega.Text = option.Vega(volatility).ToString("0.00");
			this.Theta.Text = option.Theta(volatility).ToString("0.00");
			this.Rho.Text = option.Rho(volatility).ToString("0.00");
			this.IV.Text = option.IV(option.LastTrade.Price).ToString("0.00");
		}

		private void Options_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			var option = this.SelectedOption;

			if (option != null)
			{
				this.Volatility.Text = option.Volatility.ToString();
				this.BaseActivePrice.Text = option.GetUnderlyingFuture().LastTrade.Price.ToString();
			}

			this.Calculate.IsEnabled = option != null;
		}
	}
}