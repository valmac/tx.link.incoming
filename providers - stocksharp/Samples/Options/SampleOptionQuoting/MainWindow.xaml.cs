namespace SampleOptionQuoting
{
	using System.ComponentModel;
	using System.Linq;
	using System.Windows;
	using System.Windows.Controls;
	using System.Windows.Forms;
	using MessageBox = System.Windows.MessageBox;

	using Ecng.ComponentModel;
	using Ecng.Common;
	using Ecng.Xaml;
	using Ecng.Collections;

	using StockSharp.BusinessEntities;
	using StockSharp.Quik;
	using StockSharp.Algo;
	using StockSharp.Algo.Strategies;

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

					this.Portfolio.Trader = _trader;

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

					_trader.Connected += () => _trader.StartExport();
					_trader.Connect();
				}
			}
		}

		private Security SelectedOption
		{
			get { return (Security)this.Options.SelectedItem; }
		}

		private void Options_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			var option = this.SelectedOption;

			if (option != null)
			{
				this.Volatility.Text = option.Volatility.ToString();
				this.BaseActivePrice.Text = option.GetUnderlyingFuture().LastTrade.Price.ToString();
				this.VolatilityMin.Text = this.VolatilityMax.Text = this.Volatility.Text;
			}

			this.Start.IsEnabled = option != null;
		}

		private void Start_Click(object sender, RoutedEventArgs e)
		{
			var option = this.SelectedOption;

			// создаем окно для отображения стакана
			var wnd = new QuotesWindow { Title = option.Name };
			wnd.Init(option);

			// создаем котирование на покупку 20-ти контрактов
			var quoting = new VolatilityQuotingStrategy(
					new Range<decimal>(this.VolatilityMin.Text.To<decimal>(), this.VolatilityMax.Text.To<decimal>()),
					OrderDirections.Buy, 20)
			{
				// указываем, что котирование работает с объемом в 1 контракт
				Volume = 1,
				Security = option,
				Portfolio = this.Portfolio.SelectedPortfolio,
				Trader = _trader,
			};

			var basket = new BasketStrategy(quoting);

			// запускаем котирование
			basket.Start();

			// создаем дельта-хеджирование, передав в него опционные стратегии для отслеживания их позиции
			var hedge = new DeltaHedgeStrategy(basket)
			{
				Security = option.GetUnderlyingFuture(),
				Portfolio = this.Portfolio.SelectedPortfolio,
				Trader = _trader,
			};

			// запускаем дельта-хеджирование
			hedge.Start();
			 
			// показываем окно
			wnd.ShowModal(this);

			hedge.Stop();
			basket.Stop();
		}
	}
}