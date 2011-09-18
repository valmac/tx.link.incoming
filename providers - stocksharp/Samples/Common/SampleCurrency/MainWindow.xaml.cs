namespace SampleCurrency
{
	using System.Windows;

	using Ecng.Common;
	using Ecng.Xaml;

	using StockSharp.Algo;
	using StockSharp.BusinessEntities;

	public partial class MainWindow
	{
		public MainWindow()
		{
			InitializeComponent();

			this.SourceCurrencyType.SetDataSource<CurrencyTypes>();
			this.TargetCurrencyType.SetDataSource<CurrencyTypes>();

			this.SourceCurrencyType.SetSelectedValue<CurrencyTypes>(CurrencyTypes.USD);
			this.TargetCurrencyType.SetSelectedValue<CurrencyTypes>(CurrencyTypes.RUB);
		}

		private void Convert_Click(object sender, RoutedEventArgs e)
		{
			// создаем объект валюты Currency по введенным пользователям данным
			var currency = new Currency
			{
				Type = (CurrencyTypes)this.SourceCurrencyType.GetSelectedValue<CurrencyTypes>(),
				Value = this.Amount.Text.To<decimal>(),
			};

			// переводим в другую валюту и отображаем сконвертированное значение
			this.Result.Content = currency.Convert((CurrencyTypes)this.TargetCurrencyType.GetSelectedValue<CurrencyTypes>()).Value;
		}
	}
}