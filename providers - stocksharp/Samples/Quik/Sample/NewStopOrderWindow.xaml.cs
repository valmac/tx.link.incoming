namespace Sample
{
	using System;
	using System.Windows;
	using System.Windows.Controls;

	using Ecng.Common;
	using Ecng.ComponentModel;

	using StockSharp.Quik;
	using StockSharp.BusinessEntities;

	public partial class NewStopOrderWindow
	{
		private readonly bool _initialized;

		public NewStopOrderWindow()
		{
			InitializeComponent();
			this.Portfolio.Trader = MainWindow.Instance.Trader;
			_initialized = true;
			RefreshControls();
			this.OtherSecurities.ItemsSource = MainWindow.Instance.Trader.Securities;
			this.OtherSecurities.SelectedIndex = 0;
			this.ActiveTimeFrom.SelectedTime = new TimeSpan(10, 0, 0);
			this.ActiveTimeTo.SelectedTime = new TimeSpan(23, 50, 0);
			this.Offset.Unit = new Unit();
			this.Spread.Unit = new Unit();
		}

		public Security Security { get; set; }

		private Order _conditionOrder;

		public Order ConditionOrder
		{
			get { return _conditionOrder; }
			set
			{
				if (value == null)
					throw new ArgumentNullException("value");

				_conditionOrder = value;

				this.Security = value.Security;

				// типы стоп-заявок "со связанной заявкой" и "по другой бумаге" недоступны при стоп-заявке по исполнению
				this.LinkedOrderType.IsEnabled = this.OtherSecurityType.IsEnabled = false;

				// заявка условие всегда имеет противоположное направление
				this.IsBuy.IsChecked = (value.Direction == OrderDirections.Buy);

				// пользователь не может выбрать направление завки
				this.IsBuy.IsEnabled = this.IsSell.IsEnabled = false;

				// выбираем по умолчанию тип стоп-лимит
				this.StopOrderType.SelectedIndex = 1;
				
				this.PartiallyMatched.IsEnabled = this.UseMatchedBalance.IsEnabled = true;
				this.PartiallyMatched.IsChecked = this.UseMatchedBalance.IsChecked = true;
			}
		}

		private void Send_Click(object sender, RoutedEventArgs e)
		{
			Order stopOrder;

			// если это обычная стоп-заявка
			if (this.ConditionOrder == null)
			{
				switch (this.StopOrderType.SelectedIndex)
				{
					case 0:
						stopOrder = CreateLinkedOrder();
						break;
					case 1:
						stopOrder = CreateStopLimit();
						break;
					case 2:
						stopOrder = CreateOtherSecurity();
						break;
					case 3:
						stopOrder = CreateTakeProfit();
						break;
					case 4:
						stopOrder = CreateTakeProfitAndStopLimit();
						break;
					default:
						throw new InvalidOperationException("Выбран неизвестный тип стоп-заявки.");
				}
			}
			else // если это стоп-заявка "по исполнению"
			{
				switch (this.StopOrderType.SelectedIndex)
				{
					case 1:
						stopOrder = CreateConditionStopLimit();
						break;
					case 3:
						stopOrder = CreateConditionTakeProfit();
						break;
					case 4:
						stopOrder = CreateConditionTakeProfitAndStopLimit();
						break;
					default:
						throw new InvalidOperationException("Выбран неизвестный тип стоп-заявки.");
				}
			}

			stopOrder.Portfolio = this.Portfolio.SelectedPortfolio;

			MainWindow.Instance.Trader.RegisterOrder(stopOrder);
			base.DialogResult = true;
		}

		private Order CreateLinkedOrder()
		{
			return new Order
			{
				Type = OrderTypes.Conditional,
				Volume = this.Volume.Text.To<int>(),
				Price = this.Price.Text.To<decimal>(),
				Security = this.Security,
				Direction = this.IsBuy.IsChecked == true ? OrderDirections.Buy : OrderDirections.Sell,
				StopCondition = new QuikStopCondition
				{
					Type = QuikStopConditionTypes.LinkedOrder,
					ExpiryDate = this.ExpirationDate.DateTime,
					LinkedOrderPrice = this.LinkedOrderPrice.Text.To<decimal>(),
					LinkedOrderCancel = this.LinkedOrderCancel.IsChecked == true,
					StopPrice = this.StopPrice.Text.To<decimal>(),
					ActiveTime = this.ActiveTime,
				},
			};
		}

		private Order CreateStopLimit()
		{
			return new Order
			{
				Type = OrderTypes.Conditional,
				Volume = this.Volume.Text.To<int>(),
				Price = this.Price.Text.To<decimal>(),
				Security = this.Security,
				Direction = this.IsBuy.IsChecked == true ? OrderDirections.Buy : OrderDirections.Sell,
				StopCondition = new QuikStopCondition
				{
					Type = QuikStopConditionTypes.StopLimit,
					ExpiryDate = this.ExpirationDate.DateTime,
					StopPrice = this.StopPrice.Text.To<decimal>(),
					ActiveTime = this.ActiveTime,
				},
			};
		}

		private Order CreateOtherSecurity()
		{
			return new Order
			{
				Type = OrderTypes.Conditional,
				Volume = this.Volume.Text.To<int>(),
				Price = this.Price.Text.To<decimal>(),
				Security = this.Security,
				Direction = this.IsBuy.IsChecked == true ? OrderDirections.Buy : OrderDirections.Sell,
				StopCondition = new QuikStopCondition
				{
					Type = QuikStopConditionTypes.OtherSecurity,
					ExpiryDate = this.ExpirationDate.DateTime,
					StopPriceCondition = this.StopPriceCondition.Text == ">=" ? QuikStopPriceConditions.MoreOrEqual : QuikStopPriceConditions.LessOrEqual,
					StopPrice = this.StopPrice.Text.To<decimal>(),
					OtherSecurity = (Security)this.OtherSecurities.SelectedValue,
					ActiveTime = this.ActiveTime,
				},
			};
		}

		private Order CreateTakeProfit()
		{
			return new Order
			{
				Type = OrderTypes.Conditional,
				Volume = this.Volume.Text.To<int>(),
				Security = this.Security,
				Direction = this.IsBuy.IsChecked == true ? OrderDirections.Buy : OrderDirections.Sell,
				StopCondition = new QuikStopCondition
				{
					Type = QuikStopConditionTypes.TakeProfit,
					ExpiryDate = this.ExpirationDate.DateTime,
					StopPrice = this.StopPrice.Text.To<decimal>(),
					Offset = this.Offset.Unit,
					Spread = this.Spread.Unit,
					ActiveTime = this.ActiveTime,
				},
			};
		}

		private Order CreateTakeProfitAndStopLimit()
		{
			return new Order
			{
				Type = OrderTypes.Conditional,
				Volume = this.Volume.Text.To<int>(),
				Price = this.Price.Text.To<decimal>(),
				Security = this.Security,
				Direction = this.IsBuy.IsChecked == true ? OrderDirections.Buy : OrderDirections.Sell,
				StopCondition = new QuikStopCondition
				{
					Type = QuikStopConditionTypes.TakeProfitStopLimit,
					ExpiryDate = this.ExpirationDate.DateTime,
					StopPrice = this.StopPrice.Text.To<decimal>(),
					StopLimitPrice = this.StopLimitPrice.Text.To<decimal>(),
					Offset = this.Offset.Unit,
					Spread = this.Spread.Unit,
					ActiveTime = this.ActiveTime,
				},
			};
		}

		private Order CreateConditionStopLimit()
		{
			var stopLimit = CreateStopLimit();
			var condition = (QuikStopCondition)stopLimit.StopCondition;
			condition.ConditionOrder = this.ConditionOrder;
			condition.ConditionOrderPartiallyMatched = this.PartiallyMatched.IsChecked;
			condition.ConditionOrderUseMatchedBalance = this.UseMatchedBalance.IsChecked;
			return stopLimit;
		}

		private Order CreateConditionTakeProfit()
		{
			var stopLimit = CreateTakeProfit();
			var condition = (QuikStopCondition)stopLimit.StopCondition;
			condition.ConditionOrder = this.ConditionOrder;
			condition.ConditionOrderPartiallyMatched = this.PartiallyMatched.IsChecked;
			condition.ConditionOrderUseMatchedBalance = this.UseMatchedBalance.IsChecked;
			return stopLimit;
		}

		private Order CreateConditionTakeProfitAndStopLimit()
		{
			var stopLimit = CreateTakeProfitAndStopLimit();
			var condition = (QuikStopCondition)stopLimit.StopCondition;
			condition.ConditionOrder = this.ConditionOrder;
			condition.ConditionOrderPartiallyMatched = this.PartiallyMatched.IsChecked;
			condition.ConditionOrderUseMatchedBalance = this.UseMatchedBalance.IsChecked;
			return stopLimit;
		}

		private void StopOrderType_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			RefreshControls();
		}

		private void RefreshControls()
		{
			if (_initialized)
			{
				this.LinkedOrderCancel.IsEnabled = this.LinkedOrderPrice.IsEnabled = this.StopOrderType.SelectedIndex == 0;
				this.StopPriceCondition.IsEnabled = this.OtherSecurities.IsEnabled = this.StopOrderType.SelectedIndex == 2;
				this.Offset.IsUnitEnabled = this.Spread.IsUnitEnabled = this.StopOrderType.SelectedIndex == 3 || this.StopOrderType.SelectedIndex == 4;
				this.Price.IsEnabled = this.StopOrderType.SelectedIndex != 3;
				this.StopLimitPrice.IsEnabled = this.StopOrderType.SelectedIndex == 4;
			}
		}

		private Range<DateTime> ActiveTime
		{
			get
			{
				if (this.IsActiveTime.IsChecked == true)
					return new Range<DateTime>(DateTime.Today + this.ActiveTimeFrom.SelectedTime, DateTime.Today + this.ActiveTimeTo.SelectedTime);
				else
					return null;
			}
		}

		private void IsActiveTime_Checked(object sender, RoutedEventArgs e)
		{
			this.ActiveTimeFrom.IsEnabled = this.ActiveTimeTo.IsEnabled = this.IsActiveTime.IsChecked == true;
		}
	}
}