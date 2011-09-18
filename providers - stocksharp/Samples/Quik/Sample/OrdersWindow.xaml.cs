namespace Sample
{
	using System.Collections.ObjectModel;
	using System.ComponentModel;
	using System.Windows;
	using System.Windows.Controls;

	using Ecng.Common;
	using Ecng.Xaml;

	using StockSharp.BusinessEntities;

	public partial class OrdersWindow
	{
		public OrdersWindow()
		{
			this.Orders = new ObservableCollection<Order>();
			InitializeComponent();
		}

		protected override void OnClosing(CancelEventArgs e)
		{
			if (!this.RealClose)
			{
				base.Hide();
				e.Cancel = true;
			}

			base.OnClosing(e);
		}

		public bool RealClose { get; set; }

		public ObservableCollection<Order> Orders { get; private set; }

		private Order SelectedOrder
		{
			get { return OrdersDetails.SelectedValue as Order; }
		}

		private void CancelOrder_Click(object sender, RoutedEventArgs e)
		{
			MainWindow.Instance.Trader.CancelOrder(this.SelectedOrder);
		}

		private void CancelGroupOrders_Click(object sender, RoutedEventArgs e)
		{
			MainWindow.Instance.Trader.CancelOrders(null, null, null, null, null);
		}

		private void OrdersDetails_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			var order = SelectedOrder;
			this.ExecConditionOrder.IsEnabled = this.CancelOrder.IsEnabled = (order != null && order.State == OrderStates.Active);
		}

		private void ExecConditionOrder_Click(object sender, RoutedEventArgs e)
		{
			var order = SelectedOrder;

			var newOrder = new NewStopOrderWindow
			{
				Title = "Новая условная заявка на исполнение заявки '{0}'".Put(order.Id),
				ConditionOrder = order,
			};
			newOrder.ShowModal(this);
		}
	}
}