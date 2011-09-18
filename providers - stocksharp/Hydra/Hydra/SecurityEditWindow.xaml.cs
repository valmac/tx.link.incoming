namespace StockSharp.Hydra
{
	using System;
	using System.Collections.Generic;
	using System.Linq;
	using System.Windows;

	using Ecng.Common;

	using StockSharp.BusinessEntities;
	using StockSharp.Hydra.Core;

	public partial class SecurityEditWindow
	{
		protected SecurityEditWindow()
		{
			InitializeComponent();
		}

		public SecurityEditWindow(HydraStorage storage)
			: this()
		{
			ExchangeCtrl.ItemsSource = storage.Exchanges.ToArray();
			ExchangeCtrl.SelectedIndex = 0;
		}

		private VisualSecurity _security;

		public VisualSecurity Security
		{
			get
			{
				return _security;
			}
			set
			{
				_security = value;

				var security = Security.Security;

				Code.Text = security.Code;
				SecName.Text = security.Name;
				Class.Text = security.Class;
				MinLotSize.Text = security.MinLotSize.ToString();
				MinStepSize.Text = security.MinStepSize.ToString();
				ExchangeCtrl.SelectedItem = security.Exchange;
			}
		}

		private IEnumerable<IMarketDataSource> _sources;

		public IEnumerable<IMarketDataSource> Sources
		{
			get { return _sources; }
			set
			{
				if (value == null)
					throw new ArgumentNullException("value");

				TradeSources.ItemsSource = new[] { string.Empty }.Concat(value.OfType<ITradeSource>().Select(l => l.Name));
				DepthSources.ItemsSource = new[] { string.Empty }.Concat(value.OfType<ITradeSource>().Select(l => l.Name));

				TradeSources.SelectedItem = Security.TradeSource;
				DepthSources.SelectedItem = Security.DepthSource;

				if (TradeSources.SelectedItem == null)
					TradeSources.SelectedItem = string.Empty;

				if (DepthSources.SelectedItem == null)
					DepthSources.SelectedItem = string.Empty;

				_sources = value;
			}
		}

		private void Ok_Click(object sender, RoutedEventArgs e)
		{
			var security = Security.Security;

			security.Code = Code.Text;
			security.Name = SecName.Text;
			security.Class = Class.Text;
			security.MinLotSize = MinLotSize.Text.To<int>();
			security.MinStepSize = MinStepSize.Text.To<decimal>();
			security.Exchange = (Exchange)ExchangeCtrl.SelectedItem;

			Security.TradeSource = (string)TradeSources.SelectedItem;
			Security.DepthSource = (string)DepthSources.SelectedItem;

			DialogResult = true;
			Close();
		}
	}
}