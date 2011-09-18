namespace StockSharp.Hydra
{
	using System;
	using System.Collections.Generic;
	using System.Linq;
	using System.Windows;

	using StockSharp.Hydra.Core;

	public partial class SecuritySelectWindow
	{
		public SecuritySelectWindow()
		{
			InitializeComponent();
		}

		public VisualSecurity Security { get; set; }

		private IEnumerable<IMarketDataSource> _sources;

		public IEnumerable<IMarketDataSource> Sources
		{
			get { return _sources; }
			set
			{
				if (value == null)
					throw new ArgumentNullException("value");

				this.TradeSources.ItemsSource = this.DepthSources.ItemsSource = new[] { string.Empty }.Concat(value.Select(l => l.Name));

				this.TradeSources.SelectedItem = this.Security.TradeSource;
				this.DepthSources.SelectedItem = this.Security.DepthSource;

				_sources = value;
			}
		}

		private void Ok_Click(object sender, RoutedEventArgs e)
		{
			this.Security.TradeSource = (string)this.TradeSources.SelectedItem;
			this.Security.DepthSource = (string)this.DepthSources.SelectedItem;
			base.DialogResult = true;
			base.Close();
		}
	}
}