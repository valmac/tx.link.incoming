namespace StockSharp.Hydra
{
	using System;
	using System.Collections.Generic;
	using System.IO;
	using System.Linq;
	using System.Windows;
	using System.Windows.Controls;
	using System.Xml.Linq;

	using Antlr3.ST;

	using Ecng.Collections;
	using Ecng.Common;
	using Ecng.Interop;
	using Ecng.Xaml;

	using Microsoft.Win32;

	using StockSharp.Algo.Candles;
	using StockSharp.BusinessEntities;

	using Syncfusion.Windows.Tools.Controls;

	public partial class CandlesWindow
	{
		public CandlesWindow()
		{
			InitializeComponent();
			TimeFrame.DateTime = DateTime.Today + TimeSpan.FromMinutes(5);
			CandleType.SelectedIndex = 0;
		}

		public Security Security { get; set; }

		public IEnumerable<Trade> Trades { get; set; }

		private void ExportType_Click(object sender, RoutedEventArgs e)
		{
			var selectedItem = (DropDownMenuItem)sender;

			var candles = (IEnumerable<Candle>)BuildedCandles.ItemsSource;

			var dlg = new SaveFileDialog
			{
				FileName = "candles_{0}_{1}".Put(Security.Id, GetCandleTypeText()),
				RestoreDirectory = true,
			};

			if (selectedItem == Excel)
			{
				dlg.Filter = @"xlsx files (*.xlsx)|*.xlsx|All files (*.*)|*.*";
				dlg.FileName += ".xlsx";
			}
			else if (selectedItem == Xml)
			{
				dlg.Filter = @"xml files (*.xml)|*.xml|All files (*.*)|*.*";
				dlg.FileName += ".xml";
			}
			else if (selectedItem == Txt)
			{
				dlg.Filter = @"text files (*.txt)|*.txt|All files (*.*)|*.*";
				dlg.FileName += ".txt";
			}

			if (dlg.ShowDialog(this) != true)
				return;

			if (selectedItem == Excel)
			{
				var exporter = new ExcelExporter();

				exporter
					.SetCell(0, 0, "Время")
					.SetCell(1, 0, "O")
					.SetCell(2, 0, "H")
					.SetCell(3, 0, "L")
					.SetCell(4, 0, "C")
					.SetCell(4, 0, "V");

				var index = 1;

				foreach (var candle in candles)
				{
					exporter
						.SetCell(0, index, candle.Time)
						.SetCell(1, index, candle.OpenPrice)
						.SetCell(2, index, candle.HighPrice)
						.SetCell(3, index, candle.LowPrice)
						.SetCell(4, index, candle.ClosePrice)
						.SetCell(5, index, candle.TotalVolume);

					index++;
				}

				exporter.Save(dlg.FileName);
			}
			else if (selectedItem == Xml)
			{
				new XElement("candles",
					candles.Select(c => new XElement("candle", new[]
				    {
				        new XAttribute("time", c.Time),
				        new XAttribute("open", c.OpenPrice),
				        new XAttribute("high", c.HighPrice),
				        new XAttribute("low", c.LowPrice),
				        new XAttribute("close", c.ClosePrice),
						new XAttribute("volume", c.TotalVolume)
				    }))).Save(dlg.FileName);
			}
			else if (selectedItem == Txt)
			{
				using (var writer = new StreamWriter(dlg.FileName))
				{
					var st = new StringTemplate(File.ReadAllText("txt_export_candles.st"));
					st.RegisterRenderer(typeof(DateTime), new AdvancedDateTimeRenderer());
					st.SetAttribute("candles", candles);
					st.Write(new AutoIndentWriter(writer));
				}
			}
			else
			{
				MessageBox.Show(this, "Неизвестный тип экспорта.", "Гидра", MessageBoxButton.OK, MessageBoxImage.Error);
			}
		}

		private void CandleType_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			TimeFrameSettings.Visibility = VolumeSettings.Visibility = TickSettings.Visibility = RangeSettings.Visibility = Visibility.Hidden;

			switch (CandleType.SelectedIndex)
			{
				case 0:
					TimeFrameSettings.SetVisibility(true);
					break;
				case 1:
					TickSettings.SetVisibility(true);
					break;
				case 2:
					VolumeSettings.SetVisibility(true);
					break;
				case 3:
					RangeSettings.SetVisibility(true);
					break;
			}
		}

		private void Build_Click(object sender, RoutedEventArgs e)
		{
			var candleManager = new CandleManager(Trades) { IsSyncRegister = true };

			CandleToken token = null;

			switch (CandleType.SelectedIndex)
			{
				case 0:
					token = candleManager.RegisterTimeFrameCandles(Security, ((DateTime)TimeFrame.DateTime).TimeOfDay);
					break;
				case 1:
					token = candleManager.RegisterTickCandles(Security, TickCount.Text.To<int>());
					break;
				case 2:
					token = candleManager.RegisterVolumeCandles(Security, Volume.Text.To<int>());
					break;
				case 3:
					token = candleManager.RegisterRangeCandles(Security, Range.Text.ToUnit(Security));
					break;
			}

			if (token == null)
				return;

			BuildedCandles.ItemsSource = candleManager.GetCandles(token);

			ExportBtn.IsEnabled = ShowChart.IsEnabled = true;
		}

		private void ShowChart_Click(object sender, RoutedEventArgs e)
		{
			var chartWnd = new ChartWindow
			{
				Title = "{0} {1}".Put(Security.Id, GetCandleTypeText())
			};
			chartWnd.Chart.Candles.AddRange((IEnumerable<Candle>)BuildedCandles.ItemsSource);
			chartWnd.Show();
		}

		private string GetCandleTypeText()
		{
			return (string)((ComboBoxItem)CandleType.SelectedItem).Content;
		}
	}
}