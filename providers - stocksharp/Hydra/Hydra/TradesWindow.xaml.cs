namespace StockSharp.Hydra
{
	using System;
	using System.Collections.Generic;
	using System.IO;
	using System.Linq;
	using System.Windows;
	using System.Xml.Linq;

	using Antlr3.ST;

	using Ecng.Common;
	using Ecng.Interop;
	using Ecng.Xaml;

	using StockSharp.BusinessEntities;
	using StockSharp.Hydra.Core;

	using Microsoft.Win32;

	using Syncfusion.Windows.Tools.Controls;

	public partial class TradesWindow
	{
		public TradesWindow()
		{
			InitializeComponent();
			From.DateTime = DateTime.Today - TimeSpan.FromDays(7);
			To.DateTime = DateTime.Today + TimeSpan.FromDays(1);
		}

		public Security Security { get; set; }

		public HydraStorage Storage { get; set; }

		private DateTime FromDate
		{
			get
			{
				if (From.DateTime == null)
					throw new InvalidOperationException("Значение 'от' не инициализировано.");

				return (DateTime)From.DateTime;
			}
		}

		private DateTime ToDate
		{
			get
			{
				if (To.DateTime == null)
					throw new InvalidOperationException("Значение 'до' не инициализировано.");

				return (DateTime)To.DateTime;
			}
		}

		private void Find_Click(object sender, RoutedEventArgs e)
		{
			if (Security != null && From.DateTime != null && To.DateTime != null)
			{
				FindedTrades.ItemsSource = Storage.GetTradeStorage(Security)
					.Load(FromDate, ToDate + TimeSpan.FromTicks(TimeSpan.TicksPerDay - 1));

				ExportBtn.IsEnabled = Candles.IsEnabled = true;
			}
		}

		private void ExportType_Click(object sender, RoutedEventArgs e)
		{
			var selectedItem = (DropDownMenuItem)sender;

			var trades = (IEnumerable<Trade>)FindedTrades.ItemsSource;

			var dlg = new SaveFileDialog
			{
				FileName = "trades_{0}_{1}_{2}".Put(Security.Id, FromDate.ToString("yyyy_MM_dd"), ToDate.ToString("yyyy_MM_dd")),
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
					.SetCell(0, 0, "Номер")
					.SetCell(1, 0, "Время")
					.SetCell(2, 0, "Цена")
					.SetCell(3, 0, "Объем")
					.SetCell(4, 0, "Направление");

				var index = 1;

				foreach (var trade in trades)
				{
					exporter
						.SetCell(0, index, trade.Id)
						.SetCell(1, index, trade.Time)
						.SetCell(2, index, trade.Price)
						.SetCell(3, index, trade.Volume)
						.SetCell(4, index, trade.OrderDirection != null ? trade.OrderDirection.ToString() : string.Empty);

					index++;
				}

				exporter.Save(dlg.FileName);
			}
			else if (selectedItem == Xml)
			{
				new XElement("trades",
					trades.Select(t => new XElement("trade", new[]
				    {
				        new XAttribute("id", t.Id),
				        new XAttribute("time", t.Time),
				        new XAttribute("price", t.Price),
				        new XAttribute("volume", t.Volume),
				        new XAttribute("orderDirection", t.OrderDirection != null ? t.OrderDirection.ToString() : string.Empty)
				    }))).Save(dlg.FileName);
			}
			else if (selectedItem == Txt)
			{
				using (var writer = new StreamWriter(dlg.FileName))
				{
					var st = new StringTemplate(File.ReadAllText("txt_export_trades.st"));
					st.RegisterRenderer(typeof(DateTime), new AdvancedDateTimeRenderer());
					st.SetAttribute("security", Security.Id);
					st.SetAttribute("from", FromDate);
					st.SetAttribute("to", ToDate);
					st.SetAttribute("trades", trades);
					st.Write(new AutoIndentWriter(writer));
				}
			}
			else
			{
				MessageBox.Show(this, "Неизвестный тип экспорта.", "Гидра", MessageBoxButton.OK, MessageBoxImage.Error);
			}
		}

		private void Candles_Click(object sender, RoutedEventArgs e)
		{
			var wnd = new CandlesWindow
			{
				Security = Security,
				Trades = (IEnumerable<Trade>)FindedTrades.ItemsSource,
			};
			wnd.Title += Security.Id;
			wnd.ShowModal(this);
		}
	}
}