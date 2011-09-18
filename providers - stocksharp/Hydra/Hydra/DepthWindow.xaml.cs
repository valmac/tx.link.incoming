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

	using Microsoft.Win32;

	using StockSharp.BusinessEntities;
	using StockSharp.Hydra.Core;

	using Syncfusion.Windows.Tools.Controls;

	public partial class DepthWindow
	{
		private readonly List<MarketDepth> _loadedDepths = new List<MarketDepth>();

		public DepthWindow()
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
			if (Security != null)
			{
				_loadedDepths.Clear();
				_loadedDepths.AddRange(
					Storage
						.GetMarketDepthStorage(Security)
						.Load(FromDate, ToDate + TimeSpan.FromTicks(TimeSpan.TicksPerDay - 1)));

				QuotesSlider.Maximum = _loadedDepths.Count - 1;
				QuotesSlider.Minimum = 0;
				QuotesSlider.SmallChange = 1;
				QuotesSlider.LargeChange = 5;

				QuotesSlider.SelectionEnd = _loadedDepths.Count;

				ExportBtn.IsEnabled = true;

				DisplayDepth();
			}
		}

		private void QuotesSlider_ValueChanged(object sender, RoutedPropertyChangedEventArgs<double> e)
		{
			DisplayDepth();
		}

		private void DisplayDepth()
		{
			if (_loadedDepths.Count == 0)
				return;

			var depth = _loadedDepths[(int)this.QuotesSlider.Value];
			Quotes.ItemsSource = depth.Select(q => new VisualQuote(q));
			DateEdit.Text = depth.LastChangeTime.ToString("dd-MM-yyyy HH:mm:ss.fff");
		}

		private void ExportType_Click(object sender, RoutedEventArgs e)
		{
			var selectedItem = (DropDownMenuItem)sender;

			var dlg = new SaveFileDialog
			{
				FileName = "depths_{0}_{1}_{2}".Put(Security.Id, FromDate.ToString("yyyy_MM_dd"), ToDate.ToString("yyyy_MM_dd")),
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

				var columnIndex = 0;

				foreach (var depth in _loadedDepths)
				{
					exporter.SetCell(columnIndex, 0, depth.LastChangeTime);

					var rowIndex = 1;

					foreach (var quote in depth)
					{
						exporter
							.SetCell(columnIndex++, rowIndex, quote.Price)
							.SetCell(columnIndex++, rowIndex, quote.Volume)
							.SetCell(columnIndex++, rowIndex, quote.OrderDirection == OrderDirections.Buy ? "B" : "A");

						rowIndex++;
					}

					columnIndex++;
				}

				exporter.Save(dlg.FileName);
			}
			else if (selectedItem == Xml)
			{
				new XElement("depths",
					_loadedDepths.Select(depth => new XElement("depth", new[]
					{
						new XElement("lastChangeTime", depth.LastChangeTime),
						new XElement("quotes", depth.Select(q => new[]
						{
							new XElement("price", q.Price),
							new XElement("volume", q.Volume),
							new XElement("dir", q.OrderDirection)
						}))
					}))).Save(dlg.FileName);
			}
			else if (selectedItem == Txt)
			{
				using (var writer = new StreamWriter(dlg.FileName))
				{
					var st = new StringTemplate(File.ReadAllText("txt_export_depths.st"));
					st.RegisterRenderer(typeof(DateTime), new AdvancedDateTimeRenderer());
					st.SetAttribute("security", Security.Id);
					st.SetAttribute("from", FromDate);
					st.SetAttribute("to", ToDate);
					st.SetAttribute("depths", _loadedDepths);
					st.Write(new AutoIndentWriter(writer));
				}
			}
			else
			{
				MessageBox.Show(this, "Неизвестный тип экспорта.", "Гидра", MessageBoxButton.OK, MessageBoxImage.Error);
			}
		}
	}
}