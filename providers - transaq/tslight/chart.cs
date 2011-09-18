using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;
using System.IO;
using System.Windows.Forms.DataVisualization.Charting;


namespace tslight
{
	//================================================================================
	#region Summary description for FinancialChart
	public class FinancialChart : System.Windows.Forms.UserControl
	{
		public System.Windows.Forms.DataVisualization.Charting.Chart chart1;
		private System.Windows.Forms.Control panel;
		public int period; // The number of candles for chart
		public int bar_count; // The number of candles for source data
		public string ImageDir;
		
		/// Required designer variable.
		private System.ComponentModel.Container components = null;

		//================================================================================
		public FinancialChart(Control panel_control, int chart_period, string AppDir)
		{
			this.Parent = panel_control;
			panel = panel_control;
			period = chart_period;
			ImageDir = AppDir + "image/";
			
			InitializeComponent();
		}

		//================================================================================
		/// Clean up any resources being used.
		protected override void Dispose( bool disposing )
		{
			if( disposing && components != null) components.Dispose();
			base.Dispose( disposing );
		}

		//================================================================================
		private void InitializeComponent()
		{
			System.Windows.Forms.DataVisualization.Charting.ChartArea chartArea1 = new System.Windows.Forms.DataVisualization.Charting.ChartArea();
			System.Windows.Forms.DataVisualization.Charting.Legend legend1 = new System.Windows.Forms.DataVisualization.Charting.Legend();
			System.Windows.Forms.DataVisualization.Charting.Series series1 = new System.Windows.Forms.DataVisualization.Charting.Series();
			System.Windows.Forms.DataVisualization.Charting.Series seriesBuy = new System.Windows.Forms.DataVisualization.Charting.Series();
			//System.Windows.Forms.DataVisualization.Charting.Series seriesShort = new System.Windows.Forms.DataVisualization.Charting.Series();
			//System.Windows.Forms.DataVisualization.Charting.Series seriesCover = new System.Windows.Forms.DataVisualization.Charting.Series();
			this.chart1 = new System.Windows.Forms.DataVisualization.Charting.Chart();
			((System.ComponentModel.ISupportInitialize)(this.chart1)).BeginInit();
			this.SuspendLayout();
			// 
			// chart1
			// 
			this.chart1.BorderlineColor = System.Drawing.Color.FromArgb(((System.Byte)(26)), ((System.Byte)(59)), ((System.Byte)(105)));
			this.chart1.BorderlineDashStyle = System.Windows.Forms.DataVisualization.Charting.ChartDashStyle.Solid;
			this.chart1.BorderlineWidth = 2;
			chartArea1.Area3DStyle.IsClustered = true;
			chartArea1.Area3DStyle.Perspective = 10;
			chartArea1.Area3DStyle.IsRightAngleAxes = false;
			chartArea1.Area3DStyle.WallWidth = 0;
			chartArea1.Area3DStyle.Inclination = 15;
			chartArea1.Area3DStyle.Rotation = 10;
			chartArea1.AxisX.IsLabelAutoFit = false;
			chartArea1.AxisX.LabelStyle.Font = new System.Drawing.Font("Trebuchet MS", 8.25F, System.Drawing.FontStyle.Bold);
			chartArea1.AxisX.LabelStyle.IsEndLabelVisible = false;
			chartArea1.AxisX.LineColor = System.Drawing.Color.FromArgb(((System.Byte)(64)), ((System.Byte)(64)), ((System.Byte)(64)), ((System.Byte)(64)));
			chartArea1.AxisX.MajorGrid.LineColor = System.Drawing.Color.FromArgb(((System.Byte)(64)), ((System.Byte)(64)), ((System.Byte)(64)), ((System.Byte)(64)));
			chartArea1.AxisY.IsLabelAutoFit = false;
			chartArea1.AxisY.LabelStyle.Font = new System.Drawing.Font("Trebuchet MS", 8.25F, System.Drawing.FontStyle.Bold);
			chartArea1.AxisY.LineColor = System.Drawing.Color.FromArgb(((System.Byte)(64)), ((System.Byte)(64)), ((System.Byte)(64)), ((System.Byte)(64)));
			chartArea1.AxisY.MajorGrid.LineColor = System.Drawing.Color.FromArgb(((System.Byte)(64)), ((System.Byte)(64)), ((System.Byte)(64)), ((System.Byte)(64)));
			chartArea1.AxisY.IsStartedFromZero = false;
			//chartArea1.BackColor = System.Drawing.Color.FromArgb(((System.Byte)(64)), ((System.Byte)(165)), ((System.Byte)(191)), ((System.Byte)(228)));
			chartArea1.BackColor = Color.White;
			chartArea1.BorderColor = System.Drawing.Color.FromArgb(((System.Byte)(64)), ((System.Byte)(64)), ((System.Byte)(64)), ((System.Byte)(64)));
			chartArea1.BorderDashStyle = System.Windows.Forms.DataVisualization.Charting.ChartDashStyle.Solid;
			chartArea1.Name = "Price";
			chartArea1.Position.Auto = false;
			chartArea1.Position.Height = 90F;
			chartArea1.Position.Width = 90F;
			chartArea1.Position.X = 5F;
			chartArea1.Position.Y = 5F;
			chartArea1.ShadowColor = System.Drawing.Color.Transparent;
			this.chart1.ChartAreas.Add(chartArea1);

			legend1.Alignment = System.Drawing.StringAlignment.Far;
			legend1.IsTextAutoFit = false;
			legend1.BackColor = System.Drawing.Color.Transparent;
			legend1.Docking = System.Windows.Forms.DataVisualization.Charting.Docking.Top;
			legend1.IsDockedInsideChartArea = false;
			legend1.DockedToChartArea = "Price";
			legend1.Font = new System.Drawing.Font("Trebuchet MS", 8.25F, System.Drawing.FontStyle.Bold);
			legend1.LegendStyle = System.Windows.Forms.DataVisualization.Charting.LegendStyle.Row;
			legend1.Name = "Default";
			legend1.Position.Auto = false;
			legend1.Position.Height = 7F;
			legend1.Position.Width = 40F;
			legend1.Position.X = 60F;
			legend1.Position.Y = 1F;
			this.chart1.Legends.Add(legend1);

			this.chart1.Name = "chart1";
			this.chart1.Palette = System.Windows.Forms.DataVisualization.Charting.ChartColorPalette.BrightPastel;
			series1.BorderColor = System.Drawing.Color.FromArgb(((System.Byte)(180)), ((System.Byte)(26)), ((System.Byte)(59)), ((System.Byte)(105)));
			series1.ChartArea = "Price";
			series1.ChartType = SeriesChartType.Candlestick;
			series1.Name = "Price";
			series1.IsVisibleInLegend = false;
//			series1.XValueType = System.Windows.Forms.DataVisualization.Charting.ChartValueType.Time;
			series1.YValuesPerPoint = 4;
			this.chart1.Series.Add(series1);

			seriesBuy.ChartArea = "Price";
			seriesBuy.ChartType = SeriesChartType.Line;
			seriesBuy.Color = Color.White;
			seriesBuy.Name = "Buy";
			seriesBuy.IsVisibleInLegend = false;
			this.chart1.Series.Add(seriesBuy);
			/*
			seriesShort.ChartArea = "Price";
			seriesShort.ChartType = SeriesChartType.Line;
			seriesShort.Color = Color.White;
			seriesShort.Name = "Short";
			seriesShort.IsVisibleInLegend = false;
			this.chart1.Series.Add(seriesShort);
	
			seriesCover.ChartArea = "Price";
			seriesCover.ChartType = SeriesChartType.Line;
			seriesCover.Color = Color.White;
			seriesCover.Name = "Cover";
			seriesCover.IsVisibleInLegend = false;
			this.chart1.Series.Add(seriesCover);
			*/
			this.chart1.Size = new System.Drawing.Size(panel.Width, panel.Height);
			this.chart1.TabIndex = 1;

			
			// Add custom legend items
			LegendItem legendItem = new LegendItem(); 
			legendItem.Name = "Buy";
			legendItem.ImageStyle = LegendImageStyle.Marker;
			legendItem.MarkerImageTransparentColor = Color.White;
			legendItem.MarkerImage = ImageDir + "up.bmp";
			chart1.Legends[0].CustomItems.Add(legendItem);

			legendItem = new LegendItem(); 
			legendItem.Name = "Sell";
			legendItem.ImageStyle = LegendImageStyle.Marker;
			legendItem.MarkerImageTransparentColor = Color.White;
			legendItem.MarkerImage = ImageDir + "down.bmp";
            chart1.Legends[0].CustomItems.Add(legendItem);
			/*
			legendItem = new LegendItem(); 
			legendItem.Name = "Open short";
			legendItem.ImageStyle = LegendImageStyle.Marker;
			legendItem.MarkerImageTransparentColor = Color.White;
			legendItem.MarkerImage = ImageDir + "down_magenta.bmp";
            chart1.Legends[0].CustomItems.Add(legendItem);

			legendItem = new LegendItem(); 
			legendItem.Name = "Close short";
			legendItem.ImageStyle = LegendImageStyle.Marker;
			legendItem.MarkerImageTransparentColor = Color.White;
			legendItem.MarkerImage = ImageDir + "up_green.bmp";
            chart1.Legends[0].CustomItems.Add(legendItem);
            */
            // 
			// FinancialChart
			// 
			this.BackColor = System.Drawing.Color.White;
			this.Controls.AddRange(new System.Windows.Forms.Control[] {this.chart1});
			this.Font = new System.Drawing.Font("Verdana", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
			this.Name = "FinancialChart";
			this.Width = panel.Width;
			this.Height = panel.Height;
			((System.ComponentModel.ISupportInitialize)(this.chart1)).EndInit();
			this.ResumeLayout(false);

		}
		//================================================================================
		public void LoadData(DataTable_candle candles)
		{
			DateTime dt = DateTime.Now;
			DateTime prev_dt = dt;
			DateTime delta_dt = dt;
			
			chart1.Series["Price"].Points.Clear();
			chart1.Series["Buy"].Points.Clear();
			//chart1.Series["Short"].Points.Clear();
			//chart1.Series["Cover"].Points.Clear();
			int count = period;
			bar_count = candles.Count;
			if (count > candles.Count) count = candles.Count;
			for (int i=0; i<count; i++)
			{
				int bar = candles.Count - (count) + i;
				DataRow_candle candle = candles[bar];

				dt = candle.date;
				if (i == 0)
				{
					//chart1.Series["Price"].Points.AddXY(dt, candle.high);
					chart1.Series["Price"].Points.AddXY(1, candle.high);
					
					chart1.Series["Buy"].Points.AddXY(1, candle.low * 0.9999);
					//chart1.Series["Short"].Points.AddXY(1, candle.high * 1.0005);
					//chart1.Series["Cover"].Points.AddXY(1, candle.low * 0.9999);
				}
				else
				{
					//int mn = dt.Hour * 60 + dt.Minute;
					//int prev_mn = prev_dt.Hour * 60 + prev_dt.Minute;
					//int delta_mn = mn - prev_mn;
					//if (mn < prev_mn) delta_mn = delta_mn + 1440;
					//double xval = chart1.Series["Price"].Points[i-1].XValue + delta_mn / 1440.0;
					double xval = chart1.Series["Price"].Points[i-1].XValue + 1;
					chart1.Series["Price"].Points.AddXY(i, candle.high);
					chart1.Series["Price"].Points[i].XValue = xval;

					chart1.Series["Buy"].Points.AddXY(i, candle.low * 0.9999);
					chart1.Series["Buy"].Points[i].XValue = xval;
					//chart1.Series["Short"].Points.AddXY(i, candle.high * 1.0005);
					//chart1.Series["Short"].Points[i].XValue = xval;
					//chart1.Series["Cover"].Points.AddXY(i, candle.low * 0.9999);
					//chart1.Series["Cover"].Points[i].XValue = xval;
				}
				chart1.Series["Price"].Points[i].YValues[1] = candle.low;
				chart1.Series["Price"].Points[i].YValues[3] = candle.open;
				chart1.Series["Price"].Points[i].YValues[2] = candle.close;
				prev_dt = dt;
			}
		}
		//================================================================================
		public void LoadPoint(int index, DataRow_candle candle)
		{
			if (index <= 0) return;
			if (index >= chart1.Series["Price"].Points.Count) return;
				
			double xval = chart1.Series["Price"].Points[index-1].XValue + 1;
			chart1.Series["Price"].Points[index].XValue = xval;
			chart1.Series["Price"].Points[index].YValues[0] = candle.high;
			chart1.Series["Price"].Points[index].YValues[1] = candle.low;
			chart1.Series["Price"].Points[index].YValues[3] = candle.open;
			chart1.Series["Price"].Points[index].YValues[2] = candle.close;
			
			chart1.Series["Buy"].Points[index].YValues[0] = candle.low * 0.9999;
			//chart1.Series["Short"].Points[index].YValues[0] = candle.high * 1.0005;
			//chart1.Series["Cover"].Points[index].YValues[0] = candle.low * 0.9999;
		}
		//================================================================================
		public void SetMarkers(IList<int> Bars, string image, string info, string series)
		{
			string name = series;
			if (name.Length == 0) name = "Price";
			
			for (int i=0; i<Bars.Count; i++)
			{
				int index = Bars[i] - (bar_count - period);
				if (index >= 0 && index < chart1.Series[name].Points.Count)
				{
					chart1.Series[name].Points[index].MarkerImage = image;
					chart1.Series[name].Points[index].MarkerImageTransparentColor = Color.White;
					if (info.Length > 0)
					{
						chart1.Series[name].Points[index].ToolTip = info;
						chart1.Series[name].Points[index].Label = info;
					}
				}
			}
		}
		//================================================================================
	}
	#endregion
	//================================================================================
}
