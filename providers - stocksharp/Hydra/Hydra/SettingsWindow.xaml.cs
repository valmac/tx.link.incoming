namespace StockSharp.Hydra
{
	using System;
	using System.Collections.Generic;
	using System.Globalization;
	using System.Linq;
	using System.Windows;
	using System.Windows.Data;

	using Microsoft.Windows.Controls;
	using Microsoft.Windows.Controls.PropertyGrid.Editors;

	using StockSharp.Hydra.Core;

	public partial class SettingsWindow
	{
		private sealed class TimeSpanEditor : DateTimeUpDownEditor
		{
			private sealed class TimeSpanConverter : IValueConverter
			{
				object IValueConverter.Convert(object value, Type targetType, object parameter, CultureInfo culture)
				{
					return DateTime.Today + (TimeSpan)value;
				}

				object IValueConverter.ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
				{
					return ((DateTime)value).TimeOfDay;
				}
			}

			public TimeSpanEditor()
			{
				Editor.Format = DateTimeFormat.LongTime;
			}

			protected override IValueConverter CreateValueConverter()
			{
				return new TimeSpanConverter();
			}
		}

		private static bool _initialized;

		public SettingsWindow()
		{
			InitializeComponent();
		}

		public IEnumerable<IMarketDataSource> Sources
		{
			get { return (IEnumerable<IMarketDataSource>)SourcesCtrl.ItemsSource; }
			set
			{
				if (!_initialized)
				{
					var names = value
						.SelectMany(v => v.Settings.GetType().GetProperties().Where(p => p.PropertyType == typeof(TimeSpan)))
						.Select(p => p.Name)
						.Distinct();

					foreach (var name in names)
					{
						var timeSpanEditor = new CustomTypeEditor { Editor = new TimeSpanEditor() };
						timeSpanEditor.Properties.Add(name);
						SourceSettings.CustomTypeEditors.Add(timeSpanEditor);
					}

					_initialized = true;
				}

				SourcesCtrl.ItemsSource = value;
				SourcesCtrl.SelectedItem = value.FirstOrDefault();
			}
		}

		private void OK_Click(object sender, RoutedEventArgs e)
		{
			DialogResult = true;
		}
	}
}