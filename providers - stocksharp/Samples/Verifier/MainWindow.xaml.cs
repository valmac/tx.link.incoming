namespace StockSharp.Quik.Verifier
{
	using System;
	using System.Linq;
	using System.Text;
	using System.Windows;
	using System.Windows.Controls;
	using System.Windows.Input;

	using Ecng.Common;
	using Ecng.Xaml;
	using Ecng.Collections;

	using StockSharp.Quik;

	public partial class MainWindow
	{
		private sealed class SettingsError
		{
			public SettingsError(string message, bool isCritical)
			{
				this.Message = message;
				this.IsCritical = isCritical;
			}

			public string Message { get; private set; }
			public bool IsCritical { get; private set; }
		}

		private readonly ThreadSafeObservableCollection<SettingsError> _settingErrors = new ThreadSafeObservableCollection<SettingsError>();

		public MainWindow()
		{
			InitializeComponent();

			this.Results.ItemsSource = _settingErrors;

			FindTerminals();
		}

		private void FindTerminals()
		{
			var terminals = QuikTerminal.Terminals;
			this.QuikTerminals.ItemsSource = terminals;

			this.SelectedTerminal = terminals.FirstOrDefault();
		}

		public QuikTerminal SelectedTerminal
		{
			get { return (QuikTerminal)this.QuikTerminals.SelectedItem; }
			set { this.QuikTerminals.SelectedItem = value; }
		}

		private void RefreshTerminals_Click(object sender, RoutedEventArgs e)
		{
			FindTerminals();
		}

		private void QuikTerminals_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			var terminal = this.SelectedTerminal;

			if (terminal != null)
			{
				this.QuikTitle.Text = terminal.SystemProcess.MainWindowTitle;
				this.Check.IsEnabled = true;
			}
			else
			{
				this.QuikTitle.Text = string.Empty;
				this.Check.IsEnabled = true;
			}
		}

		private void Check_Click(object sender, RoutedEventArgs e)
		{
			var terminal = this.SelectedTerminal;

			if (terminal.SystemProcess.MainModule == null)
				throw new InvalidOperationException("Неподходящий процесс для обработки.");

			this.OkResult.SetVisibility(false);

			_settingErrors.Clear();

			var trader = new QuikTrader(terminal.SystemProcess.MainModule.FileName);

			trader.Connected += () => this.GuiAsync(() =>
			{
				if (this.CheckDde.IsChecked == true)
					trader.StartExport();

				OnConnect(trader, null);
			});

			trader.ConnectionError += error => this.GuiSync(() => OnConnect(trader, error));

			if (this.CheckDde.IsChecked == true)
				trader.ProcessDataError += error => _settingErrors.Add(new SettingsError("Экспорт DDE. {0}".Put(error.Message), true));	

			trader.Connect();
		}

		private void OnConnect(QuikTrader trader, Exception connectionError)
		{
			if (connectionError == null)
			{
				_settingErrors.AddRange(trader.Terminal.GetTableSettings()
					.Select(r => new SettingsError("Таблица {0}. {1}".Put(r.Table.Caption, r.Error.Message), r.IsCritical)));

				if (_settingErrors.Count == 0)
					this.OkResult.SetVisibility(true);
			}
			else
				MessageBox.Show(this, connectionError.ToString(), "Verifier");

			trader.Dispose();
		}

		private void CopyCanExecute(object sender, CanExecuteRoutedEventArgs e)
		{
			e.CanExecute = true;
			e.Handled = true;
		}

		private void CopyExecuted(object sender, ExecutedRoutedEventArgs e)
		{
			var text = new StringBuilder();

			foreach (SettingsError error in this.Results.SelectedItems)
			{
				text.AppendFormat("{0}.	{1}", error.IsCritical ? "Ошибка" : "Предупреждение", error.Message);
				text.AppendLine();
			}

            XamlHelper.TryCopyToClipboard(text.To<string>());
			e.Handled = true;
		}
	}
}