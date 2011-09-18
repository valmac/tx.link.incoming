namespace StockSharp.Hydra
{
	using System;
	using System.Collections.Generic;
	using System.ComponentModel;
	using System.Data.Common;
	using System.Diagnostics;
	using System.IO;
	using System.Linq;
	using System.Reflection;
	using System.Threading;
	using System.Windows;
	using System.Windows.Controls;

	using Ecng.Collections;
	using Ecng.Common;
	using Ecng.Configuration;
	using Ecng.Xaml;

	using StockSharp.BusinessEntities;
	using StockSharp.Hydra.Core;

	public partial class MainWindow
	{
		private readonly object _loggerLock = new object();
		private DateTime _loggerCreationDate;
		private StreamWriter _logger;
		private string _loggerFileName;
		private const string _pluginsDir = "Plugins";

		private readonly Worker _worker;
		private readonly HydraStorage _storage;
		private readonly IList<IMarketDataSource> _sources = new List<IMarketDataSource>();
		private bool _isStarted;
		private readonly List<VisualSecurity> _selectedSecurities = new List<VisualSecurity>();
		private readonly Timer _updateUITimer;

		public MainWindow()
		{
			InitializeComponent();

			CreateLogger();

			_updateUITimer = TimeSpan.FromMilliseconds(300).CreateTimer(OnUpdateUI);

			_storage = ConfigManager.ServiceLocator.GetInstance<HydraStorage>();

			try
			{
				if (_storage.Exchanges.Count < 4)
				{
					_storage.Exchanges.Save(Exchange.Test);
					_storage.Exchanges.Save(Exchange.Micex);
					_storage.Exchanges.Save(Exchange.Rts);
					_storage.Exchanges.Save(Exchange.Ux);
				}

				if (Directory.Exists(_pluginsDir))
				{
					foreach (var plugin in Directory.GetFiles(_pluginsDir, "*.dll"))
					{
						try
						{
							var asm = Assembly.Load(AssemblyName.GetAssemblyName(plugin));

							_sources.AddRange(asm
								.GetTypes()
								.Where(t => typeof(IMarketDataSource).IsAssignableFrom(t) && !t.IsAbstract)
								.Select(t => t.CreateInstance<IMarketDataSource>(_storage))
								.ToArray());
						}
						catch (Exception ex)
						{
							OnError(null, ex);
						}
					}
				}
			}
			catch (DbException ex)
			{
				LogError(ex);
				MessageBox.Show(this, ex.ToString(), "Гидра - ошибка базы данных.", MessageBoxButton.OK, MessageBoxImage.Error);
				Close();
				return;
			}

			foreach (var s in _sources)
			{
				var source = s;
				source.Log += msg => OnLog(source, msg);
			}
			
			_worker = new Worker(_storage, _sources, 10);
			_worker.Error += OnError;
			_worker.Log += OnLog;
			_worker.Stopped += OnStopped;

			FillSecurities();
		}

		private void CreateLogger()
		{
			_loggerCreationDate = DateTime.Today;
			_loggerFileName = "log_{0}.txt".Put(_loggerCreationDate.ToString("yyyy_MM_dd"));
			_logger = new StreamWriter(_loggerFileName, true) { AutoFlush = true };
		}

		private VisualSecurity SelectedSecurity
		{
			get { return SecuritiesCtrl.SelectedSecurities.FirstOrDefault(); }
		}

		public void LogError(Exception error)
		{
			OnLog(null, error.ToString());
		}

		private void OnError(IMarketDataSource source, Exception error)
		{
			this.GuiSync(() => LogErrorImg.Visibility = Visibility.Visible);
			OnLog(source, error.ToString());
		}

		private void OnLog(IMarketDataSource source, string message)
		{
			lock (_loggerLock)
			{
				if (_loggerCreationDate != DateTime.Today)
				{
					_logger.Dispose();
					CreateLogger();
				}

				_logger.Write(source != null ? source.Name : "Гидра");
				_logger.Write(" ");
				_logger.Write(DateTime.Now.TimeOfDay);
				_logger.Write(" ");
				_logger.WriteLine(message);
			}
		}

		private void OnUpdateUI()
		{
			int totalTrades = 0;
			int totalDepths = 0;

			foreach (var ms in _sources)
			{
				if (ms is ITradeSource)
					totalTrades += (ms as ITradeSource).TotalTrades;
				if (ms is IMarketDepthSource)
					totalDepths += (ms as IMarketDepthSource).TotalDepths;
			}

			this.GuiAsync(() => Status.Text = "T {0} D {1}".Put(totalTrades, totalDepths));
		}

		protected override void OnClosing(CancelEventArgs e)
		{
			if (_isStarted)
			{
				MessageBox.Show(this, "Гидра в процессе работы. Необходимо остановить скачивание данных или дождаться окончания скачивания.", "Гидра", MessageBoxButton.OK, MessageBoxImage.Error);
				e.Cancel = true;
			}

			base.OnClosing(e);
		}

		private void OnStopped()
		{
			_updateUITimer.Dispose();

			_isStarted = false;

			this.GuiSync(() =>
			{
				StartStop.Content = "Старт";
				StartStop.IsEnabled = true;
				Settings.IsEnabled = true;
				Securities.IsEnabled = true;
			});
		}

		private void StartStop_Click(object sender, RoutedEventArgs e)
		{
			if (_isStarted)
			{
				StartStop.IsEnabled = false;
				_worker.Stop();
			}
			else
			{
				if (_sources.Count(l => l.Settings.IsEnabled) == 0)
				{
					MessageBox.Show(this, "Необходимо включить хотя бы один источник данных.", "Гидра", MessageBoxButton.OK, MessageBoxImage.Warning);
					return;
				}

				_worker.Start(_selectedSecurities);
				StartStop.Content = "Стоп";
				Settings.IsEnabled = false;
				Securities.IsEnabled = false;

				_isStarted = true;
			}

            EnableDisableButtons();
		}

		private void SecuritiesCtrl_SelectionChanged(object sender, EventArgs e)
		{
            EnableDisableButtons();
		}

        private void EnableDisableButtons()
        {
            Depths.IsEnabled = Trades.IsEnabled = (SelectedSecurity != null && !_isStarted);
        }

		private void FillSecurities()
		{
			_selectedSecurities.Clear();
			_selectedSecurities.AddRange(_storage.Securities.Select(s => s.ToVisualSecurity()).Where(s => s.IsSelected));

			FilterSecurities();
		}

		private void Settings_Click(object sender, RoutedEventArgs e)
		{
			var wnd = new SettingsWindow { Sources = _sources };

			if (wnd.ShowModal(this))
			{
				_sources.ForEach(l => l.SaveSettings());
			}
		}

		private void Securities_Click(object sender, RoutedEventArgs e)
		{
			var securitiesWnd = new SecuritiesWindow(_storage, _sources);
			securitiesWnd.ShowModal(this);

			FillSecurities();
		}

		private void Logs_Click(object sender, RoutedEventArgs e)
		{
			Process.Start(_loggerFileName);
			LogErrorImg.Visibility = Visibility.Collapsed;
		}

		private void Depths_Click(object sender, RoutedEventArgs e)
		{
			var wnd = new DepthWindow
			{
				Security = SelectedSecurity.Security,
				Storage = _storage,
			};
			wnd.Title += SelectedSecurity.Security.Id;
			wnd.ShowModal(this);
		}

		private void Trades_Click(object sender, RoutedEventArgs e)
		{
			var wnd = new TradesWindow
			{
				Security = SelectedSecurity.Security,
				Storage = _storage,
			};
			wnd.Title += SelectedSecurity.Security.Id;
			wnd.ShowModal(this);
		}

		private void NameLike_TextChanged(object sender, TextChangedEventArgs e)
		{
			FilterSecurities();
		}

		private void FilterSecurities()
		{
			var secName = NameLike.Text;

			var selectedSecurities = _selectedSecurities
				.Where(s =>
					(s.Security.Id != null && s.Security.Name != null && s.Security.Code != null) &&
					(secName.IsEmpty() || s.Security.Id.ContainsIgnoreCase(secName) ||
					s.Security.Code.ContainsIgnoreCase(secName) || s.Security.Name.ContainsIgnoreCase(secName)))
				.ToArray();

			SecuritiesCtrl.Securities.Clear();
			SecuritiesCtrl.Securities.AddRange(selectedSecurities);
		}
	}
}
