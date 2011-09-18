namespace StockSharp.Hydra
{
	using System;
	using System.Collections.Generic;
	using System.Linq;
	using System.Windows;
	using System.Windows.Controls;

	using Ecng.Collections;
	using Ecng.Common;
	using Ecng.Xaml;

	using StockSharp.Hydra.Core;
	using StockSharp.BusinessEntities;

	public partial class SecuritiesWindow
	{
		private readonly HydraStorage _storage;
		private readonly List<VisualSecurity> _securities = new List<VisualSecurity>();
		private readonly IEnumerable<IMarketDataSource> _sources;
		private bool _isOnlySelected;

		public SecuritiesWindow()
		{
			InitializeComponent();
		}

		internal SecuritiesWindow(HydraStorage storage, IEnumerable<IMarketDataSource> sources)
			: this()
		{
			if (storage == null)
				throw new ArgumentNullException("storage");

			if (sources == null)
				throw new ArgumentNullException("sources");

			_storage = storage;
			_sources = sources;

			AddToVisualSecurities(storage.Securities);
			Sources.ItemsSource = new[] { string.Empty }.Concat(_sources.Select(l => l.Name));
			RefreshSecurities();
		}

		private IList<VisualSecurity> VisibleSecurities
		{
			get { return SecuritiesCtrl.Securities; }
		}

		private VisualSecurity SelectedSecurity
		{
			get { return SecuritiesCtrl.SelectedSecurities.FirstOrDefault(); }
		}

		private void NameLike_TextChanged(object sender, TextChangedEventArgs e)
		{
			RefreshSecurities();
		}

		private void RefreshSecurities()
		{
			var secName = NameLike.Text;

			VisibleSecurities.Clear();

			var selectedSource = (string)Sources.SelectedItem;

			VisibleSecurities.AddRange(_securities.Where(s => (s.Security.Id != null && s.Security.Name != null && s.Security.Code != null) &&
				(secName.IsEmpty() || s.Security.Id.ContainsIgnoreCase(secName) || s.Security.Code.ContainsIgnoreCase(secName) || s.Security.Name.ContainsIgnoreCase(secName)) &&
				(selectedSource == null || (s.TradeSource == selectedSource || s.DepthSource == selectedSource || s.Source == selectedSource))));
		}

		private void OnlySelected_Click(object sender, RoutedEventArgs e)
		{
			_isOnlySelected = !_isOnlySelected;

			OnlySelected.Content = _isOnlySelected ? "Показать все" : "Только выбранные";

			if (_isOnlySelected)
			{
				VisibleSecurities.Clear();
				VisibleSecurities.AddRange(_securities.Where(s => s.IsSelected));
			}
			else
				RefreshSecurities();
		}

		private void Selected_Click(object sender, RoutedEventArgs e)
		{
			UpdateSecurities();
		}

		private void SecuritiesCtrl_SecurityClicked(object sender, EventArgs e)
		{
			UpdateSecurities();
		}

		private void UpdateSecurities()
		{
			var security = SelectedSecurity;

			if (!security.IsSelected)
			{
				var wnd = new SecurityEditWindow(_storage) { Security = security, Sources = _sources };
				if (!wnd.ShowModal(this))
					return;
			}

			security.IsSelected = !security.IsSelected;
			_storage.Securities.Save(security.Security);
		}

		private void Sources_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			RefreshSecurities();
		}

		private void SecuritiesCtrl_SelectionChanged(object sender, EventArgs e)
		{
			Edit.IsEnabled = Selected.IsEnabled = SelectedSecurity != null;
		}

		private void Refresh_Click(object sender, RoutedEventArgs e)
		{
			var sources = _sources
				.Where(s => s.Settings.IsEnabled)
				.OfType<ISecuritySource>();

			if (sources.Count() == 0)
			{
				MessageBox.Show(this, "Необходимо включить хотя бы один источник инструментов.", "Гидра", MessageBoxButton.OK, MessageBoxImage.Warning);
				return;
			}

			if (MessageBox.Show(this, "Обновление справочника инструментов может занять до нескольких минут. Продолжить?",
				"Гидра", MessageBoxButton.YesNo, MessageBoxImage.Warning) == MessageBoxResult.Yes)
			{
				var newSecurities = sources.SelectMany(s => s.GetNewSecurities()).ToArray();

				AddToVisualSecurities(newSecurities);

				var count = newSecurities.Count();

				MessageBox.Show(this, "Добавлено {0} новых инструментов.".Put(count));

				if (count > 0)
					RefreshSecurities();
			}
		}

		private void Edit_Click(object sender, RoutedEventArgs e)
		{
			var wnd = new SecurityEditWindow(_storage) { Security = SelectedSecurity, Sources = _sources };

			if (wnd.ShowModal(this))
			{
				_storage.Securities.Save(wnd.Security.Security);
				_securities.Add(wnd.Security);
			}
		}

		private void AddToVisualSecurities(IEnumerable<Security> securities)
		{
			_securities.AddRange(securities.Select(s => s.ToVisualSecurity()));
		}
	}
}