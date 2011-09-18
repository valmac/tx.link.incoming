namespace StockSharp.Hydra
{
	using System;
	using System.Collections.Generic;
	using System.Collections.ObjectModel;
	using System.Linq;
	using System.Windows.Controls;
	using System.Windows.Input;

	using Ecng.Common;

	public partial class SecurityListView
	{
		private readonly ObservableCollection<VisualSecurity> _securities = new ObservableCollection<VisualSecurity>();

		public SecurityListView()
		{
			InitializeComponent();
			SecurityDetails.ItemsSource = _securities;
		}

		public bool AllowMultiSelect
		{
			get { return SecurityDetails.SelectionMode == SelectionMode.Multiple; }
			set { SecurityDetails.SelectionMode = value ? SelectionMode.Multiple : SelectionMode.Single; }
		}

		public event EventHandler<EventArgs> SelectionChanged;
		public event EventHandler<EventArgs> SecurityClicked;

		public IList<VisualSecurity> Securities
		{
			get { return _securities; }
		}

		public IEnumerable<VisualSecurity> SelectedSecurities
		{
			get { return SecurityDetails.SelectedItems.Cast<VisualSecurity>().ToArray(); }
		}

		private GridViewColumnCollection Columns
		{
			get { return ((GridView)SecurityDetails.View).Columns; }
		}

		private bool IsColumnVisible(GridViewColumn column)
		{
			return Columns.Contains(column);
		}

		private void ChangedColumnVisible(GridViewColumn column, bool isVisible)
		{
			if (isVisible)
				Columns.Add(column);
			else
				Columns.Remove(column);
		}

		public bool IsLastUpdateTimeColumnVisible
		{
			get { return IsColumnVisible(LastUpdateTimeColumn); }
			set { ChangedColumnVisible(LastUpdateTimeColumn, value); }
		}

		public bool IsSelectedColumnVisible
		{
			get { return IsColumnVisible(SelectedColumn); }
			set { ChangedColumnVisible(SelectedColumn, value); }
		}

		private void SecurityDetails_SelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			SelectionChanged.SafeInvoke(this);
		}

		private void HandleDoubleClick(object sender, MouseButtonEventArgs e)
		{
			SecurityClicked.SafeInvoke(this);
		}
	}
}