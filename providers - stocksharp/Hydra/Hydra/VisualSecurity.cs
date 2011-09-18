namespace StockSharp.Hydra
{
	using System;
	using System.ComponentModel;

	using Ecng.Collections;
	using Ecng.Common;

	using StockSharp.BusinessEntities;

	public enum VisualSecurityStates
	{
		None,
		Runned,
		Error,
	}

	public class VisualSecurity : INotifyPropertyChanged
	{
		public VisualSecurity(Security security)
		{
			if (security == null)
				throw new ArgumentNullException("security");

			Security = security;
		}

		public Security Security { get; private set; }

		public string TradeSource
		{
			get { return (string)Security.ExtensionInfo.TryGetValue("TradeSource") ?? string.Empty; }
			set
			{
				Security.ExtensionInfo["TradeSource"] = value;
				NotifyPropertyChanged("TradeSource");
			}
		}

		public string DepthSource
		{
			get { return (string)Security.ExtensionInfo.TryGetValue("DepthSource") ?? string.Empty; }
			set
			{
				Security.ExtensionInfo["DepthSource"] = value;
				NotifyPropertyChanged("DepthSource");
			}
		}

		public string Source
		{
			get { return (string)Security.ExtensionInfo.TryGetValue("Source") ?? string.Empty; }
			set { Security.ExtensionInfo["Source"] = value; }
		}

		private VisualSecurityStates _state;

		public VisualSecurityStates State
		{
			get { return _state; }
			set
			{
				_state = value;
				NotifyPropertyChanged("State");
			}
		}

		public bool IsSelected
		{
			get
			{
				var value = Security.ExtensionInfo.TryGetValue("IsSelected");
				return value != null && (bool)value;
			}
			set
			{
				Security.ExtensionInfo["IsSelected"] = value;
				NotifyPropertyChanged("IsSelected");
			}
		}

		public int TradeCount
		{
			get
			{
				var value = Security.ExtensionInfo.TryGetValue("TradeCount");
				return value != null ? (int)value : 0;
			}
			set
			{
				Security.ExtensionInfo["TradeCount"] = value;
				NotifyPropertyChanged("TradeCount");
			}
		}

		public int DepthCount
		{
			get
			{
				var value = Security.ExtensionInfo.TryGetValue("DepthCount");
				return value != null ? (int)value : 0;
			}
			set
			{
				Security.ExtensionInfo["DepthCount"] = value;
				NotifyPropertyChanged("DepthCount");
			}
		}

		public DateTime? LastUpdateTime
		{
			get
			{
				var lastUpdateTime = Security.ExtensionInfo.TryGetValue("LastUpdateTime");

				if (lastUpdateTime is DateTime)
					return (DateTime?)lastUpdateTime;
				else if (lastUpdateTime is long)
					return lastUpdateTime.To<DateTime>();

				return null;
			}
			set
			{
				Security.ExtensionInfo["LastUpdateTime"] = (value ?? DateTime.MinValue).Ticks;
				NotifyPropertyChanged("LastUpdateTime");
			}
		}

		[field: NonSerialized]
		private PropertyChangedEventHandler _propertyChanged;

		event PropertyChangedEventHandler INotifyPropertyChanged.PropertyChanged
		{
			add { _propertyChanged += value; }
			remove { _propertyChanged -= value; }
		}

		private void NotifyPropertyChanged(string info)
		{
			if (_propertyChanged != null)
				_propertyChanged(this, new PropertyChangedEventArgs(info));
		}
	}
}