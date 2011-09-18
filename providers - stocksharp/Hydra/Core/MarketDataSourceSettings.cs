namespace StockSharp.Hydra.Core
{
	using System;
	using System.Collections.Generic;
	using System.ComponentModel;

	using Ecng.ComponentModel;
	using Ecng.Serialization;

	using StockSharp.BusinessEntities;

	/// <summary>
	/// Настройки источника <see cref="IMarketDataSource"/>.
	/// </summary>
	public class MarketDataSourceSettings : IExtendableEntity
	{
		/// <summary>
		/// Идентификатор источника <see cref="IMarketDataSource.Id"/>.
		/// </summary>
		[Identity]
		[Browsable(false)]
		public Guid SourceId { get; set; }

		/// <summary>
		/// Включен ли источник.
		/// </summary>
		[Category("Основные")]
		[DisplayName("Включен")]
		public virtual bool IsEnabled { get; set; }

		/// <summary>
		/// Работать с.
		/// </summary>
		[Category("Основные")]
		[DisplayName("Работать с")]
		public virtual TimeSpan WorkingFrom { get; set; }

		/// <summary>
		/// Работать до.
		/// </summary>
		[Category("Основные")]
		[DisplayName("Работать до")]
		public virtual TimeSpan WorkingTo { get; set; }

		/// <summary>
		/// Интервал работы.
		/// </summary>
		[Category("Основные")]
		[DisplayName("Интервал работы")]
		public virtual TimeSpan Interval { get; set; }

		/// <summary>
		/// Путь к директории, куда будут сохранятся скаченные файлы (если такие есть).
		/// </summary>
		[Category("Основные")]
		[DisplayName("Временная директория")]
		public virtual string DumpFolder { get; set; }

		/// <summary>
		/// Расширенная информация, храняющая в себе дополнительные настройки для источника.
		/// </summary>
		[Browsable(false)]
		public IDictionary<object, object> ExtensionInfo { get; set; }

		[field: NonSerialized]
		private PropertyChangedEventHandler _propertyChanged;

		event PropertyChangedEventHandler INotifyPropertyChanged.PropertyChanged
		{
			add { _propertyChanged += value; }
			remove { _propertyChanged -= value; }
		}

		void INotifyPropertyChangedEx.NotifyPropertyChanged(string info)
		{
			if (_propertyChanged != null)
				_propertyChanged(this, new PropertyChangedEventArgs(info));
		}
	}
}