namespace StockSharp.Hydra
{
	using System;

	using Antlr3.ST;

	sealed class AdvancedDateTimeRenderer : IAttributeRenderer
	{
		public string ToString(object o)
		{
			return ToString(o, null);
		}

		public string ToString(object o, string formatName)
		{
			if (o == null)
				return null;

			return string.IsNullOrEmpty(formatName) ? o.ToString() : ((DateTime)o).ToString(formatName);
		}
	}
}