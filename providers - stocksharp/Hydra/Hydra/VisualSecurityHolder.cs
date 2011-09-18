namespace StockSharp.Hydra
{
	using System;

	using Ecng.Collections;

	using StockSharp.BusinessEntities;

	static class VisualSecurityHolder
	{
		private static readonly SynchronizedDictionary<Security, VisualSecurity> _securities = new SynchronizedDictionary<Security, VisualSecurity>();

		public static VisualSecurity ToVisualSecurity(this Security security)
		{
			if (security == null)
				throw new ArgumentNullException("security");

			return _securities.SafeAdd(security, key => new VisualSecurity(key));
		}
	}
}