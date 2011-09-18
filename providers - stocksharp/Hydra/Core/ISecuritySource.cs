namespace StockSharp.Hydra.Core
{
	using System.Collections.Generic;

	using StockSharp.BusinessEntities;

	/// <summary>
	/// Интерфейс, описывающий источник информации об инструментах.
	/// </summary>
	public interface ISecuritySource
	{
		/// <summary>
		/// Получить новые инструменты.
		/// </summary>
		/// <returns>Новые инструменты.</returns>
		IEnumerable<Security> GetNewSecurities();
	}
}