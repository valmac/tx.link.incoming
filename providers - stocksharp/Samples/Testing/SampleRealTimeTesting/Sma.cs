namespace SampleRealTimeTesting
{
	using System;

	using StockSharp.Algo.Candles;

	/// <summary>
	/// Реализация идникатора простой скользящей средней.
	/// </summary>
	class Sma : Ma
	{
		/// <summary>
		/// Создать скользящуюю среднюю.
		/// </summary>
		/// <param name="length">Длина скользящей.</param>
		public Sma(int length)
			: base(length)
		{
		}

		/// <summary>
		/// Добавить новую свечку.
		/// </summary>
		/// <param name="candle">Новая свечка.</param>
		public override void Add(TimeFrameCandle candle)
		{
			if (candle == null)
				throw new ArgumentNullException("candle");

			var value = base.Value;

			// добавляем новое начало
			value += candle.ClosePrice / this.Length;
			base.Buffer.Add(candle);

			// если буффер стал достаточно большим (стал больше длины)
			if (base.BufferSize > base.Length)
			{
				// удаляем хвостовое значение
				value -= base.Buffer[0].ClosePrice / base.Length;
				base.Buffer.RemoveAt(0);
			}

			base.Value = value;
		}
	}
}