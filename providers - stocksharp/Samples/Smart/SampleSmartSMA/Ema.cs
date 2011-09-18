namespace SampleSmartSMA
{
	using System;

	using StockSharp.Algo.Candles;

	/// <summary>
	/// Реализация идникатора экспоненциальной скользящей средней.
	/// </summary>
	class Ema : Ma
	{
		/// <summary>
		/// Создать скользящуюю среднюю.
		/// </summary>
		/// <param name="length">Длина скользящей.</param>
		public Ema(int length)
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

			if (base.BufferSize == 0)
				base.Value = candle.ClosePrice;
			else
			{
				var k = 2 / (base.Length + 1);
				base.Value = k * candle.ClosePrice + (1 - k) * base.Value;
			}

			base.Buffer.Add(candle);
		}
	}
}