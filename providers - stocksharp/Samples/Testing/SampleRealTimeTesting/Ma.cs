namespace SampleRealTimeTesting
{
	using System.Collections.Generic;

	using StockSharp.Algo.Candles;

	abstract class Ma
	{
		protected readonly List<TimeFrameCandle> Buffer = new List<TimeFrameCandle>();

		/// <summary>
		/// Создать скользящуюю среднюю.
		/// </summary>
		/// <param name="length">Длина скользящей.</param>
		protected Ma(int length)
		{
			this.Length = length;
		}

		/// <summary>
		/// Длина скользящей.
		/// </summary>
		public int Length { get; private set; }

		/// <summary>
		/// Текущее значение.
		/// </summary>
		public decimal Value { get; protected set; }

		/// <summary>
		/// Размер буфера. После достаточного наполнения данными через метод <see cref="Add"/>
		/// длина станет равной <see cref="Length"/>.
		/// </summary>
		public int BufferSize { get { return Buffer.Count; } }

		/// <summary>
		/// Добавить новую свечку.
		/// </summary>
		/// <param name="candle">Новая свечка.</param>
		public abstract void Add(TimeFrameCandle candle);
	}
}