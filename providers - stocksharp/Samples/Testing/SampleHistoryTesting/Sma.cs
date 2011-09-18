namespace SampleHistoryTesting
{
	using System;
	using System.Collections.Generic;

	using StockSharp.Algo.Candles;

	/// <summary>
	/// Реализация идникатора скользящей средней.
	/// </summary>
	class Sma
	{
		private readonly List<TimeFrameCandle> _buffer = new List<TimeFrameCandle>();

		/// <summary>
		/// Создать скользящуюю среднюю.
		/// </summary>
		/// <param name="length">Длина скользящей.</param>
		public Sma(int length)
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
		public decimal Value { get; private set; }

		/// <summary>
		/// Размер буфера. После достаточного наполнения данными через метод <see cref="Add"/>
		/// длина станет равной <see cref="Length"/>.
		/// </summary>
		public int BufferSize { get { return _buffer.Count; } }

		/// <summary>
		/// Добавить новую свечку.
		/// </summary>
		/// <param name="candle">Новая свечка.</param>
		public void Add(TimeFrameCandle candle)
		{
			if (candle == null)
				throw new ArgumentNullException("candle");

			var value = this.Value;

			// добавляем новое начало
			value += candle.ClosePrice / this.Length;
			_buffer.Add(candle);

			// если буффер стал достаточно большим (стал больше длины)
			if (_buffer.Count > this.Length)
			{
				// удаляем хвостовое значение
				value -= _buffer[0].ClosePrice / this.Length;
				_buffer.RemoveAt(0);
			}

			this.Value = value;
		}
	}
}