namespace SampleAsyncTransactions
{
	using System;

	using StockSharp.BusinessEntities;
	using StockSharp.Quik;

	public class SampleQuote
	{
		public SampleQuote(Quote quote)
		{
			if (quote == null)
				throw new ArgumentNullException("quote");

			this.Quote = quote;
			this.Price = quote.Price;

			if (quote.OrderDirection == OrderDirections.Buy)
			{
				this.Bid = quote.Volume.ToString();

				// копируем информацию о собственном объеме на покупку
				this.OwnVolume = (int)quote.ExtensionInfo[DdeQuoteColumns.OwnBidVolume];
			}
			else
			{
				this.Ask = quote.Volume.ToString();

				// копируем информацию о собственном объеме на продажу
				this.OwnVolume = (int)quote.ExtensionInfo[DdeQuoteColumns.OwnAskVolume];
			}
		}

		public Quote Quote { get; private set; }

		public string Bid { get; private set; }
		public string Ask { get; private set; }
		public decimal Price { get; private set; }
		public bool HasOwnVolume { get { return this.OwnVolume > 0; } }
		public int OwnVolume { get; private set; }
	}
}