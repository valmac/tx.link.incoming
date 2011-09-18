namespace Sample
{
	using System;

	using StockSharp.BusinessEntities;

	public class SampleQuote
	{
		public SampleQuote(Quote quote)
		{
			if (quote == null)
				throw new ArgumentNullException("quote");

            this.Price = quote.Price;

			if (quote.OrderDirection == OrderDirections.Buy)
			{
				this.Bid = quote.Volume.ToString();
			}
			else
			{
				this.Ask = quote.Volume.ToString();
			}
		}

		public string Bid { get; private set; }
		public string Ask { get; private set; }
		public decimal Price { get; private set; }
	}
}