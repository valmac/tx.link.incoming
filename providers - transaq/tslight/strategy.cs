using System;
using System.Collections.Generic;

namespace tslight
{
	/// Description of strategy.
	public class StrategyClass : Object
	{
		public int period1;
		public int period2;
		public bool LongPos;
		public bool ShortPos;
		public bool PrevLongPos;
		public bool PrevShortPos;
		public DateTime LastDT;
		public DateTime BuyDT;
		public DateTime SellDT;
		public DateTime ShortDT;
		public DateTime CoverDT;
		
		public IList<int> nBuy;
		public IList<int> nSell;
		public IList<int> nShort;
		public IList<int> nCover;
		
		//================================================================================
		public StrategyClass(int p1, int p2)
		{
			nBuy = new List<int> (1000);
			nSell = new List<int> (1000);
			nShort = new List<int> (1000);
			nCover = new List<int> (1000);
			InitParams(p1, p2);
		}
		
		//================================================================================
		public void InitParams(int p1, int p2)
		{
			period1 = p1;
			period2 = p2;
			
		}
		//================================================================================
		public void Handle(DataTable_candle Candles)
		{
			double price;
			double vEMA1 = Candles[0].close;
			double vEMA2 = Candles[0].close;
			double pEMA1 = vEMA1;
			double pEMA2 = vEMA2;
			DateTime zdt = DateTime.Parse("01.01.2000");
			BuyDT = zdt;
			SellDT = zdt;
			ShortDT = zdt;
			CoverDT = zdt;

			nBuy.Clear();
			nSell.Clear();
			nShort.Clear();
			nCover.Clear();
			
			for (int bar=0; bar<Candles.Count-1; bar++)
			{
				PrevLongPos = LongPos;
				PrevShortPos = ShortPos;
				
				price = Candles[bar].close;
				vEMA1 = vEMA1 + (price - vEMA1) / period1;
				vEMA2 = vEMA2 + (price - vEMA2) / period2;

				if (vEMA1 > vEMA2 && pEMA1 <= pEMA2)
				{
					if(!LongPos) 
					{
						LongPos = true;
						BuyDT = Candles[bar].date;
						nBuy.Add(bar+1);
					}
					if(ShortPos)
					{
						ShortPos = false;
						CoverDT = Candles[bar].date;
						nCover.Add(bar+1);
					}
				}
				if (vEMA1 < vEMA2 && pEMA1 >= pEMA2)
				{
					if(LongPos)
					{
						LongPos = false;
						SellDT = Candles[bar].date;
						nSell.Add(bar+1);
					}
					if(!ShortPos) 
					{
						ShortPos = true;
						ShortDT = Candles[bar].date;
						nShort.Add(bar+1);
					}
				}
				
				pEMA1 = vEMA1;
				pEMA2 = vEMA2;
				LastDT = Candles[bar].date;
			}
		}
		//================================================================================
	}
}
