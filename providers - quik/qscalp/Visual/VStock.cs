// =====================================================================
//    VStock.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// =====================================================================

using System;
using System.Collections.Generic;
using System.Drawing;

namespace QScalp.Visual
{
  class VStock : IDisposable
  {
    // **********************************************************************

    class StockData
    {
      public readonly Object[,] table;
      public int ac;
      public int bc;
      public int pc;

      public StockData(Object[,] table)
      {
        this.table = table;
        ac = -1;
        bc = -1;
        pc = -1;
      }
    }

    // **********************************************************************

    LinkedList<VQuote> quotes;
    HashSet<int> orders;

    StockData data;

    int height;
    int basePriceY;

    Graphics bufGrfx;

    // **********************************************************************

    public int Ask { get; protected set; }
    public int Bid { get; protected set; }

    public bool DataExist { get { return data != null; } }

    public Bitmap Bitmap { get; protected set; }
    public bool Updated { get; protected set; }

    // **********************************************************************

    public VStock()
    {
      quotes = new LinkedList<VQuote>();
      orders = new HashSet<int>();

      height = 1;

      Bitmap = new Bitmap(1, 1);
      bufGrfx = Graphics.FromImage(Bitmap);
    }

    // **********************************************************************

    public int Height
    {
      get
      {
        return height;
      }
      set
      {
        height = (value > 0) ? value : 1;

        Bitmap = new Bitmap(VQuote.Width, height);

        Graphics g = bufGrfx;
        bufGrfx = Graphics.FromImage(Bitmap);
        g.Dispose();

        lock(quotes)
          foreach(VQuote q in quotes)
            q.Invalidate();

        RefreshList();
      }
    }

    // **********************************************************************

    public int GetY(int price)
    {
      return basePriceY - price * cfg.u.VQuoteHeight / cfg.u.PriceStep;
    }

    // **********************************************************************

    public int GetPriceFromY(int y)
    {
      return ((basePriceY - y - 1) / cfg.u.VQuoteHeight + 1) * cfg.u.PriceStep;
    }

    // **********************************************************************

    public bool QVisible(int price)
    {
      int y = GetY(price);
      return y + cfg.u.VQuoteHeight > 0 && y < height;
    }

    // **********************************************************************

    public void PaintGrid(Graphics grfx, int grid, Pen gpen, int width, int height)
    {
      int d = grid * cfg.u.VQuoteHeight / cfg.u.PriceStep;
      int y = (basePriceY + cfg.u.VQuoteHeight) % d - cfg.u.VQuoteHeight + cfg.u.VQuoteHeight / 2;

      height += cfg.u.VQuoteHeight;

      while(y < height)
      {
        grfx.DrawLine(gpen, 0, y, width, y);
        y += d;
      }
    }

    // **********************************************************************

    public void Rebuild()
    {
      lock(quotes)
        quotes.Clear();

      RefreshList();
    }

    // **********************************************************************

    public void CenterSpread()
    {
      basePriceY = (Ask + Bid) * cfg.u.VQuoteHeight / 2 / cfg.u.PriceStep + height / 2 - cfg.u.VQuoteHeight / 2;
      Rebuild();
    }

    // **********************************************************************

    public void Scroll(int offset)
    {
      basePriceY += offset;

      lock(quotes)
        foreach(VQuote q in quotes)
          q.Invalidate();

      RefreshList();
    }

    // **********************************************************************

    public void SetOrder(int price, bool active)
    {
      lock(orders)
      {
        if(active)
          orders.Add(price);
        else
          orders.Remove(price);
      }

      UpdateQuotes();
    }

    // **********************************************************************

    public bool Update(Object[,] table)
    {
      if(table.GetLength(0) < 2)
        return true;

      StockData data = new StockData(table);

      for(int col = 0; col < table.GetLength(1); col++)
        switch(table[0, col] as string)
        {
          case cfg.AskVolumeCN:
            data.ac = col;
            break;
          case cfg.BidVolumeCN:
            data.bc = col;
            break;
          case cfg.PriceCN:
            data.pc = col;
            break;
        }

      if(data.ac < 0 || data.bc < 0 || data.pc < 0)
        return true;

      // --------------------------------------------------------------------

      int ask = int.MaxValue;
      int bid = int.MinValue;

      for(int row = 1; row < table.GetLength(0); row++)
        if(table[row, data.ac] is double && table[row, data.bc] is double && table[row, data.pc] is double)
        {
          int price = Price.Conv((double)table[row, data.pc]);

          if(price < 0)
            return true;

          if((double)table[row, data.ac] > 0 && price < ask)
            ask = price;

          if((double)table[row, data.bc] > 0 && price > bid)
            bid = price;
        }
        else
          return true;

      if(ask == int.MaxValue || bid == int.MinValue)
        return true;

      // --------------------------------------------------------------------

      this.data = data;

      this.Ask = ask;
      this.Bid = bid;

      UpdateQuotes();

      return false;
    }

    // **********************************************************************

    void UpdateQuotes()
    {
      StockData data = this.data;

      lock(quotes)
      {
        if(data == null)
          foreach(VQuote q in quotes)
            q.SetFree(true);
        else
        {
          int row = 1;

          foreach(VQuote q in quotes)
          {
            // --------------------------------------------------------------

            while(row < data.table.GetLength(0))
            {
              int price = Price.Conv((double)data.table[row, data.pc]);

              if(price == q.price)
              {
                int v = (int)((double)data.table[row, data.ac]);

                if(v > 0)
                  if(price == Ask)
                    q.SetVolume(v, VQuote.Type.BestAsk);
                  else
                    q.SetVolume(v, VQuote.Type.Ask);
                else
                  if(price == Bid)
                    q.SetVolume((int)(double)data.table[row, data.bc], VQuote.Type.BestBid);
                  else
                    q.SetVolume((int)(double)data.table[row, data.bc], VQuote.Type.Bid);

                break;
              }
              else if(price < q.price || row + 1 >= data.table.GetLength(0))
              {
                if(q.price < Ask && q.price > Bid)
                  q.SetFree(true);
                else
                  q.SetFree(false);

                break;
              }

              row++;
            }

            // --------------------------------------------------------------

            lock(orders)
              q.Selected = orders.Contains(q.price);

            // --------------------------------------------------------------
          }
        }
      }

      Updated = true;
    }

    // **********************************************************************

    void RefreshList()
    {
      lock(quotes)
      {
        while(quotes.Count > 0 && !QVisible(quotes.First.Value.price))
          quotes.RemoveFirst();

        while(quotes.Count > 0 && !QVisible(quotes.Last.Value.price))
          quotes.RemoveLast();

        if(quotes.Count == 0)
          quotes.AddFirst(new VQuote(GetPriceFromY(0)));

        while(QVisible(quotes.First.Value.price + cfg.u.PriceStep))
          quotes.AddFirst(new VQuote(quotes.First.Value.price + cfg.u.PriceStep));

        while(QVisible(quotes.Last.Value.price - cfg.u.PriceStep))
          quotes.AddLast(new VQuote(quotes.Last.Value.price - cfg.u.PriceStep));
      }

      UpdateQuotes();
    }

    // **********************************************************************

    public void Paint()
    {
      Updated = false;

      lock(quotes)
        foreach(VQuote q in quotes)
          q.Paint(bufGrfx, GetY(q.price));
    }

    // **********************************************************************

    public void Invalidate()
    {
      Updated = true;
    }

    // **********************************************************************

    public void Dispose()
    {
      if(bufGrfx != null)
        bufGrfx.Dispose();
    }

    // **********************************************************************
  }
}
