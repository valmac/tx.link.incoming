// =====================================================================
//    VCluster.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// =====================================================================

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Threading;

namespace QScalp.Visual
{
  class VGraph : IDisposable
  {
    enum Operation { Buy, Sell, Unknown }

    // **********************************************************************

    struct Trade
    {
      public int price;
      public int quantity;
      public Operation op;

      public Trade(int price, int quantity, Operation op)
      {
        this.price = price;
        this.quantity = quantity;
        this.op = op;
      }
    }

    // **********************************************************************

    struct Spread
    {
      public int ask;
      public int bid;

      public Spread(int ask, int bid)
      {
        this.ask = ask;
        this.bid = bid;
      }
    }

    // **********************************************************************

    struct Columns
    {
      public int date;
      public int time;
      public int price;
      public int qty;
      public int op;
      public int scode;
      public int ccode;

      public void Reset()
      {
        date = -1;
        time = -1;
        price = -1;
        qty = -1;
        op = -1;
        scode = -1;
        ccode = -1;
      }
    }

    // **********************************************************************

    LinkedList<Trade> trades;
    int maxTrades;

    LinkedList<Spread> spreads;
    int maxSpreads;

    LinkedList<int> guides;
    int maxGuides;
    int guideBaseY;
    int gcOffset;

    Timer tradesTicking;
    Timer spreadTicking;

    Columns cols;

    VStock stock;
    VClusters clusters;

    Graphics bufGrfx;
    int width, height;

    // **********************************************************************

    public Bitmap Bitmap { get; protected set; }
    public bool Updated { get; protected set; }

    // **********************************************************************

    public VGraph(VStock stock, VClusters clusters)
    {
      this.stock = stock;
      this.clusters = clusters;

      trades = new LinkedList<Trade>();
      maxTrades = cfg.u.MinGrpahTicks;

      spreads = new LinkedList<Spread>();
      maxSpreads = cfg.u.MinGrpahTicks;

      guides = new LinkedList<int>();
      maxGuides = cfg.u.MinGrpahTicks;

      tradesTicking = new Timer(TradesTick);
      spreadTicking = new Timer(SpreadTick);

      cols.Reset();

      Bitmap = new Bitmap(1, 1);
      width = 1;
      height = 1;

      bufGrfx = Graphics.FromImage(Bitmap);
    }

    // **********************************************************************

    public Size Size
    {
      get
      {
        return new Size(width, height);
      }
      set
      {
        width = (value.Width > 0) ? value.Width : 1;
        height = (value.Height > 0) ? value.Height : 1;

        Bitmap = new Bitmap(width, height);

        Graphics g = bufGrfx;
        bufGrfx = Graphics.FromImage(Bitmap);
        g.Dispose();

        maxTrades = (width - 1) / cfg.u.TradeBallWidth + 1;
        if(maxTrades < cfg.u.MinGrpahTicks)
          maxTrades = cfg.u.MinGrpahTicks;

        maxSpreads = (width - 1) / cfg.u.SpreadXScale + 2;
        if(maxSpreads < cfg.u.MinGrpahTicks)
          maxSpreads = cfg.u.MinGrpahTicks;

        maxGuides = (width - 1) / cfg.u.GuideXScale + 2;
        if(maxGuides < cfg.u.MinGrpahTicks)
          maxGuides = cfg.u.MinGrpahTicks;

        Updated = true;
      }
    }

    // **********************************************************************

    public void Clear()
    {
      tradesTicking.Change(Timeout.Infinite, Timeout.Infinite);
      spreadTicking.Change(Timeout.Infinite, Timeout.Infinite);

      lock(trades)
        trades.Clear();

      lock(spreads)
        spreads.Clear();

      lock(guides)
        guides.Clear();

      guideBaseY = 0;
      gcOffset = 0;

      Updated = true;
    }

    // **********************************************************************

    void TradesTick(Object state)
    {
      lock(trades)
      {
        while(trades.Count >= maxTrades)
          trades.RemoveLast();

        trades.AddFirst(new Trade(0, 0, Operation.Unknown));
      }

      Updated = true;
    }

    // **********************************************************************

    void SpreadTick(Object state)
    {
      lock(spreads)
      {
        while(spreads.Count >= maxSpreads)
          spreads.RemoveLast();

        spreads.AddFirst(new Spread(stock.Ask, stock.Bid));
      }

      Updated = true;
    }

    // **********************************************************************

    public bool Update(Object[,] table)
    {
      bool firstTable = true;

      for(int col = 0; col < table.GetLength(1); col++)
        if(!(table[0, col] is string))
        {
          firstTable = false;
          break;
        }

      // --------------------------------------------------------------------

      int row = 0;

      if(firstTable)
      {
        cols.Reset();

        for(int col = 0; col < table.GetLength(1); col++)
          switch((string)table[0, col])
          {
            case cfg.TradeDateCN:
              cols.date = col;
              break;
            case cfg.TradeTimeCN:
              cols.time = col;
              break;
            case cfg.TradePriceCN:
              cols.price = col;
              break;
            case cfg.TradeQuantityCN:
              cols.qty = col;
              break;
            case cfg.TradeOperationCN:
              cols.op = col;
              break;
            case cfg.TradeSecCN:
              cols.scode = col;
              break;
            case cfg.TradeClassCN:
              cols.ccode = col;
              break;
          }

        clusters.Clear();

        row = 1;
      }

      // --------------------------------------------------------------------

      if(cols.date < 0
        || cols.time < 0
        || cols.price < 0
        || cols.qty < 0
        || cols.op < 0
        || cols.scode < 0
        || cols.ccode < 0)
        return true;

      // --------------------------------------------------------------------

      bool error = false;

      while(row < table.GetLength(0))
      {
        if(table[row, cols.date] is string
          && table[row, cols.time] is string
          && table[row, cols.price] is double
          && table[row, cols.qty] is double
          && table[row, cols.op] is string
          && table[row, cols.scode] is string
          && table[row, cols.ccode] is string)
        {
          string SecCode = (string)table[row, cols.scode];
          string ClassCode = (string)table[row, cols.ccode];

          if(SecCode == cfg.u.SecCode && ClassCode == cfg.u.ClassCode)
          {
            Trade t;

            t.price = Price.Conv((double)table[row, cols.price]);
            t.quantity = (int)(double)table[row, cols.qty];

            // ------------------------------------------------------

            if(t.quantity >= cfg.u.TradeVolume1)
            {
              switch((string)table[row, cols.op])
              {
                case cfg.TradeBuyOp:
                  t.op = Operation.Buy;
                  break;
                case cfg.TradeSellOp:
                  t.op = Operation.Sell;
                  break;
                default:
                  t.op = Operation.Unknown;
                  break;
              }

              lock(trades)
              {
                while(trades.Count >= maxTrades)
                  trades.RemoveLast();

                trades.AddFirst(t);
              }

              tradesTicking.Change(cfg.u.TradesTickInterval, cfg.u.TradesTickInterval);

              Updated = true;
            }

            // ------------------------------------------------------

            DateTime dt;

            if(DateTime.TryParse((string)table[row, cols.date] + " " + (string)table[row, cols.time], out dt))
              clusters.Add(t.price, t.quantity, dt);
            else
              error = true;

            // ------------------------------------------------------
          }
          else if(SecCode == cfg.u.GuideSecCode && ClassCode == cfg.u.GuideClassCode)
          {
            lock(guides)
            {
              while(guides.Count >= maxGuides)
                guides.RemoveLast();

              guides.AddFirst(Price.Guide((double)table[row, cols.price]));
            }

            Updated = true;
          }
        }
        else
          error = true;

        row++;
      }

      return error;
    }

    // **********************************************************************

    public void UpdateSpread()
    {
      spreadTicking.Change(0, cfg.u.SpreadsTickInterval);
    }

    // **********************************************************************

    public void TryCenterGuide()
    {
      lock(guides)
        if(guides.Count > 0)
        {
          int fy = guideBaseY - guides.First.Value / cfg.u.GuideStep * cfg.u.GuideYScale;

          if((fy < height * cfg.u.GuideCenteringStart / 100)
            || (fy + cfg.u.GuideYScale > height * (100 - cfg.u.GuideCenteringStart) / 100))
          {
            gcOffset = (height - cfg.u.GuideYScale) / 2 - fy;
            gcOffset += Math.Sign(gcOffset) * height * cfg.u.GuideCenteringShift / 100;

            if(Math.Abs(gcOffset) > height * 2)
            {
              guideBaseY += gcOffset;
              gcOffset = 0;
            }
          }

          if(gcOffset != 0)
          {
            int offset = gcOffset / cfg.u.GuideCenteringDiv;

            if(Math.Abs(offset) < cfg.u.GuideCenteringMin)
              if(Math.Abs(gcOffset) < cfg.u.GuideCenteringMin)
                offset = gcOffset;
              else
                offset = Math.Sign(gcOffset) * cfg.u.GuideCenteringMin;

            guideBaseY += offset;
            gcOffset -= offset;

            Updated = true;
          }
        }
    }

    // **********************************************************************

    public void Paint()
    {
      Updated = false;

      bufGrfx.FillRectangle(cfg.BackBrush, 0, 0, width - 1, height);
      bufGrfx.DrawLine(cfg.BlockLinePen, width - 1, 0, width - 1, height);

      // --------------------------------------------------------------------

      stock.PaintGrid(bufGrfx, cfg.u.Grid1Step, cfg.GridLine1Pen, width - 1, height);
      stock.PaintGrid(bufGrfx, cfg.u.Grid2Step, cfg.GridLine2Pen, width - 1, height);

      // --------------------------------------------------------------------

      lock(spreads)
        if(spreads.Count > 1)
        {
          Point[] asks = new Point[spreads.Count];
          Point[] bids = new Point[spreads.Count];
          int i = 0;

          foreach(Spread s in spreads)
          {
            asks[i] = new Point(width - i * cfg.u.SpreadXScale - 2, stock.GetY(s.ask) + cfg.u.VQuoteHeight / 2);
            bids[i] = new Point(width - i * cfg.u.SpreadXScale - 2, stock.GetY(s.bid) + cfg.u.VQuoteHeight / 2);
            i++;
          }

          bufGrfx.DrawLines(cfg.AskGraphPen, asks);
          bufGrfx.DrawLines(cfg.BidGraphPen, bids);
        }

      // --------------------------------------------------------------------

      lock(guides)
      {
        if(guides.Count > 1)
        {
          Point[] gg = new Point[guides.Count];
          int i = 0;

          foreach(int n in guides)
          {
            gg[i] = new Point(width - i * cfg.u.GuideXScale - 2, guideBaseY - n / cfg.u.GuideStep * cfg.u.GuideYScale);
            i++;
          }

          bufGrfx.DrawLines(cfg.GuideGraphPen, gg);
        }
      }

      // --------------------------------------------------------------------

      int x = width - 1;

      lock(trades)
        foreach(Trade t in trades)
        {
          int m;

          if(t.quantity < cfg.u.TradeVolume2)
            m = 1;
          else if(t.quantity < cfg.u.TradeVolume3)
            m = 2;
          else
            m = 3;

          if(t.price > 0)
          {
            Brush b;
            switch(t.op)
            {
              case Operation.Buy:
                b = cfg.BestBidBackBrush;
                break;
              case Operation.Sell:
                b = cfg.BestAskBackBrush;
                break;
              default:
                b = cfg.BackBrush;
                break;
            }

            int y = stock.GetY(t.price) + cfg.u.VQuoteHeight / 2 - (cfg.u.TradeBallWidth * m) / 2;

            x -= cfg.u.TradeBallWidth * m;

            bufGrfx.FillEllipse(b,
              x, y, cfg.u.TradeBallWidth * m - 1, cfg.u.TradeBallWidth * m - 1);
            bufGrfx.DrawEllipse(cfg.TradeArcPen,
              x, y, cfg.u.TradeBallWidth * m - 1, cfg.u.TradeBallWidth * m - 1);

            if(m >= 3)
            {
              int q = (int)Math.Round(t.quantity / Math.Pow(10, (int)Math.Log10(cfg.u.TradeVolume3)));
              string s = q > 99 ? "\x2206" : q.ToString();
              int w = bufGrfx.MeasureString(s, cfg.SelectedFont).ToSize().Width;

              bufGrfx.DrawString(s, cfg.SelectedFont, cfg.ForeBrush,
                x + (cfg.u.TradeBallWidth * m - w) / 2 - 1,
                y + (cfg.u.TradeBallWidth * m - cfg.StringHeight) / 2);
            }
          }
          else
            x -= cfg.u.TradeBallWidth;

        }

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
