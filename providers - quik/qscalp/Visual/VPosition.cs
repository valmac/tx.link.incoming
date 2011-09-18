// ========================================================================
//    VPosition.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// ========================================================================

using System;
using System.Drawing;
using System.Drawing.Drawing2D;

namespace QScalp.Visual
{
  class VPosition : IDisposable
  {
    // **********************************************************************

    VStock stock;

    int pricesum;
    int quantity;

    int height;
    Graphics bufGrfx;

    // **********************************************************************

    public Bitmap Bitmap { get; protected set; }
    public bool Updated { get; protected set; }

    public TradeLog TradeLog { get; protected set; }

    // **********************************************************************

    public VPosition(VStock stock)
    {
      this.stock = stock;

      height = 1;

      Bitmap = new Bitmap(1, 1);
      bufGrfx = Graphics.FromImage(Bitmap);

      pricesum = 0;
      quantity = 0;

      TradeLog = new TradeLog();
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

        Bitmap = new Bitmap(cfg.u.VPositionWidth, height);

        Graphics g = bufGrfx;
        bufGrfx = Graphics.FromImage(Bitmap);
        g.Dispose();

        Updated = true;
      }
    }

    // **********************************************************************

    public void Clear()
    {
      pricesum = 0;
      quantity = 0;

      TradeLog.Clear();

      Updated = true;
    }

    // **********************************************************************

    public void Update(int price, int quantity)
    {
      TradeLog.AddTurnover(Math.Abs(quantity));

      int nq = this.quantity + quantity;

      if(Math.Sign(nq) != Math.Sign(this.quantity))
      {
        if(this.quantity != 0)
          TradeLog.AddResult(price * this.quantity - this.pricesum);

        if(nq > 0)
          TradeLog.NewTrade("Long");
        else if(nq < 0)
          TradeLog.NewTrade("Short");

        this.pricesum = price * nq;
      }
      else
        this.pricesum += price * quantity;

      this.quantity = nq;

      Updated = true;
    }

    // **********************************************************************

    public void Paint()
    {
      Updated = false;

      bufGrfx.FillRectangle(cfg.BackBrush, 0, 0, cfg.u.VPositionWidth, height);
      stock.PaintGrid(bufGrfx, cfg.u.Grid2Step, cfg.GridLine2Pen, cfg.u.VPositionWidth, height);

      // --------------------------------------------------------------------

      if(quantity != 0)
      {
        Brush b = cfg.BackBrush;
        int y1, y2, pstrY, deltaY;
        string pstr = "";
        string delta = "";

        if(quantity > 0)
        {
          int price = Price.Ceil(pricesum / quantity);

          if(price < stock.Bid)
          {
            b = cfg.PPBackBrush;
            y1 = stock.GetY(stock.Bid);
            y2 = stock.GetY(price);

            pstrY = y2;
            deltaY = y1;
          }
          else if(price > stock.Bid)
          {
            b = cfg.LPBackBrush;
            y1 = stock.GetY(price);
            y2 = stock.GetY(stock.Bid);

            pstrY = y1;
            deltaY = y2;
          }
          else
          {
            b = cfg.PBackBrush;
            y1 = y2 = pstrY = deltaY = stock.GetY(price);
          }

          pstr = "L " + quantity.ToString();
          delta = Price.GetString(stock.Bid - price);
        }
        else
        {
          int price = Price.Floor(pricesum / quantity);

          if(price > stock.Ask)
          {
            b = cfg.PPBackBrush;
            y1 = stock.GetY(price);
            y2 = stock.GetY(stock.Ask);

            pstrY = y1;
            deltaY = y2;
          }
          else if(price < stock.Ask)
          {
            b = cfg.LPBackBrush;
            y1 = stock.GetY(stock.Ask);
            y2 = stock.GetY(price);

            pstrY = y2;
            deltaY = y1;
          }
          else
          {
            b = cfg.PBackBrush;
            y1 = y2 = pstrY = deltaY = stock.GetY(price);
          }

          pstr = "S " + (-quantity).ToString();
          delta = Price.GetString(price - stock.Ask);
        }

        // ------------------------------------------------------------------

        Point[] points;

        if(pstrY != deltaY)
        {
          points = new Point[] {
          new Point(0, y1 + cfg.u.VQuoteHeight / 2),
          new Point(cfg.u.VQuoteHeight / 2, y1),
          new Point(cfg.u.VPositionWidth - 1, y1),
          new Point(cfg.u.VPositionWidth - 1, y2 + cfg.u.VQuoteHeight - 1),
          new Point(cfg.u.VQuoteHeight / 2, y2 + cfg.u.VQuoteHeight - 1),
          new Point(0, y2 + cfg.u.VQuoteHeight / 2),
          new Point(cfg.u.VQuoteHeight / 2, y2),
          new Point(cfg.u.VPositionWidth - cfg.u.VQuoteHeight - 1, y2),
          new Point(cfg.u.VPositionWidth - cfg.u.VQuoteHeight - 1, y1 + cfg.u.VQuoteHeight - 1),
          new Point(cfg.u.VQuoteHeight / 2, y1 + cfg.u.VQuoteHeight - 1) };

          bufGrfx.FillPolygon(b, points, FillMode.Winding);
          bufGrfx.DrawPolygon(cfg.PositionPen, points);

          bufGrfx.DrawString(delta, cfg.Font, cfg.ForeBrush,
            cfg.u.VPositionWidth - bufGrfx.MeasureString(delta, cfg.Font).ToSize().Width - cfg.StringMargin - 1,
            deltaY + cfg.u.VQuoteHeight / 2 - cfg.StringHeight / 2);
        }

        points = new Point[] {
          new Point(0, pstrY + cfg.u.VQuoteHeight / 2),
          new Point(cfg.u.VQuoteHeight / 2, pstrY),
          new Point(cfg.u.VPositionWidth - 1, pstrY),
          new Point(cfg.u.VPositionWidth - 1, pstrY + cfg.u.VQuoteHeight - 1),
          new Point(cfg.u.VQuoteHeight / 2, pstrY + cfg.u.VQuoteHeight - 1),
          new Point(0, pstrY + cfg.u.VQuoteHeight / 2) };

        bufGrfx.FillPolygon(cfg.PBackBrush, points, FillMode.Winding);
        bufGrfx.DrawPolygon(new Pen(cfg.PBackBrush, 1), points);

        bufGrfx.DrawString(pstr, cfg.Font, cfg.ForeBrush,
          cfg.u.VPositionWidth - bufGrfx.MeasureString(pstr, cfg.Font).ToSize().Width - cfg.StringMargin - 1,
          pstrY + cfg.u.VQuoteHeight / 2 - cfg.StringHeight / 2);

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
