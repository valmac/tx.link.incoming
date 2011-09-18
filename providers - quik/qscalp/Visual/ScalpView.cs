// ========================================================================
//    ScalpView.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// ========================================================================

using System;
using System.Drawing;
using System.Windows.Forms;

namespace QScalp.Visual
{
  class ScalpView : Control
  {
    VStock stock;
    VGraph graph;
    VClusters clusters;
    VPosition position;

    bool mouseScrolling;
    bool mouseTrading;
    int mouseY;

    int acOffset;

    Timer painting;
    Graphics grfx;

    // **********************************************************************

    protected override bool IsInputKey(Keys keyData) { return true; }

    // **********************************************************************

    public delegate void OnQuoteClickDelegate(MouseButtons button, int price);
    public OnQuoteClickDelegate OnQuoteClick { get; set; }

    public int AskPrice { get { return stock.Ask; } }
    public int BidPrice { get { return stock.Bid; } }

    public bool DataExist { get { return stock.DataExist; } }
    public TradeLog TradeLog { get { return position.TradeLog; } }

    public bool AutoCentering { get; protected set; }

    // **********************************************************************

    public ScalpView()
    {
      grfx = CreateGraphics();
      stock = new VStock();
      position = new VPosition(stock);
      clusters = new VClusters(stock);
      graph = new VGraph(stock, clusters);

      BackColor = cfg.BackColor;

      AutoCentering = true;
      acOffset = 0;

      painting = new Timer();
      painting.Interval = cfg.PaintingInterval;
      painting.Tick += new EventHandler(painting_Tick);

      painting_Tick(null, null);
      painting.Start();
    }

    // **********************************************************************

    void painting_Tick(object sender, EventArgs e)
    {
      if(AutoCentering)
      {
        int askY = stock.GetY(stock.Ask);
        int bidY = stock.GetY(stock.Bid);

        if((askY < stock.Height * cfg.u.CenteringStart / 100)
          || (bidY + cfg.u.VQuoteHeight > stock.Height * (100 - cfg.u.CenteringStart) / 100))
        {
          acOffset = (stock.Height - cfg.u.VQuoteHeight - askY - bidY) / 2;

          if(Math.Abs(acOffset) > stock.Height * 2)
          {
            stock.CenterSpread();
            position.Invalidate();
            clusters.Invalidate();
            graph.Invalidate();

            acOffset = 0;
          }
        }

        if(acOffset != 0)
        {
          int offset = acOffset / cfg.u.CenteringDiv;

          if(Math.Abs(offset) < cfg.u.CenteringMin)
            if(Math.Abs(acOffset) < cfg.u.CenteringMin)
              offset = acOffset;
            else
              offset = Math.Sign(acOffset) * cfg.u.CenteringMin;

          Scroll(offset);
          acOffset -= offset;
        }
      }

      // --------------------------------------------------------------------

      graph.TryCenterGuide();

      // --------------------------------------------------------------------

      if(stock.Updated)
      {
        stock.Paint();
        grfx.DrawImageUnscaled(stock.Bitmap, Width - VQuote.Width - cfg.u.VPositionWidth, 0);
      }

      // --------------------------------------------------------------------

      if(position.Updated)
      {
        position.Paint();
        grfx.DrawImageUnscaled(position.Bitmap, Width - cfg.u.VPositionWidth, 0);
      }

      // --------------------------------------------------------------------

      if(graph.Updated)
      {
        graph.Paint();
        grfx.DrawImageUnscaled(graph.Bitmap, VClusters.Width, 0);
      }

      // --------------------------------------------------------------------

      if(clusters.Updated)
      {
        clusters.Paint();
        grfx.DrawImageUnscaled(clusters.Bitmap, 0, 0);
      }
    }

    // **********************************************************************

    void Scroll(int offset)
    {
      stock.Scroll(offset);
      position.Invalidate();
      clusters.Invalidate();
      graph.Invalidate();
    }

    // **********************************************************************

    public bool UpdateTradesData(Object[,] table)
    {
      return graph.Update(table);
    }

    // **********************************************************************

    public bool UpdateStockData(Object[,] table)
    {
      int ask = stock.Ask;
      int bid = stock.Bid;

      bool r = stock.Update(table);

      if(stock.Ask != ask || stock.Bid != bid)
      {
        graph.UpdateSpread();
        position.Invalidate();
      }

      return r;
    }

    // **********************************************************************

    public void UpdatePosition(int price, int quantity)
    {
      position.Update(price, quantity);
    }

    // **********************************************************************

    public void ClearPosition()
    {
      position.Clear();
    }

    // **********************************************************************

    public void UpdateOrder(int price, bool active)
    {
      stock.SetOrder(price, active);
    }

    // **********************************************************************

    public void CenterSpread()
    {
      painting.Stop();

      acOffset = 0;

      stock.CenterSpread();
      position.Invalidate();
      clusters.Invalidate();
      graph.Invalidate();

      painting_Tick(null, null);
      painting.Start();

      AutoCentering = true;
    }

    // **********************************************************************

    public void Page(int n)
    {
      if(stock.DataExist)
      {
        AutoCentering = false;

        painting.Stop();
        Scroll(n * Height * cfg.u.ManualScrollSize / 100);
        painting_Tick(null, null);
        painting.Start();
      }
    }

    // **********************************************************************

    public void Rebuild()
    {
      painting.Stop();

      stock.Rebuild();
      position.Invalidate();
      clusters.Height = clusters.Height;
      graph.Size = new Size(Width - VClusters.Width - VQuote.Width - cfg.u.VPositionWidth, Height);

      painting_Tick(null, null);
      painting.Start();
    }

    // **********************************************************************

    protected override void OnResize(EventArgs e)
    {
      base.OnResize(e);

      if(Height > 0)
      {
        painting.Stop();

        Graphics g = grfx;
        grfx = CreateGraphics();
        g.Dispose();

        stock.Height = Height;
        position.Height = Height;
        clusters.Height = Height;
        graph.Size = new Size(Width - VClusters.Width - VQuote.Width - cfg.u.VPositionWidth, Height);

        if(!stock.DataExist)
          stock.CenterSpread();

        painting_Tick(null, null);
        painting.Start();
      }
    }

    // **********************************************************************

    protected override void OnPaint(PaintEventArgs e)
    {
      base.OnPaint(e);

      painting.Stop();

      stock.Invalidate();
      position.Invalidate();
      clusters.Invalidate();
      graph.Invalidate();

      painting_Tick(null, null);
      painting.Start();
    }

    // **********************************************************************

    protected override void OnMouseMove(MouseEventArgs e)
    {
      if(stock.DataExist)
      {
        if(mouseScrolling)
        {
          int delta = e.Y - mouseY;

          if(Math.Abs(delta) > cfg.u.VQuoteHeight / 2)
            AutoCentering = false;

          painting.Stop();
          Scroll(delta);
          painting_Tick(null, null);
          painting.Start();

          mouseY = e.Y;
        }
        else
        {
          bool mouseOverQuote = cfg.u.MouseWorkSize > 0
            && e.X >= Width - VQuote.Width - cfg.u.VPositionWidth
            && e.X < Width - cfg.u.VPositionWidth - 1;

          if(mouseTrading && !mouseOverQuote)
          {
            Cursor = Cursors.Default;
            mouseTrading = false;
          }
          else if(!mouseTrading && mouseOverQuote)
          {
            Cursor = Cursors.Cross;
            mouseTrading = true;
          }
        }
      }

      base.OnMouseMove(e);
    }

    // **********************************************************************

    protected override void OnMouseDown(MouseEventArgs e)
    {
      if(stock.DataExist)
      {
        if(mouseTrading)
        {
          if(OnQuoteClick != null)
            OnQuoteClick(e.Button, stock.GetPriceFromY(e.Y));
        }
        else if(e.Button == MouseButtons.Left)
        {
          Cursor = Cursors.Hand;
          mouseScrolling = true;
          mouseY = e.Y;

          if(e.Clicks > 1)
            CenterSpread();
        }
      }

      base.OnMouseDown(e);
    }

    // **********************************************************************

    protected override void OnMouseUp(MouseEventArgs e)
    {
      if(mouseScrolling)
      {
        Cursor = Cursors.Default;
        mouseScrolling = false;
      }

      base.OnMouseUp(e);
    }

    // **********************************************************************

    protected override void OnMouseCaptureChanged(EventArgs e)
    {
      if(mouseScrolling && !Capture)
      {
        Cursor = Cursors.Default;
        mouseScrolling = false;
      }

      base.OnMouseCaptureChanged(e);
    }

    // **********************************************************************

    protected override void OnMouseWheel(MouseEventArgs e)
    {
      const int WHEEL_DELTA = 120;
      Page(e.Delta / WHEEL_DELTA);
      base.OnMouseWheel(e);
    }

    // **********************************************************************

    protected override void Dispose(bool disposing)
    {
      if(graph != null)
        graph.Dispose();
      if(clusters != null)
        clusters.Dispose();
      if(position != null)
        position.Dispose();
      if(stock != null)
        stock.Dispose();
      if(graph != null)
        grfx.Dispose();

      base.Dispose(disposing);
    }

    // **********************************************************************
  }

}
