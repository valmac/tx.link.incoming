// =====================================================================
//    VQuote.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// =====================================================================

using System.Drawing;

namespace QScalp.Visual
{
  class VQuote
  {
    public enum Type { Ask, Bid, BestAsk, BestBid }

    // **********************************************************************

    public readonly int price;
    readonly string sprice;

    string cps, cvs;
    int vgWidth;
    Brush backBrush;
    Font font;

    bool repaint;

    // **********************************************************************

    public VQuote(int price)
    {
      this.price = price;
      sprice = Price.GetString(price);

      cps = cvs = cfg.SpreadString;
      vgWidth = 0;
      backBrush = cfg.BackBrush;
      font = cfg.Font;

      repaint = false;
    }

    // **********************************************************************

    public bool Selected
    {
      set
      {
        Font f;
        if(value)
          f = cfg.SelectedFont;
        else
          f = cfg.Font;

        if(font != f)
        {
          font = f;
          repaint = true;
        }
      }
    }

    // **********************************************************************

    public void SetFree(bool spread)
    {
      if(spread)
      {
        if(cps != cfg.SpreadString)
        {
          cps = cfg.SpreadString;
          repaint = true;
        }
      }
      else
      {
        if(cps != sprice)
        {
          cps = sprice;
          repaint = true;
        }
      }

      if(cvs.Length != 0)
      {
        cvs = "";
        vgWidth = 0;
        backBrush = cfg.BackBrush;
        repaint = true;
      }
    }

    // **********************************************************************

    public void SetVolume(int volume, Type type)
    {
      if(cps != sprice)
      {
        cps = sprice;
        repaint = true;
      }

      string vs = volume.ToString();

      if(cvs != vs)
      {
        cvs = vs;

        if(volume > cfg.u.FullVolume)
          vgWidth = cfg.u.VQuoteVolumeWidth;
        else
          vgWidth = volume * cfg.u.VQuoteVolumeWidth / cfg.u.FullVolume;

        repaint = true;
      }

      Brush b;

      switch(type)
      {
        case Type.Ask:
          b = cfg.AskBackBrush;
          break;
        case Type.Bid:
          b = cfg.BidBackBrush;
          break;
        case Type.BestAsk:
          b = cfg.BestAskBackBrush;
          break;
        case Type.BestBid:
          b = cfg.BestBidBackBrush;
          break;
        default:
          b = cfg.BackBrush;
          break;
      }

      if(backBrush != b)
      {
        backBrush = b;
        repaint = true;
      }
    }

    // **********************************************************************

    public void Invalidate()
    {
      repaint = true;
    }

    // **********************************************************************

    static public int Width { get { return cfg.u.VQuoteVolumeWidth + cfg.u.VQuotePriceWidth + 2; } }

    // **********************************************************************

    public void Paint(Graphics grfx, int top)
    {
      if(repaint)
      {
        int w = Width;
        int tY = top + cfg.u.VQuoteHeight / 2 - cfg.StringHeight / 2;

        // ------------------------------------------------------------------
        grfx.FillRectangle(backBrush, 0, top, w - 1, cfg.u.VQuoteHeight);

        // ------------------------------------------------------------------
        if(price % cfg.u.Grid2Step == 0)
          grfx.DrawLine(cfg.GridLine2Pen, 0, top + cfg.u.VQuoteHeight / 2, w - 1, top + cfg.u.VQuoteHeight / 2);
        else if(price % cfg.u.Grid1Step == 0)
          grfx.DrawLine(cfg.GridLine1Pen, 0, top + cfg.u.VQuoteHeight / 2, w - 1, top + cfg.u.VQuoteHeight / 2);

        grfx.DrawLine(cfg.VLinePen, cfg.u.VQuoteVolumeWidth, top, cfg.u.VQuoteVolumeWidth, top + cfg.u.VQuoteHeight - 1);
        grfx.DrawLine(cfg.VLinePen, w - 1, top, w - 1, top + cfg.u.VQuoteHeight - 1);

        // ------------------------------------------------------------------
        grfx.FillRectangle(cfg.VolumeFillBrush, 0, tY, vgWidth, cfg.StringHeight);

        // ------------------------------------------------------------------
        grfx.DrawString(cvs, font, cfg.ForeBrush, cfg.StringMargin, tY);
        grfx.DrawString(cps, font, cfg.ForeBrush, cfg.u.VQuoteVolumeWidth + cfg.StringMargin, tY);

        repaint = false;
      }
    }

    // **********************************************************************
  }
}
