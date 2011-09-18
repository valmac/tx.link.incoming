// ====================================================================
//    Price.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// ====================================================================

using System;
using System.Globalization;

namespace QScalp
{
  static class Price
  {
    // **********************************************************************

    public static int Floor(int price)
    {
      return price / cfg.u.PriceStep * cfg.u.PriceStep;
    }

    // **********************************************************************

    public static int Ceil(int price)
    {
      return ((price - 1) / cfg.u.PriceStep + 1) * cfg.u.PriceStep;
    }

    // **********************************************************************

    public static int Conv(double n)
    {
      return (int)Math.Round(n * cfg.u.PriceRatio);
    }

    // **********************************************************************

    public static int Conv(double n, int ratio)
    {
      return (int)Math.Round(n * ratio);
    }

    // **********************************************************************

    public static int Guide(double n)
    {
      return (int)Math.Round(n * cfg.u.GuideRatio);
    }

    // **********************************************************************

    public static double GetNum(int price)
    {
      return (double)price / cfg.u.PriceRatio;
    }

    // **********************************************************************

    public static string GetString(int price)
    {
      return ((double)price / cfg.u.PriceRatio).ToString("N", cfg.ci);
    }

    // **********************************************************************

    public static string GetString(int price, int ratio)
    {
      CultureInfo ci = new CultureInfo(cfg.Culture, true);
      ci.NumberFormat.NumberDecimalDigits = (int)Math.Log10(ratio);

      return ((double)price / ratio).ToString("N", ci);
    }

    // **********************************************************************
  }
}
