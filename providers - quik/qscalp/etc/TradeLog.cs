// =====================================================================
//   TradeLog.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// =====================================================================

using System;
using System.Text;

namespace QScalp
{
  public class TradeLog
  {
    // **********************************************************************

    //class Trade
    //{
    //  public string type;
    //  public int result;
    //  public DateTime open;
    //  public DateTime close;

    //  public Trade(string type)
    //  {
    //    this.type = type;
    //    this.open = DateTime.Now;
    //    this.close = new DateTime();
    //    this.result = 0;
    //  }
    //}

    // **********************************************************************

    public int LastResult { get; protected set; }

    public int TradesCount { get; protected set; }
    public int Turnover { get; protected set; }

    public int OverallResult { get; protected set; }
    public int AverageResult { get { return TradesCount == 0 ? 0 : OverallResult / TradesCount; } }

    public int OverallResultPerLot { get { return Turnover == 0 ? 0 : OverallResult * TradesCount * 2 / Turnover; } }
    public int AverageResultPerLot { get { return Turnover == 0 ? 0 : OverallResult * 2 / Turnover; } }

    public bool Updated { get { return updated && !(updated = false); } }
    public bool Cleared { get { return cleared && !(cleared = false); } }

    // **********************************************************************

    StringBuilder data;
    bool updated, cleared;

    // **********************************************************************

    public TradeLog()
    {
      data = new StringBuilder();
      Clear();
    }

    // **********************************************************************

    public void NewTrade(string type)
    {
      lock(data)
        data.AppendFormat("\r\n{0}\t{1:HH:mm:ss.fff}", type, DateTime.Now);
    }

    // **********************************************************************

    public void AddResult(int result)
    {
      lock(data)
        data.AppendFormat("\t{0:HH:mm:ss.fff}\t{1}", DateTime.Now, Price.GetString(result));

      LastResult = result;
      OverallResult += result;
      TradesCount++;

      updated = true;
    }

    // **********************************************************************

    public void AddTurnover(int turnover)
    {
      Turnover += turnover;
      updated = true;
    }

    // **********************************************************************

    public string GetText(int startIndex)
    {
      lock(data)
      {
        if(startIndex >= 0 && startIndex < data.Length)
          return data.ToString(startIndex, data.Length - startIndex);
        else
          return "";
      }
    }

    // **********************************************************************

    public void Clear()
    {
      lock(data)
      {
        data.Length = 0;
        data.Append("Напр.\tОткрытие\tЗакрытие\tРезультат");
      }

      LastResult = 0;
      OverallResult = 0;
      TradesCount = 0;
      Turnover = 0;

      updated = true;
      cleared = true;
    }

    // **********************************************************************
  }
}
