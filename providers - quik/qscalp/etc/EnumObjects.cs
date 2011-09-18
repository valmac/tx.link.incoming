// ========================================================================
//   EnumObjects.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// ========================================================================


namespace QScalp.EnumObjects
{
  // ************************************************************************

  class TradeOpObj
  {
    public readonly TradeOp Value;

    public TradeOpObj(TradeOp value) { this.Value = value; }
    public override string ToString() { return ToString(Value); }
    public override int GetHashCode() { return (int)Value; }

    public override bool Equals(object obj)
    {
      return obj is TradeOpObj && ((TradeOpObj)obj).Value == Value;
    }

    public static string ToString(TradeOp value)
    {
      switch(value)
      {
        case TradeOp.Buy:
          return "Покупка";
        case TradeOp.Sell:
          return "Продажа";
        case TradeOp.Upsize:
          return "Наращивание";
        case TradeOp.Downsize:
          return "Уменьшение";
        case TradeOp.Close:
          return "Закрытие";
        case TradeOp.Reverse:
          return "Разворот";
        case TradeOp.Cancel:
          return "Отмена";
      }

      return "?";
    }
  }

  // ************************************************************************

  class BaseQuoteObj
  {
    public readonly BaseQuote Value;

    bool genitiveCase;

    public BaseQuoteObj(BaseQuote value)
    {
      this.Value = value;
      this.genitiveCase = false;
    }

    public BaseQuoteObj(BaseQuote value, bool genitiveCase)
    {
      this.Value = value;
      this.genitiveCase = genitiveCase;
    }

    public override string ToString() { return ToString(Value, genitiveCase); }
    public override int GetHashCode() { return (int)Value; }

    public override bool Equals(object obj)
    {
      return obj is BaseQuoteObj && ((BaseQuoteObj)obj).Value == Value;
    }

    public static string ToString(BaseQuote value) { return ToString(value, false); }

    public static string ToString(BaseQuote value, bool genitiveCase)
    {
      switch(value)
      {
        case BaseQuote.None:
          return "n/a";
        case BaseQuote.Absolute:
          return genitiveCase ? "абсолютной" : "Абсолютная";
        case BaseQuote.Counter:
          return genitiveCase ? "встречной" : "Встречная";
        case BaseQuote.Similar:
          return genitiveCase ? "попутной" : "Попутная";
      }

      return "?";
    }
  }

  // ************************************************************************
}
