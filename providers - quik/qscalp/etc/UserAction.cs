// =======================================================================
//   UserAction.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// =======================================================================

using System.Windows.Forms;

namespace QScalp
{
  // ************************************************************************

  public enum TradeOp { Cancel, Buy, Sell, Upsize, Downsize, Close, Reverse }
  public enum BaseQuote { None, Counter, Similar, Absolute }

  // ************************************************************************

  public class UserAction
  {
    public TradeOp Operation;
    public BaseQuote Quote;
    public int Value;
    public int Quantity;

    public UserAction() { }

    public UserAction(TradeOp Operation, BaseQuote Quote, int Value, int Quantity)
    {
      this.Operation = Operation;
      this.Quote = Quote;
      this.Value = Value;
      this.Quantity = Quantity;
    }
  }

  // ************************************************************************

  public class KeyBinding
  {
    public Keys Key;
    public bool OnKeyDown;
    public UserAction Action;

    public KeyBinding() { }

    public KeyBinding(Keys Key, bool OnKeyDown,
      TradeOp Operation, BaseQuote Quote, int Value, int Quantity)
    {
      this.Key = Key;
      this.OnKeyDown = OnKeyDown;
      this.Action = new UserAction(Operation, Quote, Value, Quantity);
    }
  }

  // ************************************************************************
}
