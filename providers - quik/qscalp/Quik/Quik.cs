// ====================================================================
//    Quik.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// ====================================================================

using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using System.Windows.Forms;

using QScalp.EnumObjects;

using Trans2QuikAPI;

namespace QScalp.QuikTransaction
{
  static class Quik
  {
    // **********************************************************************

    public enum Connection { None, Partial, Full }

    // ----------------------------------------------------------------------

    enum TStatus { Execute, Cancel, Canceled }

    // ----------------------------------------------------------------------

    class Transaction
    {
      public TStatus status;
      public int tid;
      public double oid;
      public int price;

      public UserAction action;

      public Transaction(UserAction action)
      {
        this.status = TStatus.Execute;
        this.tid = 0;
        this.oid = 0;
        this.price = 0;

        this.action = action;
      }
    }

    // **********************************************************************

    static int transId;
    static int error;
    static StringBuilder msg;

    static bool working;
    static int ask, bid;

    static System.Threading.Timer connecting;
    static bool tryConnectDone;

    static StreamWriter log;

    static bool connectionUpdated;
    static string connectionText;

    static LinkedList<Transaction> tlist;
    static bool tlistUpdated;
    static StringBuilder tlistText;

    static int position;
    static bool positionUpdated;

    // **********************************************************************

    public static bool ConnectionUpdated
    {
      get { return connectionUpdated && !(connectionUpdated = false); }
    }

    public static string ConnectionText
    {
      get { return connectionText; }
      private set { connectionText = value; connectionUpdated = true; }
    }

    public static Connection Connected { get; private set; }

    // ----------------------------------------------------------------------

    public static bool PositionUpdated
    {
      get { return positionUpdated && !(positionUpdated = false); }
    }

    public static int Position
    {
      get { return position; }
      private set { position = value; positionUpdated = true; }
    }

    // ----------------------------------------------------------------------

    public static bool QueueUpdated
    {
      get { return tlistUpdated && !(tlistUpdated = false); }
    }

    public static int QueueLength { get; private set; }
    public static string QueueText { get; private set; }

    // **********************************************************************

    public delegate void OnNewTradeDelegate(int price, int quantity);
    public static OnNewTradeDelegate OnNewTrade { private get; set; }

    public delegate void OnOrderUpdateDelegate(int price, bool active);
    public static OnOrderUpdateDelegate OnOrderUpdate { private get; set; }

    // **********************************************************************

    static Quik()
    {
      transId = 0;
      error = 0;
      msg = new StringBuilder(256);

      working = false;
      ConnectionText = "";

      Position = 0;

      tlist = new LinkedList<Transaction>();
      tlistText = new StringBuilder(128);
      ProcessTList();

      connecting = new System.Threading.Timer(TryConnect);
      tryConnectDone = false;
    }

    // **********************************************************************

    static void WriteLog(string str)
    {
      if(log != null)
        try
        {
          lock(log)
            log.WriteLine(DateTime.Now.ToString("dd.MM.yyyy HH:mm:ss.fff") + " >;"
              + tlist.Count + ";" + Position + ";" + ask + ";" + bid + ";;"
              + str);
        }
        catch(Exception e)
        {
          MessageBox.Show("Ошибка записи файла протокола работы:\n" + e.Message,
            cfg.ProgName, MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
        }
    }

    // **********************************************************************
    // *                             Соединение                             *
    // **********************************************************************

    static void TryConnect(Object state)
    {
      if(working)
      {
        tryConnectDone = false;

        Trans2Quik.Result result = Trans2Quik.CONNECT(cfg.u.QuikFolder, out error, msg, msg.Capacity);
        if(result != Trans2Quik.Result.SUCCESS && result != Trans2Quik.Result.ALREADY_CONNECTED_TO_QUIK)
        {
          Connected = Connection.None;
          ConnectionText = msg.ToString();
          return;
        }

        if(
          Trans2Quik.SET_CONNECTION_STATUS_CALLBACK(StatusCallback, out error, msg, msg.Capacity) != Trans2Quik.Result.SUCCESS
          ||
          Trans2Quik.SET_TRANSACTIONS_REPLY_CALLBACK(TransactionReplyCallback, out error, msg, msg.Capacity) != Trans2Quik.Result.SUCCESS
          )
        {
          Connected = Connection.Partial;
          ConnectionText = msg.ToString();
          return;
        }

        if(Trans2Quik.SUBSCRIBE_ORDERS(cfg.u.ClassCode, cfg.u.SecCode) != Trans2Quik.Result.SUCCESS
          || Trans2Quik.START_ORDERS(OrderStatusCallback) != Trans2Quik.Result.SUCCESS
          || Trans2Quik.SUBSCRIBE_TRADES(cfg.u.ClassCode, cfg.u.SecCode) != Trans2Quik.Result.SUCCESS
          || Trans2Quik.START_TRADES(TradeStatusCallback) != Trans2Quik.Result.SUCCESS)
        {
          Connected = Connection.Partial;
          ConnectionText = "Соединение установлено не полностью";
          return;
        }

        Connected = Connection.Full;
        ConnectionText = "";

        tryConnectDone = true;
      }

      connecting.Change(Timeout.Infinite, Timeout.Infinite);
    }

    // **********************************************************************

    public static void Connect()
    {
      if(cfg.u.EnableQuikLog)
        try
        {
          log = new StreamWriter(cfg.LogFile, true, Encoding.UTF8);
          log.WriteLine();
          WriteLog("Connect;" + cfg.FullProgName);
        }
        catch(Exception e)
        {
          MessageBox.Show("Ошибка инициализации файла протокола работы:\n" + e.Message,
            cfg.ProgName, MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
        }
      else
        log = null;

      working = true;
      connecting.Change(0, cfg.QuikTryConnectInterval);
    }

    // **********************************************************************

    public static void Disconnect()
    {
      connecting.Change(Timeout.Infinite, Timeout.Infinite);

      if(Connected != Connection.None)
        Trans2Quik.DISCONNECT(out error, msg, msg.Capacity);

      working = false;

      WriteLog("Disconnect");

      if(log != null)
        lock(log)
          try
          {
            log.Close();
          }
          catch { }
    }

    // **********************************************************************

    static void StatusCallback(Trans2Quik.Result action, int e, string m)
    {
      WriteLog("StatusCallback;" + action + ";;;;e" + e + ";" + m);

      if(tryConnectDone)
      {
        switch(action)
        {
          case Trans2Quik.Result.QUIK_CONNECTED:
            Connected = Connection.Full;
            break;

          case Trans2Quik.Result.QUIK_DISCONNECTED:
            Connected = Connection.Partial;
            break;

          case Trans2Quik.Result.DLL_DISCONNECTED:
            Connected = Connection.None;
            connecting.Change(0, cfg.QuikTryConnectInterval);
            break;

          default: // DLL_CONNECTED
            if(Trans2Quik.IS_QUIK_CONNECTED(out e, msg, msg.Capacity) == Trans2Quik.Result.QUIK_CONNECTED)
              Connected = Connection.Full;
            else
              Connected = Connection.Partial;
            break;
        }

        ConnectionText = m;
      }
    }

    // **********************************************************************
    // *                               Сделки                               *
    // **********************************************************************

    static void TradeStatusCallback(
      int nMode,
      double trade_id,
      double order_id,
      string classCode,
      string secCode,
      double price,
      int quantity,
      double msum,
      int isSell,
      int tradeDescriptor)
    {
      if(nMode == 0)
      {
        if(isSell != 0)
          quantity = -quantity;

        string comment = Marshal.PtrToStringAnsi(Trans2Quik.TRADE_BROKERREF(tradeDescriptor));

        if((comment.EndsWith(cfg.FullProgName) || cfg.u.AcceptAllTrades) && OnNewTrade != null)
          OnNewTrade(Price.Conv(price), quantity);

        WriteLog("TradeStatusCallback;;;o" + order_id + ";q" + quantity
          + ";tt" + trade_id + ";" + comment);
      }
    }

    // **********************************************************************
    // *                               Заявки                               *
    // **********************************************************************

    static void ShowError(int tid, string op, int q, int p, Trans2Quik.Result r, int err, string msg)
    {
      MessageBox.Show("T" + tid + " " + op + " Q=" + q + " P=" + Price.GetString(p)
        + "\n" + (msg.Length == 0 ? r + ", " + err : msg),
        cfg.ProgName + ": транзакция отвергнута",
        MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }

    // **********************************************************************

    static void TransactionReplyCallback(
      Trans2Quik.Result r,
      int err,
      int rc,
      int tid,
      double order_id,
      string msg)
    {
      WriteLog("TransactionReplyCallback;" + r + ";t" + tid + ";o" + order_id + ";rc" + rc + ";;" + msg);

      lock(tlist)
        for(LinkedListNode<Transaction> node = tlist.First; node != null; node = node.Next)
          if(node.Value.tid == tid)
          {
            if(r == Trans2Quik.Result.SUCCESS && rc == 3)
              node.Value.oid = order_id;
            else
            {
              ShowError(tid, node.Value.action.Operation.ToString(),
                node.Value.action.Quantity, node.Value.price, r, err, msg);
              tlist.Remove(node);
            }

            ProcessTList();
            return;
          }
    }

    // **********************************************************************

    static void OrderStatusCallback(
      int nMode,
      int tid,
      double order_id,
      string classCode,
      string secCode,
      double price,
      int balance,
      double msum,
      int isSell,
      int status,
      int orderDescriptor)
    {
      if(nMode == 0)
      {
        int quantity;

        if(isSell == 0)
          quantity = Trans2Quik.ORDER_QTY(orderDescriptor) - balance;
        else
          quantity = balance - Trans2Quik.ORDER_QTY(orderDescriptor);

        WriteLog("OrderStatusCallback;;t" + tid + ";o" + order_id
          + ";q" + quantity + ";s" + status);

        lock(tlist)
          for(LinkedListNode<Transaction> node = tlist.First; node != null; node = node.Next)
            if(node.Value.oid == order_id)
            {
              if(status == 1)
              {
                if(OnOrderUpdate != null)
                  OnOrderUpdate(node.Value.price, true);
              }
              else
              {
                Position += quantity;

                tlist.Remove(node);
                ProcessTList();

                if(OnOrderUpdate != null)
                {
                  foreach(Transaction t in tlist)
                    if(t.price == node.Value.price)
                      return;

                  OnOrderUpdate(node.Value.price, false);
                }
              }

              return;
            }
      }
    }

    // **********************************************************************
    // *                        Управление заявками                         *
    // **********************************************************************

    static int SendOrder(char op, int price, int quantity)
    {
      transId++;

      Trans2Quik.Result r = Trans2Quik.SEND_ASYNC_TRANSACTION(
        "TRANS_ID=" + transId +
        "; ACCOUNT=" + cfg.u.QuikAccount +
        "; CLIENT_CODE=" + cfg.u.QuikClientCode + "/" + cfg.FullProgName +
        "; SECCODE=" + cfg.u.SecCode +
        "; CLASSCODE=" + cfg.u.ClassCode +
        "; ACTION=NEW_ORDER; OPERATION=" + op +
        "; PRICE=" + Price.GetNum(price) +
        "; QUANTITY=" + quantity +
        ";",
        out error, msg, msg.Capacity);

      WriteLog("SendOrder;" + r + ";t" + transId
        + ";;q" + quantity + ";" + op + ";p" + price);

      if(r == Trans2Quik.Result.SUCCESS)
        return transId;
      else
      {
        ShowError(transId, op.ToString(), quantity, price, r, error, msg.ToString());
        return 0;
      }
    }

    // **********************************************************************

    static void KillOrder(double oid)
    {
      transId++;

      Trans2Quik.Result r = Trans2Quik.SEND_ASYNC_TRANSACTION(
        "TRANS_ID=" + transId +
        "; SECCODE=" + cfg.u.SecCode +
        "; CLASSCODE=" + cfg.u.ClassCode +
        "; ACTION=KILL_ORDER; ORDER_KEY=" + oid +
        ";",
        out error, msg, msg.Capacity);

      WriteLog("KillOrder;" + r + ";t" + transId + ";o" + oid);

      if(r != Trans2Quik.Result.SUCCESS)
        ShowError(transId, "Kill", 0, 0, r, error, msg.ToString());
    }

    // **********************************************************************
    // *                     Обработка очереди операций                     *
    // **********************************************************************

    static void CreateBuyOrder(Transaction t, int quantity)
    {
      switch(t.action.Quote)
      {
        case BaseQuote.Absolute:
          t.price = t.action.Value;
          break;

        case BaseQuote.Counter:
          t.price = Price.Floor(ask + t.action.Value);
          break;

        case BaseQuote.Similar:
          t.price = Price.Floor(bid + t.action.Value);
          break;

        default:
          return;
      }

      t.tid = SendOrder('B', t.price, quantity);
    }

    // ----------------------------------------------------------------------

    static void CreateSellOrder(Transaction t, int quantity)
    {
      switch(t.action.Quote)
      {
        case BaseQuote.Absolute:
          t.price = t.action.Value;
          break;

        case BaseQuote.Counter:
          t.price = Price.Ceil(bid - t.action.Value);
          break;

        case BaseQuote.Similar:
          t.price = Price.Ceil(ask - t.action.Value);
          break;

        default:
          return;
      }

      t.tid = SendOrder('S', t.price, quantity);
    }

    // **********************************************************************

    static void ProcessTList()
    {
      LinkedListNode<Transaction> next = tlist.First;
      LinkedListNode<Transaction> curr;

      while(next != null)
      {
        curr = next;
        next = next.Next;

        switch(curr.Value.status)
        {
          // ----------------------------------------------------------------

          case TStatus.Cancel:
            if(curr.Value.tid == 0)
              tlist.Remove(curr);
            else if(curr.Value.oid > 0)
            {
              KillOrder(curr.Value.oid);
              curr.Value.status = TStatus.Canceled;
            }
            break;

          // ----------------------------------------------------------------

          case TStatus.Canceled:
            break;

          // ----------------------------------------------------------------

          case TStatus.Execute:
            switch(curr.Value.action.Operation)
            {
              // -----------------------------

              case TradeOp.Buy:
                if(curr.Value.tid == 0)
                {
                  CreateBuyOrder(curr.Value, curr.Value.action.Quantity);

                  if(curr.Value.tid == 0)
                    tlist.Remove(curr);
                }

                break;

              // -----------------------------

              case TradeOp.Sell:
                if(curr.Value.tid == 0)
                {
                  CreateSellOrder(curr.Value, curr.Value.action.Quantity);

                  if(curr.Value.tid == 0)
                    tlist.Remove(curr);
                }

                break;

              // -----------------------------

              case TradeOp.Upsize:
                if(curr == tlist.First && curr.Value.tid == 0)
                {
                  if(Position > 0)
                    CreateBuyOrder(curr.Value, curr.Value.action.Quantity);
                  else if(Position < 0)
                    CreateSellOrder(curr.Value, curr.Value.action.Quantity);

                  if(curr.Value.tid == 0 || Position == 0)
                  {
                    tlist.Remove(curr);
                    break;
                  }
                }

                next = null;
                break;

              // -----------------------------

              case TradeOp.Downsize:
                if(curr == tlist.First && curr.Value.tid == 0)
                {
                  if(Position > 0)
                    CreateSellOrder(curr.Value, curr.Value.action.Quantity);
                  else if(Position < 0)
                    CreateBuyOrder(curr.Value, curr.Value.action.Quantity);

                  if(curr.Value.tid == 0 || Position == 0)
                  {
                    tlist.Remove(curr);
                    break;
                  }
                }

                next = null;
                break;

              // -----------------------------

              case TradeOp.Close:
                if(curr == tlist.First && curr.Value.tid == 0)
                {
                  if(Position > 0)
                    CreateSellOrder(curr.Value, Position);
                  else if(Position < 0)
                    CreateBuyOrder(curr.Value, -Position);

                  if(curr.Value.tid == 0 || Position == 0)
                  {
                    tlist.Remove(curr);
                    break;
                  }
                }

                next = null;
                break;

              // -----------------------------

              case TradeOp.Reverse:
                if(curr == tlist.First && curr.Value.tid == 0)
                {
                  if(Position > 0)
                    CreateSellOrder(curr.Value, Position * 2);
                  else if(Position < 0)
                    CreateBuyOrder(curr.Value, -Position * 2);

                  if(curr.Value.tid == 0 || Position == 0)
                  {
                    tlist.Remove(curr);
                    break;
                  }
                }

                next = null;
                break;

              // -----------------------------
            }
            break;

          // ----------------------------------------------------------------
        }
      }

      // --------------------------------------------------------------------

      tlistText.Length = 0;

      foreach(Transaction t in tlist)
      {
        tlistText.Append(TradeOpObj.ToString(t.action.Operation));
        switch(t.status)
        {
          case TStatus.Cancel:
            tlistText.Append(" - отмена");
            break;
          case TStatus.Canceled:
            tlistText.Append(" (отменено)");
            break;
        }
        tlistText.AppendLine();
      }

      QueueLength = tlist.Count;
      QueueText = tlistText.ToString();

      tlistUpdated = true;
    }

    // **********************************************************************
    // *                        Пользовательские ф-ции                      *
    // **********************************************************************

    public static void ExecAction(UserAction action)
    {
      if(ask > 0 && bid > 0 && Connected == Connection.Full)
      {
        WriteLog("ExecAction;" + action.Operation
          + ";;;q" + action.Quantity
          + ";v" + action.Value
          + ";" + action.Quote);

        lock(tlist)
        {
          if(action.Operation == TradeOp.Cancel)
          {
            if(action.Quote == BaseQuote.Absolute)
            {
              foreach(Transaction t in tlist)
                if(t.price == action.Value && t.status != TStatus.Canceled)
                  t.status = TStatus.Cancel;
            }
            else
            {
              foreach(Transaction t in tlist)
                if(t.status != TStatus.Canceled)
                  t.status = TStatus.Cancel;
            }
          }
          else
          {
            if(action.Operation == TradeOp.Close || action.Operation == TradeOp.Reverse)
              foreach(Transaction t in tlist)
                if(t.status != TStatus.Canceled)
                  t.status = TStatus.Cancel;

            tlist.AddLast(new Transaction(action));
          }

          ProcessTList();
        }
      }
    }

    // **********************************************************************

    public static void DropState()
    {
      Quik.ask = 0;
      Quik.bid = 0;

      lock(tlist)
      {
        Position = 0;
        tlist.Clear();
        ProcessTList();
      }

      WriteLog("DropState");
    }

    // **********************************************************************

    public static void SetPrice(int ask, int bid)
    {
      Quik.ask = ask;
      Quik.bid = bid;
    }

    // **********************************************************************
  }
}
