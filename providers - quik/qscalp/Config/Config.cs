// =====================================================================
//    Config.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// =====================================================================

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Windows.Forms;
using System.Xml.Serialization;

namespace QScalp
{
  static class cfg
  {
    // **********************************************************************
    // *                        Program name & strings                      *
    // **********************************************************************

    public const string ProgName = "QScalp";
    public static readonly string FullProgName;

    public static string MainFormTitle { get; private set; }

    public const string SpreadString = "\x2022 \x2022 \x2022";

    // **********************************************************************
    // *                              QUIK & DDE                            *
    // **********************************************************************

    public const int QuikTryConnectInterval = 1000;

    // **********************************************************************

    public const string StockTopicName = "stock";

    public const string AskVolumeCN = "SELL_VOLUME";
    public const string BidVolumeCN = "BUY_VOLUME";
    public const string PriceCN = "PRICE";

    // **********************************************************************

    public const string TradesTopicName = "trades";

    public const string TradeDateCN = "TRADEDATE";
    public const string TradeTimeCN = "TRADETIME";
    public const string TradeSecCN = "SECCODE";
    public const string TradeClassCN = "CLASSCODE";
    public const string TradePriceCN = "PRICE";
    public const string TradeQuantityCN = "QTY";
    public const string TradeOperationCN = "BUYSELL";

    public const string TradeBuyOp = "BUY";
    public const string TradeSellOp = "SELL";

    // **********************************************************************
    // *                        Параметры отрисовщика                       *
    // **********************************************************************

    public const int PaintingInterval = 15;
    public const int StatusBarUpdateInterval = 250;

    // **********************************************************************
    // *                       Параметры вывода текста                      *
    // **********************************************************************

    public static Font Font = new Font("MS Sans Serif", 8);
    public static Font SelectedFont = new Font(Font, FontStyle.Bold);

    public static int StringHeight { get; private set; }
    public const int StringMargin = 2;

    public const int Culture = 0x0419;
    public static CultureInfo ci { get; private set; }

    // **********************************************************************
    // *                                  Цвета                             *
    // **********************************************************************

    public static Color BackColor = Color.White;

    public static Brush ForeBrush = Brushes.Black;
    public static Brush BackBrush = Brushes.White;

    public static Brush AskBackBrush = Brushes.LightPink;
    public static Brush BidBackBrush = Brushes.LightGreen;
    public static Brush BestAskBackBrush = Brushes.Red;
    public static Brush BestBidBackBrush = Brushes.LimeGreen;

    public static Brush LPBackBrush = Brushes.Salmon;
    public static Brush PPBackBrush = Brushes.PaleGreen;
    public static Brush PBackBrush = Brushes.Silver;

    public static Brush ClusterTimeBrush = Brushes.Gainsboro;

    // **********************************************************************

    public static Brush VolumeFillBrush = Brushes.Orange;

    public static Brush ClusterVolumeFillBrush1 = Brushes.Silver;
    public static Brush ClusterVolumeFillBrush2 = Brushes.Red;

    // **********************************************************************

    public static Pen GridLine1Pen = new Pen(Color.Silver, 2);
    public static Pen GridLine2Pen = new Pen(Color.DarkGray, 3);
    public static Pen BlockLinePen = new Pen(Color.Black, 1);
    public static Pen VLinePen = new Pen(Color.Silver, 1);
    public static Pen AskGraphPen = new Pen(BestAskBackBrush, 2);
    public static Pen BidGraphPen = new Pen(BestBidBackBrush, 2);
    public static Pen GuideGraphPen = new Pen(Color.Black, 2);
    public static Pen TradeArcPen = new Pen(Color.Black, 1);
    public static Pen PositionPen = new Pen(Color.Gray, 1);
    public static Pen ClusterPen = new Pen(Color.Gray, 1);

    // **********************************************************************
    // *                          Клавиши операций                          *
    // **********************************************************************

    public static Dictionary<Keys, UserAction> KeyDownActions { get; private set; }
    public static Dictionary<Keys, UserAction> KeyUpActions { get; private set; }

    // **********************************************************************
    // *                     Пользовательские настройки                     *
    // **********************************************************************

    public static UserSettings u { get; private set; }

    // **********************************************************************
    // *                                Прочее                              *
    // **********************************************************************

    public const long TicksPerSecond = 10000000L;

    public static readonly string ConfigFile;
    public static readonly string ReadmeFile;
    public static readonly string LogFile;

    // **********************************************************************
    // *                                Методы                              *
    // **********************************************************************

    static cfg()
    {
      Version ver = Assembly.GetExecutingAssembly().GetName().Version;
      FullProgName = ProgName + " " + ver.Major.ToString() + "." + ver.Minor.ToString();

      // --------------------------------------------------------------------

      string fp = Assembly.GetExecutingAssembly().Location;
      fp = fp.Remove(fp.LastIndexOf('.') + 1);

      ConfigFile = fp + "cfg";
      LogFile = fp + "QuikLog.csv";
      ReadmeFile = fp.Remove(fp.LastIndexOf('\\') + 1) + "readme.doc";

      // --------------------------------------------------------------------

      KeyDownActions = new Dictionary<Keys, UserAction>();
      KeyUpActions = new Dictionary<Keys, UserAction>();

      ci = new CultureInfo(Culture, true);

      // --------------------------------------------------------------------

      Bitmap b = new Bitmap(1, 1);
      Graphics g = Graphics.FromImage(b);
      StringHeight = g.MeasureString(SpreadString, Font).ToSize().Height;
      g.Dispose();

      // --------------------------------------------------------------------

      u = new UserSettings();
      LoadUserSettings(cfg.ConfigFile);
    }

    // **********************************************************************

    public static void Reinit()
    {
      MainFormTitle = u.SecCode.Length > 0 ? u.SecCode + " - " + FullProgName : FullProgName;

      ci.NumberFormat.NumberDecimalDigits = (int)Math.Log10(u.PriceRatio);

      KeyDownActions.Clear();
      KeyUpActions.Clear();

      foreach(KeyBinding kb in u.KeyBindings)
        if(kb.OnKeyDown)
          KeyDownActions.Add(kb.Key, kb.Action);
        else
          KeyUpActions.Add(kb.Key, kb.Action);
    }

    // **********************************************************************

    public static void SaveUserSettings(string fn)
    {
      try
      {
        using(Stream fs = new FileStream(fn, FileMode.Create, FileAccess.Write, FileShare.Read))
        {
          XmlSerializer xs = new XmlSerializer(typeof(UserSettings));
          xs.Serialize(fs, u);
        }
      }
      catch(Exception e)
      {
        MessageBox.Show("Ошибка сохранения конфигурационного файла:\n" + e.Message,
          ProgName, MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
      }
    }

    // **********************************************************************

    public static void LoadUserSettings(string fn)
    {
      try
      {
        using(Stream fs = File.OpenRead(fn))
        {
          XmlSerializer xs = new XmlSerializer(typeof(UserSettings));
          u = (UserSettings)xs.Deserialize(fs);
        }
      }
      catch(Exception e)
      {
        MessageBox.Show("Ошибка загрузки конфигурационного файла:\n"
          + e.Message + "\nИспользованы исходные настройки.",
          ProgName, MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
      }

      Reinit();
    }

    // **********************************************************************

    public static void FixFormLocation(Form form, ref Point location)
    {
      Rectangle wa = Screen.GetWorkingArea(form);

      if(location.X + form.Width <= wa.X)
        location.X = wa.X;

      if(location.Y + form.Height <= wa.Y)
        location.Y = wa.Y;

      if(location.X >= wa.Width)
        location.X = wa.Width - form.Width;

      if(location.Y >= wa.Height)
        location.Y = wa.Height - form.Height;
    }

    // **********************************************************************
  }
}
