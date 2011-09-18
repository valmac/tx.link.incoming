// =========================================================================
//   UserSettings.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// =========================================================================

using System.Drawing;
using System.Windows.Forms;

namespace QScalp
{
  public class UserSettings
  {
    // **********************************************************************
    // *                              QUIK & DDE                            *
    // **********************************************************************

    public string QuikFolder = @"C:\Program Files\QUIK";
    public bool EnableQuikLog = false;
    public bool AcceptAllTrades = false;

    public string DdeServerName = cfg.ProgName;
    public long DataTimeout = 2L * cfg.TicksPerSecond;

    // **********************************************************************
    // *                                 Счет                               *
    // **********************************************************************

    public string QuikAccount = "SPBFUTxxxxx";
    public string QuikClientCode = "";

    // **********************************************************************
    // *                              Инструмент                            *
    // **********************************************************************

    public string SecCode = "RIM1";
    public string ClassCode = "SPBFUT";

    public int PriceRatio = 1;

    public int PriceStep = 5;
    public int Grid1Step = 100;
    public int Grid2Step = 500;

    public int FullVolume = 200;

    // **********************************************************************
    // *                                 Размеры                            *
    // **********************************************************************

    public int VQuoteVolumeWidth = 70;
    public int VQuotePriceWidth = 47;
    public int VQuoteHeight = 15;

    public int VPositionWidth = 40;

    // **********************************************************************
    // *                     Параметры графиков и сделок                    *
    // **********************************************************************

    public int MinGrpahTicks = 10;

    public int SpreadXScale = 5;
    public int SpreadsTickInterval = 500;

    public int TradeBallWidth = 7;
    public int TradesTickInterval = 750;

    public int TradeVolume1 = 1;
    public int TradeVolume2 = 10;
    public int TradeVolume3 = 20;

    // **********************************************************************
    // *                               Поводырь                             *
    // **********************************************************************

    public string GuideSecCode = "MICEXINDEXCF";
    public string GuideClassCode = "INDX";

    public int GuideRatio = 100;

    public int GuideStep = 1;

    public int GuideXScale = 4;
    public int GuideYScale = 3;

    public int GuideCenteringStart = 5;
    public int GuideCenteringMin = 3;
    public int GuideCenteringDiv = 9;
    public int GuideCenteringShift = 25;

    // **********************************************************************
    // *                                 Кластеры                           *
    // **********************************************************************

    public int Clusters = 3;
    public long ClusterInterval = 600L * cfg.TicksPerSecond;

    public int ClusterFillVolume1 = 1000;
    public int ClusterFillVolume2 = 1500;

    public int ClusterWidth = 44;

    // **********************************************************************
    // *                            Автоцентровка                           *
    // **********************************************************************

    public int CenteringStart = 20;
    public int CenteringMin = 9;
    public int CenteringDiv = 6;

    // **********************************************************************
    // *                              Управление                            *
    // **********************************************************************

    public KeyBinding[] KeyBindings = new KeyBinding[] {
      new KeyBinding(Keys.Up, true, TradeOp.Buy, BaseQuote.Similar, 5, 1),
      new KeyBinding(Keys.Up, false, TradeOp.Close, BaseQuote.Counter, 500, 0),
      new KeyBinding(Keys.Down, true, TradeOp.Sell, BaseQuote.Similar, 5, 1),
      new KeyBinding(Keys.Down, false, TradeOp.Close, BaseQuote.Counter, 500, 0),
      new KeyBinding(Keys.Right, true, TradeOp.Upsize, BaseQuote.Similar, 5, 1),
      new KeyBinding(Keys.Left, true, TradeOp.Downsize, BaseQuote.Similar, 5, 1),
      new KeyBinding(Keys.Delete, true, TradeOp.Close, BaseQuote.Counter, 500, 0),
      new KeyBinding(Keys.Tab, true, TradeOp.Reverse, BaseQuote.Counter, 500, 0),
      new KeyBinding(Keys.Escape, true, TradeOp.Cancel, BaseQuote.None, 0, 0) };

    // **********************************************************************

    public int MouseWorkSize = 0;

    // **********************************************************************

    public Keys KeyBlockKey = Keys.ShiftKey;

    public Keys KeyCenterSpread = Keys.Enter;
    public Keys KeyPageUp = Keys.PageUp;
    public Keys KeyPageDown = Keys.PageDown;

    public Keys KeyHelp = Keys.F1;
    public Keys KeySaveConf = Keys.F2;
    public Keys KeyLoadConf = Keys.F3;
    public Keys KeyConfig = Keys.F4;
    public Keys KeyTradeLog = Keys.F5;
    public Keys KeyDropPos = Keys.F8;
    public Keys KeyShowMenu = Keys.F9;

    // **********************************************************************

    public int ManualScrollSize = 25;

    // **********************************************************************
    // *                         Системные параметры                        *
    // **********************************************************************

    public bool FormTopMost = false;

    public FormWindowState WindowState = FormWindowState.Normal;
    public Point Location = new Point(50, 50);
    public Size Size = new Size(600, 400);

    public bool ShowTradeLog = false;
    public Point TlfLocation = new Point(650, 50);

    // **********************************************************************
  }

}
