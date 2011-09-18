// ======================================================================
//    QScalp.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// ======================================================================

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.Windows.Forms;

using DdeInput;

using QScalp.AboutDialog;
using QScalp.ConfigDialog;
using QScalp.QuikTransaction;
using QScalp.TradeLogDialog;
using QScalp.Visual;

namespace QScalp
{
  class MainForm : Form
  {
    // **********************************************************************
    // *                                Main()                              *
    // **********************************************************************

    [STAThread]
    public static void Main()
    {
      try
      {
        Application.EnableVisualStyles();
        Application.Run(new MainForm());
      }
      catch(Exception e)
      {
        MessageBox.Show("Произошла ошибка в ходе выполнения программы:\n\n" + e.ToString(),
          cfg.ProgName, MessageBoxButtons.OK, MessageBoxIcon.Hand);
      }
    }

    // **********************************************************************
    // *                                Class                               *
    // **********************************************************************

    XltDdeServer server;

    ScalpView sv;
    StatusStrip ss;

    Timer ssUpdater;

    ToolStripStatusLabel tradesStatus;
    ToolStripStatusLabel stockStatus;
    ToolStripStatusLabel quikStatus;
    ToolStripStatusLabel acStatus;
    ToolStripStatusLabel keyStatus;
    ToolStripStatusLabel posStatus;
    ToolStripStatusLabel opqStatus;
    ToolStripStatusLabel resultStatus;

    ToolStripDropDownButton menuButton;

    bool tradesError;
    bool stockError;

    HashSet<Keys> PressedKeys;

    // **********************************************************************
    // *                              MainForm()                            *
    // **********************************************************************

    public MainForm()
    {
      this.StartPosition = FormStartPosition.Manual;

      ComponentResourceManager resources = new ComponentResourceManager(typeof(Properties.Resources));
      this.Icon = (Icon)resources.GetObject("QScalp");

      LoadFormConfig();

      // --------------------------------------------------------------------

      sv = new ScalpView();
      sv.Dock = DockStyle.Fill;
      sv.KeyDown += new KeyEventHandler(sv_KeyDown);
      sv.KeyUp += new KeyEventHandler(sv_KeyUp);
      sv.LostFocus += new EventHandler(sv_LostFocus);
      sv.GotFocus += new EventHandler(sv_GotFocus);
      sv.OnQuoteClick = QuoteClick;
      sv.Parent = this;

      // --------------------------------------------------------------------

      tradesStatus = new ToolStripStatusLabel();
      tradesStatus.AutoSize = false;
      tradesStatus.Margin = new Padding(4, 5, 5, 5);
      tradesStatus.ToolTipText = "DDE канал: \'trades\'";

      stockStatus = new ToolStripStatusLabel();
      stockStatus.AutoSize = false;
      stockStatus.Margin = new Padding(0, 5, 5, 5);
      stockStatus.ToolTipText = "DDE канал: \'stock\'";

      quikStatus = new ToolStripStatusLabel();
      quikStatus.AutoSize = false;
      quikStatus.Margin = new Padding(0, 5, 5, 5);

      acStatus = new ToolStripStatusLabel();
      acStatus.Spring = true;
      acStatus.TextAlign = ContentAlignment.MiddleCenter;
      acStatus.ToolTipText = "Автоцентровка спреда";

      keyStatus = new ToolStripStatusLabel();
      keyStatus.Spring = true;
      keyStatus.TextAlign = ContentAlignment.MiddleCenter;
      keyStatus.ToolTipText = "Индикатор клавиатуры";

      opqStatus = new ToolStripStatusLabel();
      opqStatus.Spring = true;
      opqStatus.TextAlign = ContentAlignment.MiddleCenter;

      posStatus = new ToolStripStatusLabel();
      posStatus.Spring = true;
      posStatus.TextAlign = ContentAlignment.MiddleCenter;
      posStatus.ToolTipText = "Рыночная позиция";

      resultStatus = new ToolStripStatusLabel();
      resultStatus.TextAlign = ContentAlignment.MiddleRight;
      resultStatus.ToolTipText = "Средний результат на 1 лот / Кол-во трейдов / Результат предыдущего трейда";
      resultStatus.AutoSize = false;
      //resultStatus.BorderSides = ToolStripStatusLabelBorderSides.Left;

      // --------------------------------------------------------------------

      menuButton = new ToolStripDropDownButton("меню", null, new ToolStripItem[] {
        new ToolStripMenuItem("Руководство", null, new EventHandler(MenuReadme), cfg.u.KeyHelp),
        new ToolStripMenuItem("О программе", null, new EventHandler(MenuAbout)),
        new ToolStripSeparator(),
        new ToolStripMenuItem("Выгрузить настройки", null, new EventHandler(MenuSaveConf), cfg.u.KeySaveConf),
        new ToolStripMenuItem("Загрузить настройки", null, new EventHandler(MenuLoadConf), cfg.u.KeyLoadConf),
        new ToolStripSeparator(),
        new ToolStripMenuItem("Настройки", null, new EventHandler(MenuSettings), cfg.u.KeyConfig),
        new ToolStripSeparator(),
        new ToolStripMenuItem("Показать/скрыть журнал", null, new EventHandler(MenuTradeLog), cfg.u.KeyTradeLog),
        new ToolStripMenuItem("Очистить позицию", null, new EventHandler(MenuDropPos), cfg.u.KeyDropPos),
        new ToolStripSeparator(),
        new ToolStripMenuItem("Выход", null, new EventHandler(MenuExit)) });

      menuButton.ToolTipText = "Главное меню (" + cfg.u.KeyShowMenu.ToString() + ")";

      // --------------------------------------------------------------------

      ss = new StatusStrip();

      ss.Items.AddRange(new ToolStripItem[] {
        tradesStatus,
        stockStatus,
        quikStatus,
        menuButton,
        acStatus,
        keyStatus,
        opqStatus,
        posStatus,
        resultStatus });

      ss.ShowItemToolTips = true;
      ss.Parent = this;

      resultStatus.Width = VQuote.Width + cfg.u.VPositionWidth - ss.SizeGripBounds.Width - 1;

      // --------------------------------------------------------------------

      tradesStatus.Height = tradesStatus.Width = ss.ClientSize.Height - 10;
      stockStatus.Height = stockStatus.Width = ss.ClientSize.Height - 10;
      quikStatus.Height = quikStatus.Width = ss.ClientSize.Height - 10;

      // --------------------------------------------------------------------

      server = new XltDdeServer(
        cfg.u.DdeServerName,
        new string[]
      {
        cfg.StockTopicName,
        cfg.TradesTopicName
      });

      // --------------------------------------------------------------------

      tradesError = false;
      stockError = false;

      PressedKeys = new HashSet<Keys>();

      ssUpdater = new Timer();
      ssUpdater.Interval = cfg.StatusBarUpdateInterval;
      ssUpdater.Tick += new EventHandler(ssUpdater_Tick);
    }

    // **********************************************************************
    // *                           Инициализация                            *
    // **********************************************************************

    protected override void OnLoad(EventArgs e)
    {
      base.OnLoad(e);

      server.Channels[cfg.StockTopicName].OnUpdate = OnStockUpdate;
      server.Channels[cfg.TradesTopicName].OnUpdate = OnTradesUpdate;
      server.Register();

      Quik.OnNewTrade = sv.UpdatePosition;
      Quik.OnOrderUpdate = sv.UpdateOrder;

      Quik.Connect();

      ssUpdater_Tick(null, null);
      ssUpdater.Start();

      LoadTlfConfig();

      this.Activate();
    }

    // **********************************************************************

    void LoadFormConfig()
    {
      this.MinimumSize = new Size(VClusters.Width + 1 + VQuote.Width + cfg.u.VPositionWidth
        + SystemInformation.FrameBorderSize.Width * (SystemInformation.BorderMultiplierFactor + 1),
        this.DefaultMinimumSize.Height);

      this.Text = cfg.MainFormTitle;
      this.Size = cfg.u.Size;

      cfg.FixFormLocation(this, ref cfg.u.Location);
      this.Location = cfg.u.Location;

      if(this.TopMost != cfg.u.FormTopMost)
        this.TopMost = cfg.u.FormTopMost;

      this.WindowState = cfg.u.WindowState;
    }

    // **********************************************************************

    void SaveFormConfig()
    {
      cfg.u.WindowState = this.WindowState;

      if(cfg.u.WindowState == FormWindowState.Normal)
      {
        cfg.u.Location = this.Location;
        cfg.u.Size = this.Size;
      }
      else
      {
        Rectangle b = this.RestoreBounds;
        cfg.u.Location = b.Location;
        cfg.u.Size = b.Size;
      }
    }

    // **********************************************************************

    void LoadTlfConfig()
    {
      TradeLogForm tlf = null;

      foreach(Form f in this.OwnedForms)
        if(f is TradeLogForm && !f.IsDisposed)
        {
          tlf = (TradeLogForm)f;
          break;
        }

      if(tlf == null)
      {
        if(cfg.u.ShowTradeLog)
        {
          tlf = new TradeLogForm();

          cfg.FixFormLocation(tlf, ref cfg.u.TlfLocation);
          tlf.Location = cfg.u.TlfLocation;

          tlf.Update(sv.TradeLog);
          tlf.Show(this);
        }
      }
      else if(!cfg.u.ShowTradeLog)
        tlf.Close();
    }

    // **********************************************************************

    void SaveTlfConfig()
    {
      foreach(Form f in this.OwnedForms)
        if(f is TradeLogForm && !f.IsDisposed)
        {
          cfg.u.ShowTradeLog = true;
          cfg.u.TlfLocation = f.Location;

          return;
        }

      cfg.u.ShowTradeLog = false;
    }

    // **********************************************************************
    // *                       Обработка клавиатуры                         *
    // **********************************************************************

    void sv_KeyDown(object sender, KeyEventArgs e)
    {
      if(e.KeyCode == cfg.u.KeyCenterSpread)
        sv.CenterSpread();
      else if(e.KeyCode == cfg.u.KeyPageUp)
        sv.Page(1);
      else if(e.KeyCode == cfg.u.KeyPageDown)
        sv.Page(-1);

      // --------------------------------------------------------------------

      else if(e.KeyCode == cfg.u.KeyShowMenu)
      {
        menuButton.ShowDropDown();
        menuButton.Select();
      }

      // --------------------------------------------------------------------

      else if(!PressedKeys.Contains(e.KeyCode))
      {
        PressedKeys.Add(e.KeyCode);

        UserAction a;

        if(!PressedKeys.Contains(cfg.u.KeyBlockKey)
          && cfg.KeyDownActions.TryGetValue(e.KeyCode, out a))
          Quik.ExecAction(a);
      }

      // --------------------------------------------------------------------

      UpdateKeyStatus();
    }

    // **********************************************************************

    void sv_KeyUp(object sender, KeyEventArgs e)
    {
      UserAction a;

      if(!PressedKeys.Contains(cfg.u.KeyBlockKey)
        && cfg.KeyUpActions.TryGetValue(e.KeyCode, out a))
        Quik.ExecAction(a);

      PressedKeys.Remove(e.KeyCode);

      UpdateKeyStatus();
    }

    // **********************************************************************
    // *                         Обработка мышки                            *
    // **********************************************************************

    void QuoteClick(MouseButtons button, int price)
    {
      switch(button)
      {
        case MouseButtons.Left:
          Quik.ExecAction(new UserAction(
            TradeOp.Buy,
            BaseQuote.Absolute,
            price,
            cfg.u.MouseWorkSize));
          break;

        case MouseButtons.Right:
          Quik.ExecAction(new UserAction(
            TradeOp.Sell,
            BaseQuote.Absolute,
            price,
            cfg.u.MouseWorkSize));
          break;

        case MouseButtons.Middle:
          Quik.ExecAction(new UserAction(
            TradeOp.Cancel,
            BaseQuote.Absolute,
            price,
            0));
          break;
      }
    }

    // **********************************************************************
    // *                          Функции меню                              *
    // **********************************************************************

    void MenuReadme(object sender, EventArgs e)
    {
      try
      {
        Process.Start(cfg.ReadmeFile);
      }
      catch
      {
        MessageBox.Show(this, "Ошибка выполнения " + cfg.ReadmeFile,
          cfg.ProgName, MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
      }
    }

    // **********************************************************************

    void MenuAbout(object sender, EventArgs e)
    {
      AboutForm a = new AboutForm();
      a.ShowDialog(this);
      a.Dispose();
    }

    // **********************************************************************

    const string FileDialogsFilter = "Настройки " + cfg.ProgName + " (*.cfg)|*.cfg";

    // ----------------------------------------------------------------------

    void MenuSaveConf(object sender, EventArgs e)
    {
      SaveFileDialog sfd = new SaveFileDialog();

      sfd.Filter = FileDialogsFilter;
      sfd.RestoreDirectory = true;
      sfd.Title = "Выгрузить настройки в файл";
      sfd.FileName = cfg.ProgName + "." + cfg.u.SecCode + "." + cfg.u.ClassCode;

      if(sfd.ShowDialog(this) == DialogResult.OK)
      {
        SaveTlfConfig();
        SaveFormConfig();
        cfg.SaveUserSettings(sfd.FileName);
      }
    }

    // **********************************************************************

    void MenuLoadConf(object sender, EventArgs e)
    {
      OpenFileDialog ofd = new OpenFileDialog();

      ofd.Filter = FileDialogsFilter;
      ofd.RestoreDirectory = true;
      ofd.Title = "Загрузить настройки из файла";

      if(ofd.ShowDialog(this) == DialogResult.OK)
      {
        string sc = cfg.u.SecCode;
        string cc = cfg.u.ClassCode;

        cfg.LoadUserSettings(ofd.FileName);

        if(sc != cfg.u.SecCode || cc != cfg.u.ClassCode)
        {
          Quik.DropState();
          sv.ClearPosition();
        }

        sv.Rebuild();

        LoadTlfConfig();
        LoadFormConfig();

        Quik.Disconnect();
        Quik.Connect();
      }
    }

    // **********************************************************************

    void MenuSettings(object sender, EventArgs e)
    {
      foreach(Form f in this.OwnedForms)
        if(f is ConfigForm && !f.IsDisposed)
        {
          if(f.Visible)
            f.Activate();
          else
            f.Show(this);

          return;
        }

      ConfigForm cf = new ConfigForm();

      cf.ApplyChanges += new EventHandler(ApplyConfigChanges);

      cf.Location = new Point(
        Location.X + Width / 2 - cf.Width / 2,
        Location.Y + Height / 2 - cf.Height / 2);

      cf.Show(this);
    }

    // **********************************************************************

    void MenuTradeLog(object sender, EventArgs e)
    {
      SaveTlfConfig();
      cfg.u.ShowTradeLog = !cfg.u.ShowTradeLog;
      LoadTlfConfig();
    }

    // **********************************************************************

    void MenuDropPos(object sender, EventArgs e)
    {
      if(MessageBox.Show(this, "Сбросить информацию о текущей позиции?", cfg.ProgName,
        MessageBoxButtons.OKCancel, MessageBoxIcon.Question) == DialogResult.OK)
      {
        sv.ClearPosition();
      }
    }

    // **********************************************************************

    void MenuExit(object sender, EventArgs e)
    {
      this.Close();
    }

    // **********************************************************************
    // *                  Обработчик изменения настроек                     *
    // **********************************************************************

    void ApplyConfigChanges(object sender, EventArgs e)
    {
      ConfigForm cf = (ConfigForm)sender;

      SaveFormConfig();
      LoadFormConfig();

      if(cf.ChFlagSecurity)
      {
        Quik.DropState();
        sv.ClearPosition();
      }

      if(cf.FlagRebuild)
        sv.Rebuild();

      if(cf.ChFlagQuikFolder || cf.ChFlagSecurity)
      {
        Quik.Disconnect();
        Quik.Connect();
      }
    }

    // **********************************************************************
    // *                 Обработчики событий от терминала                   *
    // **********************************************************************

    void OnStockUpdate(Object[,] table)
    {
      stockError |= sv.UpdateStockData(table);
      Quik.SetPrice(sv.AskPrice, sv.BidPrice);
    }

    // **********************************************************************

    void OnTradesUpdate(Object[,] table)
    {
      tradesError |= sv.UpdateTradesData(table);
    }

    // **********************************************************************
    // *                              Статусбар                             *
    // **********************************************************************

    void UpdateChannelStatus(ToolStripStatusLabel sl, string topic, ref bool error)
    {
      DdeChannel dc = server.Channels[topic];
      Color color;

      if(dc.Connected)
        if(error)
        {
          color = Color.Red;
          error = false;
        }
        else if(DateTime.Now.Ticks - dc.DataReceived.Ticks < cfg.u.DataTimeout)
          color = Color.LimeGreen;
        else
          color = Color.Orange;
      else
        color = Color.Silver;

      if(sl.BackColor != color)
        sl.BackColor = color;
    }

    // **********************************************************************

    void ssUpdater_Tick(object sender, EventArgs e)
    {
      if(sv.AutoCentering)
      {
        if(acStatus.Text.Length != 2)
          acStatus.Text = "\x2191\x2193";
      }
      else
      {
        if(acStatus.Text.Length != 1)
          acStatus.Text = "-";
      }

      // --------------------------------------------------------------------

      UpdateChannelStatus(stockStatus, cfg.StockTopicName, ref stockError);
      UpdateChannelStatus(tradesStatus, cfg.TradesTopicName, ref tradesError);

      // --------------------------------------------------------------------

      if(Quik.ConnectionUpdated)
        switch(Quik.Connected)
        {
          case Quik.Connection.Full:
            quikStatus.BackColor = Color.LimeGreen;
            quikStatus.ToolTipText = "Соединение с сервером QUIK установлено";
            break;

          case Quik.Connection.Partial:
            quikStatus.BackColor = Color.Orange;
            quikStatus.ToolTipText = Quik.ConnectionText;
            break;

          default:
            quikStatus.BackColor = Color.Red;
            quikStatus.ToolTipText = Quik.ConnectionText;
            break;
        }

      // --------------------------------------------------------------------

      if(Quik.QueueUpdated)
      {
        if(Quik.QueueLength > 0)
        {
          opqStatus.Text = "\x2022 " + Quik.QueueLength + " \x2022";
          opqStatus.ToolTipText = Quik.QueueText;
        }
        else
        {
          opqStatus.Text = "\x00b7 \x00b7 \x00b7";
          opqStatus.ToolTipText = "Очередь операций пуста";
        }
      }

      // --------------------------------------------------------------------

      if(Quik.PositionUpdated)
      {
        if(Quik.Position > 0)
          posStatus.Text = "L " + Quik.Position.ToString();
        else if(Quik.Position < 0)
          posStatus.Text = "S " + (-Quik.Position).ToString();
        else
          posStatus.Text = "\x00d8";
      }

      // --------------------------------------------------------------------

      if(sv.TradeLog.Updated)
      {
        const string sep = "   /   ";

        resultStatus.Text = Price.GetString(sv.TradeLog.AverageResultPerLot)
          + sep + sv.TradeLog.TradesCount
          + sep + Price.GetString(sv.TradeLog.LastResult);

        foreach(Form f in this.OwnedForms)
          if(f is TradeLogForm && !f.IsDisposed)
          {
            ((TradeLogForm)f).Update(sv.TradeLog);
            break;
          }
      }

      // --------------------------------------------------------------------
    }

    // **********************************************************************

    void UpdateKeyStatus()
    {
      if(sv.Focused)
      {
        if(PressedKeys.Contains(cfg.u.KeyBlockKey))
          keyStatus.Text = "\x00d7";
        else if(PressedKeys.Count > 0)
          keyStatus.Text = "\x2117";
        else
          keyStatus.Text = "\x25cb";
      }
      else
        keyStatus.Text = "нет фокуса";
    }

    // **********************************************************************
    // *                                 Фокус                              *
    // **********************************************************************

    void sv_LostFocus(object sender, EventArgs e)
    {
      acStatus.Enabled = false;
      keyStatus.Enabled = false;
      opqStatus.Enabled = false;
      posStatus.Enabled = false;
      resultStatus.Enabled = false;

      UpdateKeyStatus();
      PressedKeys.Clear();
    }

    // **********************************************************************

    void sv_GotFocus(object sender, EventArgs e)
    {
      ssUpdater.Stop();
      ssUpdater_Tick(null, null);
      ssUpdater.Start();

      UpdateKeyStatus();

      acStatus.Enabled = true;
      keyStatus.Enabled = true;
      opqStatus.Enabled = true;
      posStatus.Enabled = true;
      resultStatus.Enabled = true;
    }

    // **********************************************************************
    // *                               Закрытие                             *
    // **********************************************************************

    protected override void OnClosed(EventArgs e)
    {
      SaveTlfConfig();
      SaveFormConfig();
      cfg.SaveUserSettings(cfg.ConfigFile);

      base.OnClosed(e);
    }

    // **********************************************************************

    protected override void Dispose(bool disposing)
    {
      Quik.Disconnect();

      if(server != null)
        server.Dispose();
      if(sv != null)
        sv.Dispose();

      base.Dispose(disposing);
    }

    // **********************************************************************
  }

}
