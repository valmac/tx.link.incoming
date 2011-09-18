// =========================================================================
//    ConfigForm.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// =========================================================================

using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

using QScalp.EnumObjects;

namespace QScalp.ConfigDialog
{
  public partial class ConfigForm : Form
  {
    // **********************************************************************
    // *                     Константы и переменные                         *
    // **********************************************************************

    const string keyDownStr = "нажатии";
    const string keyUpStr = "отпускании";

    // **********************************************************************

    public bool ChFlagQuikFolder { get; protected set; }
    public bool ChFlagSecurity { get; protected set; }
    public bool FlagRebuild { get; protected set; }

    // **********************************************************************

    public event EventHandler ApplyChanges;

    // **********************************************************************
    // *                            Конструктор                             *
    // **********************************************************************

    public ConfigForm()
    {
      InitializeComponent();

      ComponentResourceManager resources = new ComponentResourceManager(typeof(Properties.Resources));
      this.Icon = (Icon)resources.GetObject("QScalp");
      this.Text = "Настройки " + cfg.ProgName;

      // ********************************************************************

      quikFolderBox.Text = cfg.u.QuikFolder;

      accountBox.Text = cfg.u.QuikAccount;
      clientCodeBox.Text = cfg.u.QuikClientCode;
      clientCodeBox.MaxLength = 19 - cfg.FullProgName.Length;

      secCodeBox.Text = cfg.u.SecCode;
      classCodeBox.Text = cfg.u.ClassCode;

      priceStepBox.Set(cfg.u.PriceRatio, cfg.u.PriceStep);
      priceStepBox.ValueChanged += new System.EventHandler(this.priceStepBox_ValueChanged);

      fullVolumeBox.Value = cfg.u.FullVolume;

      grid1Box.Value = (decimal)Price.GetNum(cfg.u.Grid1Step);
      grid2Box.Value = (decimal)Price.GetNum(cfg.u.Grid2Step);

      formTopMost.Checked = cfg.u.FormTopMost;

      // ********************************************************************

      tradeVolume1Box.Value = cfg.u.TradeVolume1;
      tradeVolume2Box.Value = cfg.u.TradeVolume2;
      tradeVolume3Box.Value = cfg.u.TradeVolume3;

      spreadXScaleBox.Value = cfg.u.SpreadXScale;

      guideSecCodeBox.Text = cfg.u.GuideSecCode;
      guideClassCodeBox.Text = cfg.u.GuideClassCode;
      guideStepBox.Set(cfg.u.GuideRatio, cfg.u.GuideStep);
      guideXScaleBox.Value = cfg.u.GuideXScale;
      guideYScaleBox.Value = cfg.u.GuideYScale;

      clustersCountBox.Value = cfg.u.Clusters;
      clusterIntervalBox.Value = cfg.u.ClusterInterval / cfg.TicksPerSecond;
      clusterFillVolume1Box.Value = cfg.u.ClusterFillVolume1;
      clusterFillVolume2Box.Value = cfg.u.ClusterFillVolume2;

      // ********************************************************************

      operationBox.BeginUpdate();
      operationBox.Items.AddRange(new TradeOpObj[] {
        new TradeOpObj(TradeOp.Buy),
        new TradeOpObj(TradeOp.Sell),
        new TradeOpObj(TradeOp.Upsize),
        new TradeOpObj(TradeOp.Downsize),
        new TradeOpObj(TradeOp.Close),
        new TradeOpObj(TradeOp.Reverse),
        new TradeOpObj(TradeOp.Cancel) });
      operationBox.SelectedIndex = 0;
      operationBox.EndUpdate();

      keyTypeBox.BeginUpdate();
      keyTypeBox.Items.AddRange(new string[] { keyDownStr, keyUpStr });
      keyTypeBox.SelectedIndex = 0;
      keyTypeBox.EndUpdate();
      keyTypeBox.SelectedIndexChanged += new EventHandler(ActionKeyEventChanged);

      actionKeyBox.TextChanged += new EventHandler(ActionKeyEventChanged);
      ActionKeyEventChanged(null, null);

      baseQuoteBox.BeginUpdate();
      baseQuoteBox.Items.AddRange(new BaseQuoteObj[] {
        new BaseQuoteObj(BaseQuote.Counter, true),
        new BaseQuoteObj(BaseQuote.Similar, true) });
      baseQuoteBox.SelectedIndex = 0;
      baseQuoteBox.EndUpdate();

      keyBindingsView.Set(cfg.u.PriceRatio, cfg.u.KeyBindings);
      keyBindingsView.SelectedIndexChanged += new EventHandler(keyBindingsViewItemSelected);
      keyBindingsView.Click += new EventHandler(keyBindingsViewItemSelected);

      // ********************************************************************

      mouseWorkSizeBox.Value = cfg.u.MouseWorkSize;

      keyBlockKeyBox.Value = cfg.u.KeyBlockKey;
      keyCenterSpreadBox.Value = cfg.u.KeyCenterSpread;
      keyPageUpBox.Value = cfg.u.KeyPageUp;
      keyPageDownBox.Value = cfg.u.KeyPageDown;

      keyHelpBox.Value = cfg.u.KeyHelp;
      keySaveConfBox.Value = cfg.u.KeySaveConf;
      keyLoadConfBox.Value = cfg.u.KeyLoadConf;
      keyConfigBox.Value = cfg.u.KeyConfig;
      keyTradeLogBox.Value = cfg.u.KeyTradeLog;
      keyDropPosBox.Value = cfg.u.KeyDropPos;
      keyShowMenuBox.Value = cfg.u.KeyShowMenu;

      // ********************************************************************

      ddeServerNameBox.Text = cfg.u.DdeServerName;
      enableQuikLog.Checked = cfg.u.EnableQuikLog;
      acceptAllTrades.Checked = cfg.u.AcceptAllTrades;

      // ********************************************************************

      TuneControls();

      // ********************************************************************

      quikFolderBox.TextChanged += new System.EventHandler(EnableApplyButton);
      accountBox.TextChanged += new System.EventHandler(EnableApplyButton);
      clientCodeBox.TextChanged += new System.EventHandler(EnableApplyButton);
      secCodeBox.TextChanged += new System.EventHandler(EnableApplyButton);
      classCodeBox.TextChanged += new System.EventHandler(EnableApplyButton);
      priceStepBox.ValueChanged += new System.EventHandler(EnableApplyButton);
      fullVolumeBox.ValueChanged += new System.EventHandler(EnableApplyButton);
      grid1Box.ValueChanged += new System.EventHandler(EnableApplyButton);
      grid2Box.ValueChanged += new System.EventHandler(EnableApplyButton);
      formTopMost.CheckedChanged += new System.EventHandler(EnableApplyButton);

      tradeVolume1Box.ValueChanged += new System.EventHandler(EnableApplyButton);
      tradeVolume2Box.ValueChanged += new System.EventHandler(EnableApplyButton);
      tradeVolume3Box.ValueChanged += new System.EventHandler(EnableApplyButton);
      spreadXScaleBox.ValueChanged += new System.EventHandler(EnableApplyButton);
      guideSecCodeBox.TextChanged += new System.EventHandler(EnableApplyButton);
      guideClassCodeBox.TextChanged += new System.EventHandler(EnableApplyButton);
      guideStepBox.ValueChanged += new System.EventHandler(EnableApplyButton);
      guideXScaleBox.ValueChanged += new System.EventHandler(EnableApplyButton);
      guideYScaleBox.ValueChanged += new System.EventHandler(EnableApplyButton);
      clustersCountBox.ValueChanged += new System.EventHandler(EnableApplyButton);
      clusterIntervalBox.ValueChanged += new System.EventHandler(EnableApplyButton);
      clusterFillVolume1Box.ValueChanged += new System.EventHandler(EnableApplyButton);
      clusterFillVolume2Box.ValueChanged += new System.EventHandler(EnableApplyButton);

      mouseWorkSizeBox.ValueChanged += new System.EventHandler(EnableApplyButton);
      keyBlockKeyBox.TextChanged += new System.EventHandler(EnableApplyButton);
      keyCenterSpreadBox.TextChanged += new System.EventHandler(EnableApplyButton);
      keyPageUpBox.TextChanged += new System.EventHandler(EnableApplyButton);
      keyPageDownBox.TextChanged += new System.EventHandler(EnableApplyButton);
      keySaveConfBox.TextChanged += new System.EventHandler(EnableApplyButton);
      keyLoadConfBox.TextChanged += new System.EventHandler(EnableApplyButton);

      enableQuikLog.CheckedChanged += new System.EventHandler(EnableApplyButton);
      acceptAllTrades.CheckedChanged += new System.EventHandler(EnableApplyButton);

      // --------------------------------------------------------------------

      fullVolumeBox.ValueChanged += new System.EventHandler(SetRebuildFlag);
      grid1Box.ValueChanged += new System.EventHandler(SetRebuildFlag);
      grid2Box.ValueChanged += new System.EventHandler(SetRebuildFlag);

      tradeVolume2Box.ValueChanged += new System.EventHandler(SetRebuildFlag);
      tradeVolume3Box.ValueChanged += new System.EventHandler(SetRebuildFlag);
      spreadXScaleBox.ValueChanged += new System.EventHandler(SetRebuildFlag);
      guideStepBox.ValueChanged += new System.EventHandler(SetRebuildFlag);
      guideXScaleBox.ValueChanged += new System.EventHandler(SetRebuildFlag);
      guideYScaleBox.ValueChanged += new System.EventHandler(SetRebuildFlag);
      clustersCountBox.ValueChanged += new System.EventHandler(SetRebuildFlag);
      clusterIntervalBox.ValueChanged += new System.EventHandler(SetRebuildFlag);
      clusterFillVolume1Box.ValueChanged += new System.EventHandler(SetRebuildFlag);
      clusterFillVolume2Box.ValueChanged += new System.EventHandler(SetRebuildFlag);

      // --------------------------------------------------------------------

      quikFolderBox.TextChanged += new System.EventHandler(SetQuikReconectFlag);
      secCodeBox.TextChanged += new System.EventHandler(SetQuikReconectFlag);
      classCodeBox.TextChanged += new System.EventHandler(SetQuikReconectFlag);
      enableQuikLog.CheckedChanged += new System.EventHandler(SetQuikReconectFlag);

      // --------------------------------------------------------------------

      secCodeBox.TextChanged += new EventHandler(SetSecurityFlag);
      classCodeBox.TextChanged += new EventHandler(SetSecurityFlag);

      // --------------------------------------------------------------------

      ddeServerNameBox.TextChanged += new EventHandler(ddeServerNameBox_TextChanged);
    }

    // **********************************************************************
    // *                           TuneControls()                           *
    // **********************************************************************

    void TuneControls()
    {
      grid1Box.DecimalPlaces = priceStepBox.DecimalPlaces;
      grid1Box.Increment = priceStepBox.Value * 10;

      grid2Box.DecimalPlaces = priceStepBox.DecimalPlaces;
      grid2Box.Increment = priceStepBox.Value * 100;

      keyBindingsView.PriceRatio = priceStepBox.Ratio;
      offsetBox.DecimalPlaces = priceStepBox.DecimalPlaces;
      offsetBox.Increment = priceStepBox.Value;
    }


    /////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////
    ////                          Change events                          ////
    /////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////


    // **********************************************************************
    // *                     buttonFolderSelect_Click()                     *
    // **********************************************************************

    private void buttonFolderSelect_Click(object sender, EventArgs e)
    {
      if(quikFolderBox.Text.Length > 0)
        FolderBrowser.SelectedPath = quikFolderBox.Text;

      FolderBrowser.ShowDialog(this);
      quikFolderBox.Text = FolderBrowser.SelectedPath;
    }

    // **********************************************************************
    // *                          EnableApplyButton()                       *
    // **********************************************************************

    private void EnableApplyButton(object sender, EventArgs e)
    {
      buttonApply.Enabled = true;
    }

    // **********************************************************************
    // *                          SetRebuildFlag()                          *
    // **********************************************************************

    private void SetRebuildFlag(object sender, EventArgs e)
    {
      FlagRebuild = true;
    }

    // **********************************************************************
    // *                        SetQuikReconectFlag()                       *
    // **********************************************************************

    private void SetQuikReconectFlag(object sender, EventArgs e)
    {
      ChFlagQuikFolder = true;
    }

    // **********************************************************************
    // *                          SecurityChanged()                         *
    // **********************************************************************

    private void SetSecurityFlag(object sender, EventArgs e)
    {
      ChFlagSecurity = true;
    }

    // **********************************************************************
    // *                    priceStepBox_ValueChanged()                     *
    // **********************************************************************

    private void priceStepBox_ValueChanged(object sender, EventArgs e)
    {
      TuneControls();

      grid1Box.Value = priceStepBox.Value * 10;
      grid2Box.Value = priceStepBox.Value * 100;
    }

    // **********************************************************************
    // *                   tradeVolume*Box_ValueChanged()                   *
    // **********************************************************************

    private void tradeVolume1Box_ValueChanged(object sender, EventArgs e)
    {
      if(tradeVolume2Box.Value < tradeVolume1Box.Value)
        tradeVolume2Box.Value = tradeVolume1Box.Value;
    }

    // **********************************************************************

    private void tradeVolume2Box_ValueChanged(object sender, EventArgs e)
    {
      if(tradeVolume1Box.Value > tradeVolume2Box.Value)
        tradeVolume1Box.Value = tradeVolume2Box.Value;

      if(tradeVolume3Box.Value < tradeVolume2Box.Value)
        tradeVolume3Box.Value = tradeVolume2Box.Value;
    }

    // **********************************************************************

    private void tradeVolume3Box_ValueChanged(object sender, EventArgs e)
    {
      if(tradeVolume2Box.Value > tradeVolume3Box.Value)
        tradeVolume2Box.Value = tradeVolume3Box.Value;
    }

    // **********************************************************************
    // *                      grid*Box_ValueChanged()                       *
    // **********************************************************************

    private void grid1Box_ValueChanged(object sender, EventArgs e)
    {
      if(grid1Box.Value < priceStepBox.Value)
        grid1Box.Value = grid1Box.Increment;

      if(grid2Box.Value < grid1Box.Value)
        grid2Box.Value = grid1Box.Value;
    }

    // **********************************************************************

    private void grid2Box_ValueChanged(object sender, EventArgs e)
    {
      if(grid2Box.Value < priceStepBox.Value)
        grid2Box.Value = grid2Box.Increment;

      if(grid1Box.Value > grid2Box.Value)
        grid1Box.Value = grid2Box.Value;
    }

    // **********************************************************************
    // *                  clusterIntervalBox_ValueChanged()                 *
    // **********************************************************************

    private void clusterIntervalBox_ValueChanged(object sender, EventArgs e)
    {
      if(clusterIntervalBox.Value == 0)
        clusterIntervalBox.Value = clusterIntervalBox.Increment;
    }

    // **********************************************************************
    // *                clusterFillVolume*Box_ValueChanged()                *
    // **********************************************************************

    private void clusterFillVolume1Box_ValueChanged(object sender, EventArgs e)
    {
      if(clusterFillVolume2Box.Value < clusterFillVolume1Box.Value)
        clusterFillVolume2Box.Value = clusterFillVolume1Box.Value;
    }

    // **********************************************************************

    private void clusterFillVolume2Box_ValueChanged(object sender, EventArgs e)
    {
      if(clusterFillVolume1Box.Value > clusterFillVolume2Box.Value)
        clusterFillVolume1Box.Value = clusterFillVolume2Box.Value;
    }

    // **********************************************************************
    // *                   ddeServerNameBox_TextChanged()                   *
    // **********************************************************************

    private void ddeServerNameBox_TextChanged(object sender, EventArgs e)
    {
      labelRestart.ForeColor = Color.Red;
      labelRestart.Enabled = true;
    }

    /////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////
    ////                      keyBindingsView work                       ////
    /////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////

    // **********************************************************************
    // *               keyBindingsView_Changed()               *
    // **********************************************************************

    bool keyBindingsViewItemSelected_Disabled;

    // ----------------------------------------------------------------------

    private void keyBindingsViewItemSelected(object sender, EventArgs e)
    {
      if(keyBindingsView.SelectedBinding == null)
        buttonDeleteKey.Enabled = false;
      else
      {
        buttonDeleteKey.Enabled = true;

        if(!keyBindingsViewItemSelected_Disabled)
        {
          KeyBinding kb = keyBindingsView.SelectedBinding;

          actionKeyBox.Value = kb.Key;
          keyTypeBox.SelectedItem = kb.OnKeyDown ? keyDownStr : keyUpStr;
          operationBox.SelectedItem = new TradeOpObj(kb.Action.Operation);
          baseQuoteBox.SelectedItem = new BaseQuoteObj(kb.Action.Quote, true);
          offsetBox.Value = (decimal)kb.Action.Value / priceStepBox.Ratio;
          quantityBox.Value = kb.Action.Quantity;
        }
      }
    }

    // **********************************************************************
    // *                       buttonUpdateKey_Click()                      *
    // **********************************************************************

    private void buttonUpdateKey_Click(object sender, EventArgs e)
    {
      if(actionKeyBox.Value != Keys.None)
      {
        keyBindingsView.UpdateBinding(
          actionKeyBox.Value,
          (string)keyTypeBox.SelectedItem == keyDownStr,
          ((TradeOpObj)operationBox.SelectedItem).Value,
          ((BaseQuoteObj)baseQuoteBox.SelectedItem).Value,
          Price.Conv((double)offsetBox.Value, priceStepBox.Ratio),
          (int)quantityBox.Value);

        ActionKeyEventChanged(sender, e);
        EnableApplyButton(sender, e);
      }
    }

    // **********************************************************************
    // *                       buttonDeleteKey_Click()                      *
    // **********************************************************************

    private void buttonDeleteKey_Click(object sender, EventArgs e)
    {
      if(keyBindingsView.SelectedItems.Count > 0)
      {
        keyBindingsView.DeleteSelectedBinding();
        ActionKeyEventChanged(sender, e);
        EnableApplyButton(sender, e);
      }
    }

    // **********************************************************************
    // *                  operationBox_SelectedIndexChanged()               *
    // **********************************************************************

    private void operationBox_SelectedIndexChanged(object sender, EventArgs e)
    {
      TradeOp op = ((TradeOpObj)operationBox.SelectedItem).Value;

      bool flag1 = op != TradeOp.Cancel;
      bool flag2 = flag1 && op != TradeOp.Close && op != TradeOp.Reverse;

      baseQuoteBox.Enabled = flag1;
      offsetBox.Enabled = flag1;
      quantityBox.Enabled = flag2;
    }

    // **********************************************************************
    // *                         Key event changed                          *
    // **********************************************************************

    void ActionKeyEventChanged(object sender, EventArgs e)
    {
      keyBindingsViewItemSelected_Disabled = true;

      if(actionKeyBox.Value == Keys.None)
        buttonUpdateKey.Enabled = false;
      else
        buttonUpdateKey.Enabled = true;

      if(keyBindingsView.SelectBinding(actionKeyBox.Value, (string)keyTypeBox.SelectedItem == keyDownStr))
        buttonUpdateKey.Text = "Обновить";
      else
        buttonUpdateKey.Text = "Добавить";

      keyBindingsViewItemSelected_Disabled = false;
    }


    /////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////
    ////                         Global buttons                          ////
    /////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////


    // **********************************************************************
    // *                           buttonOk_Click()                        *
    // **********************************************************************

    private void buttonOk_Click(object sender, EventArgs e)
    {
      this.Hide();
      buttonApply_Click(sender, e);
      this.Close();
    }

    // **********************************************************************
    // *                         buttonCancel_Click()                       *
    // **********************************************************************

    private void buttonCancel_Click(object sender, EventArgs e)
    {
      this.Close();
    }

    // **********************************************************************
    // *                         buttonApply_Click()                        *
    // **********************************************************************

    private void buttonApply_Click(object sender, EventArgs e)
    {
      buttonApply.Enabled = false;

      // ********************************************************************

      cfg.u.QuikFolder = quikFolderBox.Text;

      cfg.u.QuikAccount = accountBox.Text;
      cfg.u.QuikClientCode = clientCodeBox.Text;

      cfg.u.SecCode = secCodeBox.Text;
      cfg.u.ClassCode = classCodeBox.Text;

      cfg.u.PriceRatio = priceStepBox.Ratio;
      cfg.u.PriceStep = priceStepBox.Step;

      cfg.u.FullVolume = (int)fullVolumeBox.Value;

      cfg.u.Grid1Step = Price.Conv((double)grid1Box.Value);
      cfg.u.Grid2Step = Price.Conv((double)grid2Box.Value);

      cfg.u.FormTopMost = formTopMost.Checked;

      // ********************************************************************

      cfg.u.TradeVolume1 = (int)tradeVolume1Box.Value;
      cfg.u.TradeVolume2 = (int)tradeVolume2Box.Value;
      cfg.u.TradeVolume3 = (int)tradeVolume3Box.Value;

      cfg.u.SpreadXScale = (int)spreadXScaleBox.Value;

      cfg.u.GuideSecCode = guideSecCodeBox.Text;
      cfg.u.GuideClassCode = guideClassCodeBox.Text;
      cfg.u.GuideRatio = guideStepBox.Ratio;
      cfg.u.GuideStep = guideStepBox.Step;
      cfg.u.GuideXScale = (int)guideXScaleBox.Value;
      cfg.u.GuideYScale = (int)guideYScaleBox.Value;

      cfg.u.Clusters = (int)clustersCountBox.Value;
      cfg.u.ClusterInterval = (long)clusterIntervalBox.Value * cfg.TicksPerSecond;
      cfg.u.ClusterFillVolume1 = (int)clusterFillVolume1Box.Value;
      cfg.u.ClusterFillVolume2 = (int)clusterFillVolume2Box.Value;

      // ********************************************************************

      cfg.u.KeyBindings = keyBindingsView.Get();

      // ********************************************************************

      cfg.u.MouseWorkSize = (int)mouseWorkSizeBox.Value;

      cfg.u.KeyBlockKey = keyBlockKeyBox.Value;
      cfg.u.KeyCenterSpread = keyCenterSpreadBox.Value;
      cfg.u.KeyPageUp = keyPageUpBox.Value;
      cfg.u.KeyPageDown = keyPageDownBox.Value;

      // ********************************************************************

      cfg.u.DdeServerName = ddeServerNameBox.Text;
      cfg.u.EnableQuikLog = enableQuikLog.Checked;
      cfg.u.AcceptAllTrades = acceptAllTrades.Checked;

      // ********************************************************************

      cfg.Reinit();

      if(ApplyChanges != null)
        ApplyChanges(this, new EventArgs());

      this.Activate();

      // ********************************************************************

      ChFlagQuikFolder = false;
      ChFlagSecurity = false;
      FlagRebuild = false;
    }


    /////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////
  }
}
