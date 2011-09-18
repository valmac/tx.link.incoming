// =======================================================================
//    Controls.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// =======================================================================

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;
using QScalp.EnumObjects;

namespace QScalp.Controls
{
  // ************************************************************************
  // *                                 KeyBox                               *
  // ************************************************************************

  class KeyBox : TextBox
  {
    // ----------------------------------------------------------------------

    static LinkedList<KeyBox> kblist;
    static KeyBox() { kblist = new LinkedList<KeyBox>(); }

    // ----------------------------------------------------------------------

    Keys value;
    int conflicts;

    // ----------------------------------------------------------------------

    public KeyBox()
      : base()
    {
      this.ShortcutsEnabled = false;
      kblist.AddLast(this);
    }

    // ----------------------------------------------------------------------

    protected override void Dispose(bool disposing)
    {
      kblist.Remove(this);
      base.Dispose(disposing);
    }

    // ----------------------------------------------------------------------

    public void Conflicted(Keys key1, Keys key2, out bool was, out bool now)
    {
      if(key1 == this.value && key1 != Keys.None)
      {
        conflicts--;
        was = true;
      }
      else
        was = false;

      if(key2 == this.value && key2 != Keys.None)
      {
        conflicts++;
        now = true;
      }
      else
        now = false;

      if(conflicts == 0)
        this.BackColor = SystemColors.Window;
      else
        this.BackColor = Color.Red;
    }

    // ----------------------------------------------------------------------

    public Keys Value
    {
      get { return value; }
      set
      {
        foreach(KeyBox kb in kblist)
        {
          if(kb != this)
          {
            bool was, now;
            kb.Conflicted(this.value, value, out was, out now);
            if(was)
              conflicts--;
            if(now)
              conflicts++;
          }
        }

        if(conflicts == 0)
          this.BackColor = SystemColors.Window;
        else
          this.BackColor = Color.Red;

        this.value = value;

        if(value == Keys.None)
          Text = "";
        else
          Text = value.ToString();

        Select(Text.Length, 0);
      }
    }

    // ----------------------------------------------------------------------

    protected override bool ProcessCmdKey(ref Message msg, Keys keyData)
    {
      //base.ProcessCmdKey(ref msg, keyData);

      Keys key = keyData & Keys.KeyCode;

      if(Value != Keys.None && key == Keys.Back)
        Value = Keys.None;
      else
        Value = key;

      return true;
    }

    // ----------------------------------------------------------------------
  }


  // ************************************************************************
  // *                              NumStepBox                              *
  // ************************************************************************

  class NumStepBox : NumericUpDown
  {
    public override void DownButton()
    {
      if(Value <= Increment)
        Increment /= 10;

      base.DownButton();
    }

    protected override void OnValueChanged(System.EventArgs e)
    {
      int precision = (int)Math.Ceiling(-Math.Log10((double)Value));

      if(precision < 0)
        precision = 0;

      DecimalPlaces = precision;
      Increment = (decimal)(1 / Math.Pow(10, precision));
      Value = Math.Round(Value, precision);

      base.OnValueChanged(e);
    }

    public int Ratio { get { return (int)Math.Pow(10, DecimalPlaces); } }
    public int Step { get { return (int)(Value * Ratio); } }
    public void Set(int ratio, int step) { Value = (decimal)step / ratio; }
  }


  // ************************************************************************
  // *                            KeyBindingsView                           *
  // ************************************************************************

  class KeyBindingsView : ListView
  {
    // ----------------------------------------------------------------------

    string[] Headers = new string[] {
      "Клавиша",
      "Событие",
      "Операция",
      "Котировка",
      "Отступ",
      "Объем",
      "" };

    // ----------------------------------------------------------------------

    const int KeyTypeIndex = 1;
    const int OperationIndex = 2;
    const int QuoteIndex = 3;
    const int OffsetIndex = 4;
    const int QuantityIndex = 5;

    // ----------------------------------------------------------------------

    public KeyBinding SelectedBinding { get; protected set; }

    // ----------------------------------------------------------------------

    int priceRatio;

    // ----------------------------------------------------------------------

    public KeyBindingsView()
      : base()
    {
      ColumnHeader[] chs = new ColumnHeader[Headers.Length];

      for(int i = 0; i < Headers.Length; i++)
      {
        chs[i] = new ColumnHeader();
        chs[i].Text = Headers[i];
      }

      this.Columns.AddRange(chs);
      this.Columns[chs.Length - 2].TextAlign = HorizontalAlignment.Right;
      this.Columns[chs.Length - 3].TextAlign = HorizontalAlignment.Right;

      priceRatio = 1;
      SelectedBinding = null;
    }

    // ----------------------------------------------------------------------

    void SetItem(ListViewItem item, KeyBinding kb)
    {
      string quote = "";
      string qty = "";
      string offset = "";

      if(kb.Action.Operation != TradeOp.Cancel)
      {
        quote = BaseQuoteObj.ToString(kb.Action.Quote);
        offset = Price.GetString(kb.Action.Value, priceRatio);

        if(kb.Action.Operation != TradeOp.Close && kb.Action.Operation != TradeOp.Reverse)
          qty = kb.Action.Quantity.ToString();
      }

      item.SubItems.Clear();

      item.Text = kb.Key.ToString();
      item.SubItems.AddRange(new string[] {
        kb.OnKeyDown ? "нажатие" : "отпускание",
        TradeOpObj.ToString(kb.Action.Operation),
        quote,
        offset,
        qty });

      item.Tag = kb.Key;
      item.SubItems[KeyTypeIndex].Tag = kb.OnKeyDown;
      item.SubItems[OperationIndex].Tag = kb.Action.Operation;
      item.SubItems[QuoteIndex].Tag = kb.Action.Quote;
      item.SubItems[OffsetIndex].Tag = kb.Action.Value;
      item.SubItems[QuantityIndex].Tag = kb.Action.Quantity;
    }

    // ----------------------------------------------------------------------

    KeyBinding GetBinding(ListViewItem item)
    {
      return new KeyBinding(
        (Keys)item.Tag,
        (bool)item.SubItems[KeyTypeIndex].Tag,
        (TradeOp)item.SubItems[OperationIndex].Tag,
        (BaseQuote)item.SubItems[QuoteIndex].Tag,
        (int)item.SubItems[OffsetIndex].Tag,
        (int)item.SubItems[QuantityIndex].Tag);
    }

    // ----------------------------------------------------------------------

    protected override void OnSelectedIndexChanged(EventArgs e)
    {
      if(SelectedItems.Count > 0)
        SelectedBinding = GetBinding(SelectedItems[0]);
      else
        SelectedBinding = null;

      base.OnSelectedIndexChanged(e);
    }

    // ----------------------------------------------------------------------

    public int PriceRatio
    {
      set
      {
        if(priceRatio != value)
        {
          priceRatio = value;
          foreach(ListViewItem item in Items)
            if((TradeOp)item.SubItems[OperationIndex].Tag != TradeOp.Cancel)
              item.SubItems[OffsetIndex].Text = Price.GetString((int)item.SubItems[OffsetIndex].Tag, priceRatio);
        }
      }
    }

    // ----------------------------------------------------------------------

    public void Set(int priceRatio, KeyBinding[] keyBindings)
    {
      this.priceRatio = priceRatio;

      this.BeginUpdate();
      foreach(KeyBinding kb in keyBindings)
      {
        ListViewItem item = new ListViewItem();
        SetItem(item, kb);
        this.Items.Add(item);
      }

      this.AutoResizeColumns(ColumnHeaderAutoResizeStyle.HeaderSize);
      this.Columns[Headers.Length - 1].Width = 0;
      this.EndUpdate();
    }

    // ----------------------------------------------------------------------

    public KeyBinding[] Get()
    {
      KeyBinding[] keyBindings = new KeyBinding[Items.Count];

      for(int i = 0; i < Items.Count; i++)
        keyBindings[i] = GetBinding(Items[i]);

      return keyBindings;
    }

    // ----------------------------------------------------------------------

    public void UpdateBinding(Keys key, bool onKeyDown, TradeOp op, BaseQuote quote, int offset, int quantity)
    {
      if(op == TradeOp.Close || op == TradeOp.Reverse)
        quantity = 0;

      if(op == TradeOp.Cancel)
      {
        quote = BaseQuote.None;
        offset = 0;
        quantity = 0;
      }

      KeyBinding kb = new KeyBinding(key, onKeyDown, op, quote, offset, quantity);

      foreach(ListViewItem item in Items)
        if((Keys)item.Tag == key && (bool)item.SubItems[KeyTypeIndex].Tag == onKeyDown)
        {
          SetItem(item, kb);
          item.Selected = true;
          EnsureVisible(item.Index);
          return;
        }

      ListViewItem newitem = new ListViewItem();
      SetItem(newitem, kb);
      newitem.Selected = true;
      this.Items.Add(newitem);
    }

    // ----------------------------------------------------------------------

    public bool SelectBinding(Keys key, bool onKeyDown)
    {
      foreach(ListViewItem item in Items)
        if((Keys)item.Tag == key && (bool)item.SubItems[KeyTypeIndex].Tag == onKeyDown)
        {
          item.Selected = true;
          EnsureVisible(item.Index);

          return true;
        }
        else
          item.Selected = false;

      return false;
    }

    // ----------------------------------------------------------------------

    public void DeleteSelectedBinding()
    {
      if(SelectedIndices.Count > 0)
        Items.RemoveAt(SelectedIndices[0]);
    }

    // ----------------------------------------------------------------------
  }

  // ************************************************************************
}
