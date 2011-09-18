// =======================================================================
//  TradeLogForm.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// =======================================================================

using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace QScalp.TradeLogDialog
{
  public partial class TradeLogForm : Form
  {
    // **********************************************************************

    protected override bool ShowWithoutActivation { get { return true; } }

    // **********************************************************************

    public TradeLogForm()
    {
      InitializeComponent();

      ComponentResourceManager resources = new ComponentResourceManager(typeof(Properties.Resources));
      this.Icon = (Icon)resources.GetObject("QScalp");
    }

    // **********************************************************************

    public void Update(TradeLog tl)
    {
      if(tl.Cleared)
        logDataBox.Clear();

      logDataBox.AppendText(tl.GetText(logDataBox.Text.Length));

      labelTradesCount.Text = tl.TradesCount.ToString();
      labelTurnover.Text = tl.Turnover.ToString();

      labelOverallPerLot.Text = Price.GetString(tl.OverallResultPerLot);
      labelAveragePerLot.Text = Price.GetString(tl.AverageResultPerLot);

      labelOverall.Text = Price.GetString(tl.OverallResult);
      labelAverage.Text = Price.GetString(tl.AverageResult);
    }

    // **********************************************************************
  }
}
