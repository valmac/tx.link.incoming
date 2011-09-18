// ====================================================================
//    About.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// ====================================================================

using System.Diagnostics;
using System.Windows.Forms;

namespace QScalp.AboutDialog
{
  public partial class AboutForm : Form
  {
    public AboutForm()
    {
      InitializeComponent();
      progName.Text = cfg.FullProgName;
    }

    private void siteLink_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
    {
      try
      {
        Process.Start("http://www.moroshkin.com");
      }
      catch
      {
        MessageBox.Show(this,
          "Ошибка открытия ссылки",
          cfg.ProgName,
          MessageBoxButtons.OK,
          MessageBoxIcon.Exclamation);
      }
    }
  }
}
