using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;

namespace DAFBreakdown.DAFGroup
{
    public partial class LogoForm : Form
    {
        public LogoForm(string InVersion, int SleepSec, EventHandler InClosing, EventHandler InClosed)
        {
            InitializeComponent();
            FormClosing += new FormClosingEventHandler(InClosing);
            FormClosed+=new FormClosedEventHandler(InClosed);

            CloseTimer = new Timer();
            CloseTimer.Tick += new EventHandler(CloseTimer_Tick);
            CloseTimer.Interval = SleepSec * 1000;
            VersionLabel.Text = "Version: " + InVersion;
            Show();
        }

        private void CloseTimer_Tick(object sender, EventArgs e)
        {
            CloseTimer.Enabled = false;
            Close();
        }

        private void LogoForm_Shown(object sender, EventArgs e)
        {
            CloseTimer.Enabled = true;
        }

        private void LogoForm_Load(object sender, EventArgs e)
        {
            GraphicsPath RegionPath = new GraphicsPath();
            Rectangle rect = this.ClientRectangle;
            RegionPath.AddPolygon(new Point[] { new Point(rect.Left, rect.Bottom), new Point(rect.Left, rect.Top + 3), new Point(rect.Left + 3, rect.Top), new Point(rect.Right - 3, rect.Top), new Point(rect.Right, rect.Top + 3), new Point(rect.Right, rect.Bottom) });
            this.Region = new System.Drawing.Region(RegionPath);
        }

        public void UpDateError(string InMessage)
        {
            ErrorLabel.Text = InMessage;
            Refresh();
        }
    }
}
