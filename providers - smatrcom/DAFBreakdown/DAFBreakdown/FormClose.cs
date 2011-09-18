using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Threading;

namespace DAFBreakdown
{
    public partial class FormClose : Form
    {
        private bool bClose = false;
        private bool Destroyed = false;

        public FormClose()
        {
            InitializeComponent();
            CancelButton = CloseButton;
            CloseButton.DialogResult = DialogResult.Cancel;
        }

        public void OnDestroyed()
        {
            Destroyed = true;
            CancelButton = null;
            CloseButton.DialogResult = DialogResult.OK;
            AcceptButton = CloseButton;
        }

        private void CloseButton_Click(object sender, EventArgs e)
        {
            InfoLabel.Focus();
            bClose = true;
        }

        private void CloseStart()
        {
            Thread.Sleep(500);
            while (!Destroyed && !bClose)
                Thread.Sleep(500);
            if (Destroyed)
                CloseProccess();
        }

        private void CloseProccess()
        {
            if (CloseButton.InvokeRequired)
                CloseButton.BeginInvoke(new System.Windows.Forms.MethodInvoker(CloseProccess));
            else
                CloseButton.PerformClick();
        }

        private void CloseForm_Shown(object sender, EventArgs e)
        {
            new Thread(CloseStart).Start();
        }
    }
}
