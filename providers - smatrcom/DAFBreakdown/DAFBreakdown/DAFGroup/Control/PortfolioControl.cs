using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace DAFBreakdown.DAFGroup.Control
{
    public partial class PortfolioControl : UserControl
    {
        public PortfolioControl()
        {
            InitializeComponent();
        }

        private void SelectIsEmply()
        {
            if (SelectComboBox.InvokeRequired)
                SelectComboBox.BeginInvoke(new System.Windows.Forms.MethodInvoker(SelectIsEmply));
            else
            {
                if (SelectComboBox.Items.Count > 0 && SelectComboBox.Text == "")
                    SelectComboBox.SelectedIndex = 0;
            }
        }

        public void Add(PortfolioInfo portfolio)
        {
            if (Find(portfolio.Name) == -1)
            {
                SelectComboBox.Items.Add(portfolio);
                SelectIsEmply();
            }
        }

        public void UpDate(string name, double cash, double fee)
        {
            int iIndex = Find(name);
            if (iIndex > -1)
                ((PortfolioInfo)SelectComboBox.Items[iIndex]).UpDate(name, cash, fee);
            ReDraw();
        }

        private int Find(string name)
        {
            int iReturn = -1;
            for (int iIndex = 0; iIndex < SelectComboBox.Items.Count; iIndex++)
                if (((PortfolioInfo)SelectComboBox.Items[iIndex]).Name == name)
                    {
                        iReturn = iIndex;
                        break;
                    }
            return iReturn;
        }

        private void ReDraw()
        {
            if (this.InvokeRequired)
                this.BeginInvoke(new System.Windows.Forms.MethodInvoker(ReDraw));
            else
            {
                CashLabel.Text = (SelectComboBox.SelectedIndex == -1 ? "" : ((PortfolioInfo)SelectComboBox.Items[SelectComboBox.SelectedIndex]).Cash.ToString("### ##0.00;"));
                FeeLabel.Text = (SelectComboBox.SelectedIndex == -1 ? "" : ((PortfolioInfo)SelectComboBox.Items[SelectComboBox.SelectedIndex]).Fee.ToString("### ##0.00;"));
            }
        }

        private void SelectComboBox_SelectedValueChanged(object sender, EventArgs e)
        {
            ReDraw();
        }
    }    
}
