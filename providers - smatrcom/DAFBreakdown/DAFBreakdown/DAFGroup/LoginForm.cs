using System;
using System.Windows.Forms;

namespace DAFBreakdown.DAFGroup
{
    public partial class LoginForm : Form
    {
        ////////////////
        // IT Invest
        // "82.204.220.34:8090";
        // "213.247.232.238:8090";
        ////////////////
        public DAFGroup.SmartAccount GetAccount 
        { 
            get 
            {
                DAFGroup.SmartAccount Account = new DAFGroup.SmartAccount();
                Account.Login = LoginTextBox.Text;
                Account.Password = PasswordTextBox.Text;
                Account.IPPorts.Clear();
                foreach (DAFGroup.Control.IPPortControl tempIPPort in IPPortsPanel.Controls)
                    Account.IPPorts.Add(tempIPPort.IPPort);
                return Account; 
            } 
        }

        public LoginForm()
        {
            this.Icon = null;
            InitializeComponent();
            OkButton.DialogResult = DialogResult.OK;
            CancelButton.DialogResult = DialogResult.Cancel;
            new DAFGroup.Control.IPPortControl(IPPortsPanel, "82.204.220.34:8090", IPPorts_Select, IPPorts_Remove);
            new DAFGroup.Control.IPPortControl(IPPortsPanel, "213.247.232.238:8090", IPPorts_Select, IPPorts_Remove);
        }

        private void OkButton_Click(object sender, EventArgs e)
        {
            TitleLabel.Focus();
        }

        private void CanselButton_Click(object sender, EventArgs e)
        {
            TitleLabel.Focus();
        }

        private void AddButton_Click(object sender, EventArgs e)
        {
            TitleLabel.Focus();
            new DAFGroup.Control.IPPortControl(IPPortsPanel, IPPortTextBox.Text, IPPorts_Select, IPPorts_Remove);
        }

        private void IPPorts_Select(object sender, string IPPort)
        {
            IPPortTextBox.Text = IPPort;
        }

        private void IPPorts_Remove(object sender, string IPPort)
        {
            IPPortTextBox.Text = IPPort;
            IPPortsPanel.Controls.Remove((DAFGroup.Control.IPPortControl)sender);
        }
    }
}
