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
    public partial class IPPortControl : UserControl
    {
        public event EventHandler IPPortSelect;
        public event EventHandler IPPortRemove;
        public delegate void EventHandler(object sender, string IPPort);

        public IPPortControl(System.Windows.Forms.Control InParent, string InValue, EventHandler OnSelect, EventHandler OnRemove)
        {
            IPPortSelect += new EventHandler(OnSelect);
            IPPortRemove += new EventHandler(OnRemove);
             
            InitializeComponent();
            ValueLabel.Text = InValue;
            Dock = DockStyle.Top;
            Parent = InParent;
            AddToParent();
        }

        private void AddToParent()
        {
            if (Parent.InvokeRequired)
                Parent.BeginInvoke(new System.Windows.Forms.MethodInvoker(AddToParent));
            else
                Parent.Controls.Add(this);
        }

        internal string IPPort { get { return ValueLabel.Text; } }

        private void RemoveButton_Click(object sender, EventArgs e)
        {
            ValueLabel.Focus();
            if (IPPortRemove != null)
                IPPortRemove(this, ValueLabel.Text);
        }

        private void ValueLabel_Click(object sender, EventArgs e)
        {
            if (IPPortSelect != null)
                IPPortSelect(this, ValueLabel.Text);
        }
    }
}
