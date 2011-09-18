using System.Windows.Forms;

namespace DAFBreakdown.DAFGroup.Control
{
    public partial class SymbolTabControl : TabPage
    {
        public SymbolTabControl(string name)
        {
            Text = name;
            InitializeComponent();
            Controls.Add(SymbolInfo);
        }
    }
}
