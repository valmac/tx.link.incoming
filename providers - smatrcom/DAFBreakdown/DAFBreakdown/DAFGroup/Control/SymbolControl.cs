using System;
using System.Drawing;
using System.Windows.Forms;

namespace DAFBreakdown.DAFGroup.Control
{
    public partial class SymbolControl : UserControl
    {
        private Mode WorkMode;

        public SymbolControl()
        {
            InitializeComponent();
            WorkMode = Mode.Active;
            StatusButton.PerformClick();
            TransactionsGridView.ColumnHeadersDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter;
            TransactionsGridView.ColumnHeadersDefaultCellStyle.BackColor = Color.LightGray;
            TransactionsGridView.RowsDefaultCellStyle.ForeColor = Color.Black;
            TransactionsGridView.RowsDefaultCellStyle.BackColor = Color.FromArgb(185, 185, 185);
            TransactionsGridView.RowsDefaultCellStyle.SelectionBackColor = Color.DimGray;
            TransactionsGridView.RowsDefaultCellStyle.SelectionForeColor = Color.White;
            TransactionsGridView.ColumnHeadersDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter;
            TransactionsGridView.SelectionMode = DataGridViewSelectionMode.FullRowSelect;
            TransactionsGridView.RowHeadersVisible = false;
            TransactionsGridView.Columns.Add("Enter", "Вход");
            TransactionsGridView.Columns[0].DefaultCellStyle.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter;
            TransactionsGridView.Columns[0].Width = 70;
            TransactionsGridView.Columns[0].Resizable = System.Windows.Forms.DataGridViewTriState.False;
            TransactionsGridView.Columns.Add("Exit", "Выход");
            TransactionsGridView.Columns[1].DefaultCellStyle.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter;
            TransactionsGridView.Columns[1].Width = 70;
            TransactionsGridView.Columns[1].Resizable = System.Windows.Forms.DataGridViewTriState.False;
            TransactionsGridView.Columns.Add("Volume", "Объём");
            TransactionsGridView.Columns[2].DefaultCellStyle.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter;
            TransactionsGridView.Columns[2].Width = 70;
            TransactionsGridView.Columns[2].Resizable = System.Windows.Forms.DataGridViewTriState.False;
            TransactionsGridView.Columns.Add("Profit", "Прибыль");
            TransactionsGridView.Columns[3].DefaultCellStyle.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter;
            TransactionsGridView.Columns[3].Width = 70;
            TransactionsGridView.Columns[3].Resizable = System.Windows.Forms.DataGridViewTriState.False;
            TransactionsGridView.Columns.Add("MaxLoss", "Просадка");
            TransactionsGridView.Columns[4].DefaultCellStyle.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter;
            TransactionsGridView.Columns[4].Width = 70;
            TransactionsGridView.Columns[4].Resizable = System.Windows.Forms.DataGridViewTriState.False;
            TransactionsGridView.Rows.Clear();
        }

        private void StatusButton_Click(object sender, EventArgs e)
        {
            CommandPanel.Focus();
            switch (WorkMode)
            {
                case Mode.Active:
                    WorkMode = Mode.Pause;
                    StatusButton.Text = "Пауза";
                    StatusButton.BackColor = Color.Yellow;
                    StatusButton.ForeColor = Color.Red;
                    break;
                case Mode.Pause:
                    WorkMode = Mode.Stop;
                    StatusButton.Text = "Стоп";
                    StatusButton.BackColor = Color.RoyalBlue;
                    StatusButton.ForeColor = Color.Silver;
                    break;
                case Mode.Stop:
                    WorkMode = Mode.Active;
                    StatusButton.Text = "Активный";
                    StatusButton.BackColor = Color.Green;
                    StatusButton.ForeColor = Color.Yellow;
                    break;
            }
            
            if (WorkMode == Mode.Pause)
                RemoveButton.Enabled = true;
            else
                RemoveButton.Enabled = false;
        }

        private void RemoveButton_Click(object sender, EventArgs e)
        {
            CommandPanel.Focus();
        }

        private void OptionsValueChanged(object sender, EventArgs e)
        {
            CommandPanel.Focus();
        }
    }
}
