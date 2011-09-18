using System.Collections.Generic;
using System.Drawing;
using System.Threading;
using System.Windows.Forms;

namespace DAFBreakdown.DAFGroup.Control
{
    public partial class TikersControl : TabPage
    {
        public event EventHandler TikerRemove;
        public delegate void EventHandler(object sender, FavoriteTiker tiker);

        private PortfolioInfo Portfolio;

        public TikersControl(PortfolioInfo portfolio, List<Tiker> tikers, EventHandler remove)
        {
            InitializeComponent();
            Portfolio = portfolio;
            Text = Portfolio.Name + ":" + Portfolio.Exch;
            if (remove != null)
                TikerRemove += new EventHandler(remove);
            TikersGridView.ColumnHeadersDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter;
            TikersGridView.ColumnHeadersDefaultCellStyle.BackColor = Color.LightGray;
            TikersGridView.RowsDefaultCellStyle.ForeColor = Color.Black;
            TikersGridView.RowsDefaultCellStyle.BackColor = Color.FromArgb(185, 185, 185);
            TikersGridView.RowsDefaultCellStyle.SelectionBackColor = Color.DimGray;
            TikersGridView.RowsDefaultCellStyle.SelectionForeColor = Color.White;
            TikersGridView.SelectionMode = DataGridViewSelectionMode.FullRowSelect;
            TikersGridView.RowHeadersVisible = false;
            TikersGridView.Columns.Add("ShortName", "Тикер");
            TikersGridView.Columns[0].DefaultCellStyle.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter;
            TikersGridView.Columns[0].Width = 100;
            TikersGridView.Columns[0].Resizable = System.Windows.Forms.DataGridViewTriState.False;
            TikersGridView.Columns.Add("CodeClass", "Тип");
            TikersGridView.Columns[1].DefaultCellStyle.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter;
            TikersGridView.Columns[1].Width = 60;
            TikersGridView.Columns[1].Resizable = System.Windows.Forms.DataGridViewTriState.False;
            TikersGridView.Columns.Add("Step", "Шаг");
            TikersGridView.Columns[2].Width = 80;
            TikersGridView.Columns[2].DefaultCellStyle.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight;
            TikersGridView.Columns[2].Resizable = System.Windows.Forms.DataGridViewTriState.False;
            TikersGridView.Columns.Add("PriceStep", "Цена шага");
            TikersGridView.Columns[3].Width = 85;
            TikersGridView.Columns[3].DefaultCellStyle.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight;
            TikersGridView.Columns[3].Resizable = System.Windows.Forms.DataGridViewTriState.False;
            TikersGridView.Columns.Add("PricePunkt", "Цена пункта");
            TikersGridView.Columns[4].Width = 95;
            TikersGridView.Columns[4].DefaultCellStyle.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight;
            TikersGridView.Columns[4].Resizable = System.Windows.Forms.DataGridViewTriState.False;
            TikersGridView.Rows.Clear();
            new Thread(AddTikers).Start(tikers);
        }

        private void AddTikers(object oTikers)
        {
            if ((List<DAFGroup.Tiker>)oTikers != null && ((List<DAFGroup.Tiker>)oTikers).Count > 0)
                foreach (Tiker tempTiker in (List<DAFGroup.Tiker>)oTikers)
                {
                    int iAddIndex = TikersGridView.Rows.Add(tempTiker.GetInfoRow());
                    TikersGridView.Rows[iAddIndex].Tag = (object)tempTiker;
                }
            AddGridView();
        }

        public bool AddTiker(DAFBreakdown.DAFGroup.FavoriteTiker tiker)
        {
            bool bReturn = false;
            if (Portfolio.Name == tiker.Portfolio)
            {
                int iAddIndex = TikersGridView.Rows.Add(tiker.InfoTiker.GetInfoRow());
                TikersGridView.Rows[iAddIndex].Tag = (object)tiker.InfoTiker;
                bReturn = true;
            }
            return bReturn;
        }

        private void AddGridView()
        {
            if (InvokeRequired)
                BeginInvoke(new System.Windows.Forms.MethodInvoker(AddGridView));
            else
                Controls.Add(TikersGridView);
        }

        private void TikersGridView_CellMouseDoubleClick(object sender, DataGridViewCellMouseEventArgs e)
        {
            if (TikerRemove != null)
                TikerRemove(this, new FavoriteTiker(Portfolio.Name, (Tiker)TikersGridView.Rows[e.RowIndex].Tag));
            TikersGridView.Rows.RemoveAt(e.RowIndex);
        }

        public void RemoveSelect()
        {
            if (TikersGridView.SelectedRows.Count > 0)
            {
                int RowIndex = TikersGridView.SelectedRows[0].Index;
                if (TikerRemove != null)
                    TikerRemove(this, new FavoriteTiker(Portfolio.Name, (Tiker)TikersGridView.Rows[RowIndex].Tag));
                TikersGridView.Rows.RemoveAt(RowIndex);
            }
        }
    }
}
