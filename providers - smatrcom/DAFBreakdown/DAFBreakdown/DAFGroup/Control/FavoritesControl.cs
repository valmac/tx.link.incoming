using System.Drawing;
using System.Windows.Forms;

namespace DAFBreakdown.DAFGroup.Control
{
    public partial class FavoritesControl : UserControl
    {
        public event EventHandler FavoriteRemove;
        public delegate void EventHandler(object sender, FavoriteTiker tiker);

        public FavoritesControl()
        {
            InitializeComponent();
            TikersGridView.ColumnHeadersDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter;
            TikersGridView.ColumnHeadersDefaultCellStyle.BackColor = Color.LightGray;
            TikersGridView.RowsDefaultCellStyle.ForeColor = Color.Black;
            TikersGridView.RowsDefaultCellStyle.BackColor = Color.FromArgb(185, 185, 185);
            TikersGridView.RowsDefaultCellStyle.SelectionBackColor = Color.DimGray;
            TikersGridView.RowsDefaultCellStyle.SelectionForeColor = Color.White;
            TikersGridView.SelectionMode = DataGridViewSelectionMode.FullRowSelect;
            TikersGridView.RowHeadersVisible = false;
            TikersGridView.Columns.Add("Portfolio", "Счёт");
            TikersGridView.Columns[0].DefaultCellStyle.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter;
            TikersGridView.Columns[0].Width = 90;
            TikersGridView.Columns[0].Resizable = System.Windows.Forms.DataGridViewTriState.False;
            TikersGridView.Columns.Add("ShortName", "Тикер");
            TikersGridView.Columns[1].DefaultCellStyle.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter;
            TikersGridView.Columns[1].Width = 100;
            TikersGridView.Columns[1].Resizable = System.Windows.Forms.DataGridViewTriState.False;
            TikersGridView.Columns.Add("CodeClass", "Тип");
            TikersGridView.Columns[2].DefaultCellStyle.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter;
            TikersGridView.Columns[2].Width = 60;
            TikersGridView.Columns[2].Resizable = System.Windows.Forms.DataGridViewTriState.False;
            TikersGridView.Rows.Clear();
        }

        private void TikersGridView_CellMouseDoubleClick(object sender, DataGridViewCellMouseEventArgs e)
        {
            if (FavoriteRemove != null)
                FavoriteRemove(this, (FavoriteTiker)TikersGridView.Rows[e.RowIndex].Tag);
            TikersGridView.Rows.RemoveAt(e.RowIndex);   
        }

        public void AddTiker(FavoriteTiker tiker)
        {
            int iAddIndex = TikersGridView.Rows.Add(tiker.GetInfoRow());
            TikersGridView.Rows[iAddIndex].Tag = (object)tiker;
        }

        public void RemoveSelect()
        {
            if (TikersGridView.SelectedRows.Count > 0)
            {
                int RowIndex = TikersGridView.SelectedRows[0].Index;
                if (FavoriteRemove != null)
                    FavoriteRemove(this, (FavoriteTiker)TikersGridView.Rows[RowIndex].Tag);
                TikersGridView.Rows.RemoveAt(RowIndex);
            }
        }
    }
}
