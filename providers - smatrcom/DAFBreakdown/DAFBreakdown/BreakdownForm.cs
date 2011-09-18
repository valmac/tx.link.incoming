using System;
using System.Collections.Generic;
using System.Drawing;
using System.Threading;
using System.Windows.Forms;

namespace DAFBreakdown
{
    public partial class BreakdownForm : Form
    {
        private delegate void CallBackAddTikers(DAFGroup.PortfolioInfo Portfolio, List<DAFGroup.Tiker> Tikers);

        private DAFGroup.DAFLog dafLog;
        private DAFGroup.SmartCOM SmartCOMServer;

        public BreakdownForm(DAFGroup.SmartAccount InAccount, DAFGroup.DAFLog InLog, EventHandler InClosed)
        {
            InitializeComponent();

            dafLog = InLog;
            FormClosed += new FormClosedEventHandler(InClosed);
            SmartCOMServer = new DAFBreakdown.DAFGroup.SmartCOM(InAccount, InLog);
            SmartCOMServer.ReadyTikers += new DAFBreakdown.DAFGroup.SmartCOM.EventHandler(OnReadyTikers);
            SmartCOMServer.ReadyPortfolios += new DAFBreakdown.DAFGroup.SmartCOM.EventHandler(OnReadyPortfolios);
            SmartCOMServer.ChangedConnection += new DAFBreakdown.DAFGroup.SmartCOM.EventHandler(OnChangedConnection);
            SmartCOMServer.ChangedPortfolio += new EventHandler<DAFBreakdown.DAFGroup.PortfolioEventArgs>(OnChangedPortfolio);
            OnChangedConnection();
        }

        private void ConnectButton_Click(object sender, EventArgs e)
        {
            ConnectButton.Enabled = false;
            ConnectPanel.Focus();
            SmartCOMServer.Start();
        }

        private void OnRemove(object sender, DAFGroup.FavoriteTiker tiker)
        {
            Favorites.AddTiker(tiker);
            FavoritesTabControl.TabPages.Add(new DAFGroup.Control.SymbolTabControl(tiker.Portfolio + ":" + tiker.InfoTiker.ShortName));
        }

        private void Favorites_FavoriteRemove(object sender, DAFBreakdown.DAFGroup.FavoriteTiker tiker)
        {
            foreach (DAFGroup.Control.TikersControl tempTikersControl in TikersTabControl.TabPages)
                if (tempTikersControl.AddTiker(tiker))
                    break;
        }

        private void TikerAddButton_Click(object sender, EventArgs e)
        {
            TikersPanel.Focus();
            if (TikersTabControl.SelectedIndex > -1)
                ((DAFGroup.Control.TikersControl)TikersTabControl.SelectedTab).RemoveSelect();
        }

        private void TikerRemoveButton_Click(object sender, EventArgs e)
        {
            TikersPanel.Focus();
            Favorites.RemoveSelect();
        }

        private void OnReadyTikers()
        {
            new Thread(ThreadForTikers).Start();
        }

        private void OnReadyPortfolios()
        {
            foreach (DAFGroup.PortfolioInfo tempPortfolioInfo in SmartCOMServer.GetPortfolios)
                InfoPortfolios.Add(tempPortfolioInfo);
            SmartCOMServer.ListenStart();
        }

        private void OnChangedConnection()
        {
            OnConnectPanel.BackColor = (SmartCOMServer.IsConnected ? Color.Green : SmartCOMServer.IsReady ? Color.Red : Color.DarkGray);
        }

        private void OnChangedPortfolio(object sender, DAFGroup.PortfolioEventArgs e)
        {
            InfoPortfolios.UpDate(e.Name, e.Cash, e.Fee);
        }

        private void ThreadForTikers()
        {
            foreach (DAFGroup.PortfolioInfo tempPortfolioInfo in SmartCOMServer.GetPortfolios)
            {
                List<DAFGroup.Tiker> tempTikers = new List<DAFBreakdown.DAFGroup.Tiker>();
                foreach (DAFGroup.Tiker tempTiker in SmartCOMServer.GetTikers)
                    if (tempTiker.ExchName == tempPortfolioInfo.Exch)
                        tempTikers.Add(tempTiker);
                if (tempTikers.Count > 0)
                    AddTikers(tempPortfolioInfo, tempTikers);
            }
        }

        private void AddTikers(DAFGroup.PortfolioInfo Portfolio, List<DAFGroup.Tiker> Tikers)
        {
            if (TikersTabControl.InvokeRequired)
                TikersTabControl.BeginInvoke(new CallBackAddTikers(AddTikers), new object[] { Portfolio, Tikers });
            else
                TikersTabControl.Controls.Add(new DAFGroup.Control.TikersControl(Portfolio, Tikers, OnRemove));
        }

        private void FavoriteGridView_RowEnter(object sender, DataGridViewCellEventArgs e)
        {

        }

        private void BreakdownForm_FormClosing(object sender, FormClosingEventArgs e)
        {
            Enabled = false;
            if (SmartCOMServer != null && SmartCOMServer.IsReady)
            {
                FormClose IdleCloseForm = new FormClose();
                SmartCOMServer.Destroyed += new DAFBreakdown.DAFGroup.SmartCOM.EventHandler(IdleCloseForm.OnDestroyed);
                SmartCOMServer.Dispose();
                dafLog.AddMessages("SmartCOM:Destroyed", IdleCloseForm.ShowDialog().ToString());
                IdleCloseForm.Dispose();
            }
            Enabled = true;
        }
    }
}
