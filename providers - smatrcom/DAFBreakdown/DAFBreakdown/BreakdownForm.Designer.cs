namespace DAFBreakdown
{
    partial class BreakdownForm
    {
        /// <summary>
        /// Требуется переменная конструктора.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Освободить все используемые ресурсы.
        /// </summary>
        /// <param name="disposing">истинно, если управляемый ресурс должен быть удален; иначе ложно.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Код, автоматически созданный конструктором форм Windows

        /// <summary>
        /// Обязательный метод для поддержки конструктора - не изменяйте
        /// содержимое данного метода при помощи редактора кода.
        /// </summary>
        private void InitializeComponent()
        {
            this.UpPanel = new System.Windows.Forms.Panel();
            this.InfoPortfolios = new DAFBreakdown.DAFGroup.Control.PortfolioControl();
            this.ConnectPanel = new System.Windows.Forms.Panel();
            this.ConnectButton = new System.Windows.Forms.Button();
            this.OnConnectPanel = new System.Windows.Forms.Panel();
            this.MainTabControl = new System.Windows.Forms.TabControl();
            this.OptionsTabPage = new System.Windows.Forms.TabPage();
            this.Favorites = new DAFBreakdown.DAFGroup.Control.FavoritesControl();
            this.TikersPanel = new System.Windows.Forms.Panel();
            this.TikerRemoveButton = new System.Windows.Forms.Button();
            this.TikerAddButton = new System.Windows.Forms.Button();
            this.TikersTabControl = new System.Windows.Forms.TabControl();
            this.FavoritesTabPage = new System.Windows.Forms.TabPage();
            this.FavoritesTabControl = new System.Windows.Forms.TabControl();
            this.UpPanel.SuspendLayout();
            this.ConnectPanel.SuspendLayout();
            this.MainTabControl.SuspendLayout();
            this.OptionsTabPage.SuspendLayout();
            this.TikersPanel.SuspendLayout();
            this.FavoritesTabPage.SuspendLayout();
            this.SuspendLayout();
            // 
            // UpPanel
            // 
            this.UpPanel.BackColor = System.Drawing.Color.Transparent;
            this.UpPanel.Controls.Add(this.InfoPortfolios);
            this.UpPanel.Controls.Add(this.ConnectPanel);
            this.UpPanel.Dock = System.Windows.Forms.DockStyle.Top;
            this.UpPanel.Location = new System.Drawing.Point(0, 0);
            this.UpPanel.Name = "UpPanel";
            this.UpPanel.Size = new System.Drawing.Size(794, 87);
            this.UpPanel.TabIndex = 0;
            // 
            // InfoPortfolios
            // 
            this.InfoPortfolios.BackColor = System.Drawing.Color.Transparent;
            this.InfoPortfolios.Dock = System.Windows.Forms.DockStyle.Left;
            this.InfoPortfolios.Location = new System.Drawing.Point(117, 0);
            this.InfoPortfolios.Name = "InfoPortfolios";
            this.InfoPortfolios.Size = new System.Drawing.Size(129, 87);
            this.InfoPortfolios.TabIndex = 1;
            this.InfoPortfolios.TabStop = false;
            // 
            // ConnectPanel
            // 
            this.ConnectPanel.Controls.Add(this.ConnectButton);
            this.ConnectPanel.Controls.Add(this.OnConnectPanel);
            this.ConnectPanel.Dock = System.Windows.Forms.DockStyle.Left;
            this.ConnectPanel.Location = new System.Drawing.Point(0, 0);
            this.ConnectPanel.Name = "ConnectPanel";
            this.ConnectPanel.Padding = new System.Windows.Forms.Padding(30, 25, 10, 20);
            this.ConnectPanel.Size = new System.Drawing.Size(117, 87);
            this.ConnectPanel.TabIndex = 0;
            // 
            // ConnectButton
            // 
            this.ConnectButton.BackColor = System.Drawing.Color.Transparent;
            this.ConnectButton.Dock = System.Windows.Forms.DockStyle.Top;
            this.ConnectButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.ConnectButton.Location = new System.Drawing.Point(30, 25);
            this.ConnectButton.Name = "ConnectButton";
            this.ConnectButton.Size = new System.Drawing.Size(77, 23);
            this.ConnectButton.TabIndex = 2;
            this.ConnectButton.TabStop = false;
            this.ConnectButton.Text = "Старт";
            this.ConnectButton.UseVisualStyleBackColor = false;
            this.ConnectButton.Click += new System.EventHandler(this.ConnectButton_Click);
            // 
            // OnConnectPanel
            // 
            this.OnConnectPanel.BackColor = System.Drawing.Color.Transparent;
            this.OnConnectPanel.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.OnConnectPanel.Location = new System.Drawing.Point(30, 57);
            this.OnConnectPanel.Name = "OnConnectPanel";
            this.OnConnectPanel.Size = new System.Drawing.Size(77, 10);
            this.OnConnectPanel.TabIndex = 1;
            // 
            // MainTabControl
            // 
            this.MainTabControl.Controls.Add(this.OptionsTabPage);
            this.MainTabControl.Controls.Add(this.FavoritesTabPage);
            this.MainTabControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.MainTabControl.Location = new System.Drawing.Point(0, 87);
            this.MainTabControl.Name = "MainTabControl";
            this.MainTabControl.SelectedIndex = 0;
            this.MainTabControl.Size = new System.Drawing.Size(794, 395);
            this.MainTabControl.TabIndex = 1;
            this.MainTabControl.TabStop = false;
            // 
            // OptionsTabPage
            // 
            this.OptionsTabPage.Controls.Add(this.Favorites);
            this.OptionsTabPage.Controls.Add(this.TikersPanel);
            this.OptionsTabPage.Controls.Add(this.TikersTabControl);
            this.OptionsTabPage.Location = new System.Drawing.Point(4, 22);
            this.OptionsTabPage.Name = "OptionsTabPage";
            this.OptionsTabPage.Size = new System.Drawing.Size(786, 369);
            this.OptionsTabPage.TabIndex = 0;
            this.OptionsTabPage.Text = "Настройки";
            this.OptionsTabPage.UseVisualStyleBackColor = true;
            // 
            // Favorites
            // 
            this.Favorites.BackColor = System.Drawing.Color.Transparent;
            this.Favorites.Dock = System.Windows.Forms.DockStyle.Fill;
            this.Favorites.Location = new System.Drawing.Point(516, 0);
            this.Favorites.Name = "Favorites";
            this.Favorites.Size = new System.Drawing.Size(270, 369);
            this.Favorites.TabIndex = 3;
            this.Favorites.TabStop = false;
            this.Favorites.FavoriteRemove += new DAFBreakdown.DAFGroup.Control.FavoritesControl.EventHandler(this.Favorites_FavoriteRemove);
            // 
            // TikersPanel
            // 
            this.TikersPanel.Controls.Add(this.TikerRemoveButton);
            this.TikersPanel.Controls.Add(this.TikerAddButton);
            this.TikersPanel.Dock = System.Windows.Forms.DockStyle.Left;
            this.TikersPanel.Location = new System.Drawing.Point(446, 0);
            this.TikersPanel.Name = "TikersPanel";
            this.TikersPanel.Padding = new System.Windows.Forms.Padding(10, 90, 10, 90);
            this.TikersPanel.Size = new System.Drawing.Size(70, 369);
            this.TikersPanel.TabIndex = 2;
            // 
            // TikerRemoveButton
            // 
            this.TikerRemoveButton.BackgroundImage = global::DAFBreakdown.Properties.Resources.ToLeft;
            this.TikerRemoveButton.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Zoom;
            this.TikerRemoveButton.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.TikerRemoveButton.FlatAppearance.BorderSize = 0;
            this.TikerRemoveButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.TikerRemoveButton.Location = new System.Drawing.Point(10, 192);
            this.TikerRemoveButton.Name = "TikerRemoveButton";
            this.TikerRemoveButton.Size = new System.Drawing.Size(50, 87);
            this.TikerRemoveButton.TabIndex = 1;
            this.TikerRemoveButton.TabStop = false;
            this.TikerRemoveButton.UseVisualStyleBackColor = true;
            this.TikerRemoveButton.Click += new System.EventHandler(this.TikerRemoveButton_Click);
            // 
            // TikerAddButton
            // 
            this.TikerAddButton.BackgroundImage = global::DAFBreakdown.Properties.Resources.ToRight;
            this.TikerAddButton.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Zoom;
            this.TikerAddButton.Dock = System.Windows.Forms.DockStyle.Top;
            this.TikerAddButton.FlatAppearance.BorderSize = 0;
            this.TikerAddButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.TikerAddButton.Location = new System.Drawing.Point(10, 90);
            this.TikerAddButton.Name = "TikerAddButton";
            this.TikerAddButton.Size = new System.Drawing.Size(50, 87);
            this.TikerAddButton.TabIndex = 0;
            this.TikerAddButton.TabStop = false;
            this.TikerAddButton.UseVisualStyleBackColor = true;
            this.TikerAddButton.Click += new System.EventHandler(this.TikerAddButton_Click);
            // 
            // TikersTabControl
            // 
            this.TikersTabControl.Dock = System.Windows.Forms.DockStyle.Left;
            this.TikersTabControl.Location = new System.Drawing.Point(0, 0);
            this.TikersTabControl.Name = "TikersTabControl";
            this.TikersTabControl.SelectedIndex = 0;
            this.TikersTabControl.Size = new System.Drawing.Size(446, 369);
            this.TikersTabControl.TabIndex = 1;
            this.TikersTabControl.TabStop = false;
            // 
            // FavoritesTabPage
            // 
            this.FavoritesTabPage.Controls.Add(this.FavoritesTabControl);
            this.FavoritesTabPage.Location = new System.Drawing.Point(4, 22);
            this.FavoritesTabPage.Name = "FavoritesTabPage";
            this.FavoritesTabPage.Size = new System.Drawing.Size(786, 369);
            this.FavoritesTabPage.TabIndex = 1;
            this.FavoritesTabPage.Text = "Инструменты";
            this.FavoritesTabPage.UseVisualStyleBackColor = true;
            // 
            // FavoritesTabControl
            // 
            this.FavoritesTabControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.FavoritesTabControl.Location = new System.Drawing.Point(0, 0);
            this.FavoritesTabControl.Name = "FavoritesTabControl";
            this.FavoritesTabControl.SelectedIndex = 0;
            this.FavoritesTabControl.Size = new System.Drawing.Size(786, 369);
            this.FavoritesTabControl.TabIndex = 0;
            this.FavoritesTabControl.TabStop = false;
            // 
            // BreakdownForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(794, 482);
            this.Controls.Add(this.MainTabControl);
            this.Controls.Add(this.UpPanel);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "BreakdownForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "DAFGroup: Пробой уровня";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.BreakdownForm_FormClosing);
            this.UpPanel.ResumeLayout(false);
            this.ConnectPanel.ResumeLayout(false);
            this.MainTabControl.ResumeLayout(false);
            this.OptionsTabPage.ResumeLayout(false);
            this.TikersPanel.ResumeLayout(false);
            this.FavoritesTabPage.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Panel UpPanel;
        private System.Windows.Forms.Panel ConnectPanel;
        private System.Windows.Forms.Panel OnConnectPanel;
        private System.Windows.Forms.Button ConnectButton;
        private System.Windows.Forms.TabControl MainTabControl;
        private System.Windows.Forms.TabPage OptionsTabPage;
        private System.Windows.Forms.TabPage FavoritesTabPage;
        private System.Windows.Forms.TabControl FavoritesTabControl;
        private System.Windows.Forms.TabControl TikersTabControl;
        private DAFBreakdown.DAFGroup.Control.PortfolioControl InfoPortfolios;
        private System.Windows.Forms.Panel TikersPanel;
        private System.Windows.Forms.Button TikerAddButton;
        private System.Windows.Forms.Button TikerRemoveButton;
        private DAFBreakdown.DAFGroup.Control.FavoritesControl Favorites;
    }
}

