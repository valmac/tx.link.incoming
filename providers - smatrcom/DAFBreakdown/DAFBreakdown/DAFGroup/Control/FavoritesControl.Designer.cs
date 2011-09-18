namespace DAFBreakdown.DAFGroup.Control
{
    partial class FavoritesControl
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

        #region Код, автоматически созданный конструктором компонентов

        /// <summary> 
        /// Обязательный метод для поддержки конструктора - не изменяйте 
        /// содержимое данного метода при помощи редактора кода.
        /// </summary>
        private void InitializeComponent()
        {
            this.TikersGridView = new System.Windows.Forms.DataGridView();
            ((System.ComponentModel.ISupportInitialize)(this.TikersGridView)).BeginInit();
            this.SuspendLayout();
            // 
            // TikersGridView
            // 
            this.TikersGridView.AllowUserToAddRows = false;
            this.TikersGridView.AllowUserToDeleteRows = false;
            this.TikersGridView.AllowUserToResizeColumns = false;
            this.TikersGridView.AllowUserToResizeRows = false;
            this.TikersGridView.BackgroundColor = System.Drawing.SystemColors.Window;
            this.TikersGridView.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.TikersGridView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.TikersGridView.Location = new System.Drawing.Point(0, 0);
            this.TikersGridView.MultiSelect = false;
            this.TikersGridView.Name = "TikersGridView";
            this.TikersGridView.ReadOnly = true;
            this.TikersGridView.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.TikersGridView.Size = new System.Drawing.Size(551, 403);
            this.TikersGridView.TabIndex = 1;
            this.TikersGridView.TabStop = false;
            this.TikersGridView.CellMouseDoubleClick += new System.Windows.Forms.DataGridViewCellMouseEventHandler(this.TikersGridView_CellMouseDoubleClick);
            // 
            // FavoritesControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.Color.Transparent;
            this.Controls.Add(this.TikersGridView);
            this.Name = "FavoritesControl";
            this.Size = new System.Drawing.Size(551, 403);
            ((System.ComponentModel.ISupportInitialize)(this.TikersGridView)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.DataGridView TikersGridView;
    }
}
