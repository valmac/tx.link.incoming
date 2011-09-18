namespace DAFBreakdown.DAFGroup.Control
{
    partial class SymbolControl
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
            this.LeftPanel = new System.Windows.Forms.Panel();
            this.OptionsPanel = new System.Windows.Forms.Panel();
            this.OptionsBreakdownPanel = new System.Windows.Forms.Panel();
            this.OptionsBreakdownNumericUpDown = new System.Windows.Forms.NumericUpDown();
            this.OptionsBreakdownLabel = new System.Windows.Forms.Label();
            this.OptionsIntervalPanel = new System.Windows.Forms.Panel();
            this.OptionsIntervalNumericUpDown = new System.Windows.Forms.NumericUpDown();
            this.OptionsIntervalLabel = new System.Windows.Forms.Label();
            this.InfoGroupBox = new System.Windows.Forms.GroupBox();
            this.InfoProfitGroupBox = new System.Windows.Forms.GroupBox();
            this.InfoProfitLabel = new System.Windows.Forms.Label();
            this.InfoVolumeGroupBox = new System.Windows.Forms.GroupBox();
            this.InfoVolumeLabel = new System.Windows.Forms.Label();
            this.InfoPriceGroupBox = new System.Windows.Forms.GroupBox();
            this.InfoPriceLabel = new System.Windows.Forms.Label();
            this.TargetGroupBox = new System.Windows.Forms.GroupBox();
            this.TargetPriceGroupBox = new System.Windows.Forms.GroupBox();
            this.TargetPriceLabel = new System.Windows.Forms.Label();
            this.TargetSellGroupBox = new System.Windows.Forms.GroupBox();
            this.TargetSellLabel = new System.Windows.Forms.Label();
            this.TargetBuyGroupBox = new System.Windows.Forms.GroupBox();
            this.TargetBuyLabel = new System.Windows.Forms.Label();
            this.GraphPictureBox = new System.Windows.Forms.PictureBox();
            this.RightPanel = new System.Windows.Forms.Panel();
            this.TransactionsGridView = new System.Windows.Forms.DataGridView();
            this.CommandPanel = new System.Windows.Forms.Panel();
            this.StatusButton = new System.Windows.Forms.Button();
            this.RemoveButton = new System.Windows.Forms.Button();
            this.LeftPanel.SuspendLayout();
            this.OptionsPanel.SuspendLayout();
            this.OptionsBreakdownPanel.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.OptionsBreakdownNumericUpDown)).BeginInit();
            this.OptionsIntervalPanel.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.OptionsIntervalNumericUpDown)).BeginInit();
            this.InfoGroupBox.SuspendLayout();
            this.InfoProfitGroupBox.SuspendLayout();
            this.InfoVolumeGroupBox.SuspendLayout();
            this.InfoPriceGroupBox.SuspendLayout();
            this.TargetGroupBox.SuspendLayout();
            this.TargetPriceGroupBox.SuspendLayout();
            this.TargetSellGroupBox.SuspendLayout();
            this.TargetBuyGroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.GraphPictureBox)).BeginInit();
            this.RightPanel.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.TransactionsGridView)).BeginInit();
            this.CommandPanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // LeftPanel
            // 
            this.LeftPanel.Controls.Add(this.TargetGroupBox);
            this.LeftPanel.Controls.Add(this.InfoGroupBox);
            this.LeftPanel.Controls.Add(this.OptionsPanel);
            this.LeftPanel.Controls.Add(this.GraphPictureBox);
            this.LeftPanel.Dock = System.Windows.Forms.DockStyle.Left;
            this.LeftPanel.Location = new System.Drawing.Point(0, 0);
            this.LeftPanel.Name = "LeftPanel";
            this.LeftPanel.Size = new System.Drawing.Size(350, 369);
            this.LeftPanel.TabIndex = 0;
            // 
            // OptionsPanel
            // 
            this.OptionsPanel.Controls.Add(this.OptionsBreakdownPanel);
            this.OptionsPanel.Controls.Add(this.OptionsIntervalPanel);
            this.OptionsPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.OptionsPanel.Location = new System.Drawing.Point(0, 200);
            this.OptionsPanel.Name = "OptionsPanel";
            this.OptionsPanel.Size = new System.Drawing.Size(350, 169);
            this.OptionsPanel.TabIndex = 2;
            // 
            // OptionsBreakdownPanel
            // 
            this.OptionsBreakdownPanel.Controls.Add(this.OptionsBreakdownNumericUpDown);
            this.OptionsBreakdownPanel.Controls.Add(this.OptionsBreakdownLabel);
            this.OptionsBreakdownPanel.Dock = System.Windows.Forms.DockStyle.Top;
            this.OptionsBreakdownPanel.Location = new System.Drawing.Point(0, 20);
            this.OptionsBreakdownPanel.Name = "OptionsBreakdownPanel";
            this.OptionsBreakdownPanel.Size = new System.Drawing.Size(350, 20);
            this.OptionsBreakdownPanel.TabIndex = 1;
            // 
            // OptionsBreakdownNumericUpDown
            // 
            this.OptionsBreakdownNumericUpDown.DecimalPlaces = 2;
            this.OptionsBreakdownNumericUpDown.Dock = System.Windows.Forms.DockStyle.Fill;
            this.OptionsBreakdownNumericUpDown.Increment = new decimal(new int[] {
            5,
            0,
            0,
            65536});
            this.OptionsBreakdownNumericUpDown.Location = new System.Drawing.Point(85, 0);
            this.OptionsBreakdownNumericUpDown.Maximum = new decimal(new int[] {
            1000,
            0,
            0,
            0});
            this.OptionsBreakdownNumericUpDown.Minimum = new decimal(new int[] {
            1000,
            0,
            0,
            -2147483648});
            this.OptionsBreakdownNumericUpDown.Name = "OptionsBreakdownNumericUpDown";
            this.OptionsBreakdownNumericUpDown.Size = new System.Drawing.Size(265, 20);
            this.OptionsBreakdownNumericUpDown.TabIndex = 1;
            this.OptionsBreakdownNumericUpDown.TabStop = false;
            this.OptionsBreakdownNumericUpDown.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.OptionsBreakdownNumericUpDown.ValueChanged += new System.EventHandler(this.OptionsValueChanged);
            // 
            // OptionsBreakdownLabel
            // 
            this.OptionsBreakdownLabel.Dock = System.Windows.Forms.DockStyle.Left;
            this.OptionsBreakdownLabel.Location = new System.Drawing.Point(0, 0);
            this.OptionsBreakdownLabel.Name = "OptionsBreakdownLabel";
            this.OptionsBreakdownLabel.Size = new System.Drawing.Size(85, 20);
            this.OptionsBreakdownLabel.TabIndex = 0;
            this.OptionsBreakdownLabel.Text = "Пробой:";
            this.OptionsBreakdownLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // OptionsIntervalPanel
            // 
            this.OptionsIntervalPanel.Controls.Add(this.OptionsIntervalNumericUpDown);
            this.OptionsIntervalPanel.Controls.Add(this.OptionsIntervalLabel);
            this.OptionsIntervalPanel.Dock = System.Windows.Forms.DockStyle.Top;
            this.OptionsIntervalPanel.Location = new System.Drawing.Point(0, 0);
            this.OptionsIntervalPanel.Name = "OptionsIntervalPanel";
            this.OptionsIntervalPanel.Size = new System.Drawing.Size(350, 20);
            this.OptionsIntervalPanel.TabIndex = 0;
            // 
            // OptionsIntervalNumericUpDown
            // 
            this.OptionsIntervalNumericUpDown.Dock = System.Windows.Forms.DockStyle.Fill;
            this.OptionsIntervalNumericUpDown.Location = new System.Drawing.Point(85, 0);
            this.OptionsIntervalNumericUpDown.Maximum = new decimal(new int[] {
            120,
            0,
            0,
            0});
            this.OptionsIntervalNumericUpDown.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.OptionsIntervalNumericUpDown.Name = "OptionsIntervalNumericUpDown";
            this.OptionsIntervalNumericUpDown.Size = new System.Drawing.Size(265, 20);
            this.OptionsIntervalNumericUpDown.TabIndex = 1;
            this.OptionsIntervalNumericUpDown.TabStop = false;
            this.OptionsIntervalNumericUpDown.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.OptionsIntervalNumericUpDown.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.OptionsIntervalNumericUpDown.ValueChanged += new System.EventHandler(this.OptionsValueChanged);
            // 
            // OptionsIntervalLabel
            // 
            this.OptionsIntervalLabel.Dock = System.Windows.Forms.DockStyle.Left;
            this.OptionsIntervalLabel.Location = new System.Drawing.Point(0, 0);
            this.OptionsIntervalLabel.Name = "OptionsIntervalLabel";
            this.OptionsIntervalLabel.Size = new System.Drawing.Size(85, 20);
            this.OptionsIntervalLabel.TabIndex = 0;
            this.OptionsIntervalLabel.Text = "Интервал:";
            this.OptionsIntervalLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // InfoGroupBox
            // 
            this.InfoGroupBox.Controls.Add(this.InfoProfitGroupBox);
            this.InfoGroupBox.Controls.Add(this.InfoVolumeGroupBox);
            this.InfoGroupBox.Controls.Add(this.InfoPriceGroupBox);
            this.InfoGroupBox.Dock = System.Windows.Forms.DockStyle.Right;
            this.InfoGroupBox.Location = new System.Drawing.Point(250, 200);
            this.InfoGroupBox.Name = "InfoGroupBox";
            this.InfoGroupBox.Size = new System.Drawing.Size(100, 169);
            this.InfoGroupBox.TabIndex = 3;
            this.InfoGroupBox.TabStop = false;
            this.InfoGroupBox.Text = "Позиция";
            // 
            // InfoProfitGroupBox
            // 
            this.InfoProfitGroupBox.Controls.Add(this.InfoProfitLabel);
            this.InfoProfitGroupBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.InfoProfitGroupBox.Location = new System.Drawing.Point(3, 90);
            this.InfoProfitGroupBox.Name = "InfoProfitGroupBox";
            this.InfoProfitGroupBox.Size = new System.Drawing.Size(94, 37);
            this.InfoProfitGroupBox.TabIndex = 2;
            this.InfoProfitGroupBox.TabStop = false;
            this.InfoProfitGroupBox.Text = "Прибыль";
            // 
            // InfoProfitLabel
            // 
            this.InfoProfitLabel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.InfoProfitLabel.Location = new System.Drawing.Point(3, 16);
            this.InfoProfitLabel.Name = "InfoProfitLabel";
            this.InfoProfitLabel.Size = new System.Drawing.Size(88, 18);
            this.InfoProfitLabel.TabIndex = 0;
            this.InfoProfitLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // InfoVolumeGroupBox
            // 
            this.InfoVolumeGroupBox.Controls.Add(this.InfoVolumeLabel);
            this.InfoVolumeGroupBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.InfoVolumeGroupBox.Location = new System.Drawing.Point(3, 53);
            this.InfoVolumeGroupBox.Name = "InfoVolumeGroupBox";
            this.InfoVolumeGroupBox.Size = new System.Drawing.Size(94, 37);
            this.InfoVolumeGroupBox.TabIndex = 1;
            this.InfoVolumeGroupBox.TabStop = false;
            this.InfoVolumeGroupBox.Text = "Объём";
            // 
            // InfoVolumeLabel
            // 
            this.InfoVolumeLabel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.InfoVolumeLabel.Location = new System.Drawing.Point(3, 16);
            this.InfoVolumeLabel.Name = "InfoVolumeLabel";
            this.InfoVolumeLabel.Size = new System.Drawing.Size(88, 18);
            this.InfoVolumeLabel.TabIndex = 0;
            this.InfoVolumeLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // InfoPriceGroupBox
            // 
            this.InfoPriceGroupBox.Controls.Add(this.InfoPriceLabel);
            this.InfoPriceGroupBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.InfoPriceGroupBox.Location = new System.Drawing.Point(3, 16);
            this.InfoPriceGroupBox.Name = "InfoPriceGroupBox";
            this.InfoPriceGroupBox.Size = new System.Drawing.Size(94, 37);
            this.InfoPriceGroupBox.TabIndex = 0;
            this.InfoPriceGroupBox.TabStop = false;
            this.InfoPriceGroupBox.Text = "Ср. Цена";
            // 
            // InfoPriceLabel
            // 
            this.InfoPriceLabel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.InfoPriceLabel.Location = new System.Drawing.Point(3, 16);
            this.InfoPriceLabel.Name = "InfoPriceLabel";
            this.InfoPriceLabel.Size = new System.Drawing.Size(88, 18);
            this.InfoPriceLabel.TabIndex = 0;
            this.InfoPriceLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // TargetGroupBox
            // 
            this.TargetGroupBox.Controls.Add(this.TargetPriceGroupBox);
            this.TargetGroupBox.Controls.Add(this.TargetSellGroupBox);
            this.TargetGroupBox.Controls.Add(this.TargetBuyGroupBox);
            this.TargetGroupBox.Dock = System.Windows.Forms.DockStyle.Right;
            this.TargetGroupBox.Location = new System.Drawing.Point(150, 200);
            this.TargetGroupBox.Name = "TargetGroupBox";
            this.TargetGroupBox.Size = new System.Drawing.Size(100, 169);
            this.TargetGroupBox.TabIndex = 1;
            this.TargetGroupBox.TabStop = false;
            this.TargetGroupBox.Text = "Цены:";
            // 
            // TargetPriceGroupBox
            // 
            this.TargetPriceGroupBox.Controls.Add(this.TargetPriceLabel);
            this.TargetPriceGroupBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.TargetPriceGroupBox.Location = new System.Drawing.Point(3, 90);
            this.TargetPriceGroupBox.Name = "TargetPriceGroupBox";
            this.TargetPriceGroupBox.Size = new System.Drawing.Size(94, 37);
            this.TargetPriceGroupBox.TabIndex = 2;
            this.TargetPriceGroupBox.TabStop = false;
            this.TargetPriceGroupBox.Text = "Текущая";
            // 
            // TargetPriceLabel
            // 
            this.TargetPriceLabel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.TargetPriceLabel.Location = new System.Drawing.Point(3, 16);
            this.TargetPriceLabel.Name = "TargetPriceLabel";
            this.TargetPriceLabel.Size = new System.Drawing.Size(88, 18);
            this.TargetPriceLabel.TabIndex = 0;
            this.TargetPriceLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // TargetSellGroupBox
            // 
            this.TargetSellGroupBox.Controls.Add(this.TargetSellLabel);
            this.TargetSellGroupBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.TargetSellGroupBox.Location = new System.Drawing.Point(3, 53);
            this.TargetSellGroupBox.Name = "TargetSellGroupBox";
            this.TargetSellGroupBox.Size = new System.Drawing.Size(94, 37);
            this.TargetSellGroupBox.TabIndex = 1;
            this.TargetSellGroupBox.TabStop = false;
            this.TargetSellGroupBox.Text = "Продажа";
            // 
            // TargetSellLabel
            // 
            this.TargetSellLabel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.TargetSellLabel.Location = new System.Drawing.Point(3, 16);
            this.TargetSellLabel.Name = "TargetSellLabel";
            this.TargetSellLabel.Size = new System.Drawing.Size(88, 18);
            this.TargetSellLabel.TabIndex = 0;
            this.TargetSellLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // TargetBuyGroupBox
            // 
            this.TargetBuyGroupBox.Controls.Add(this.TargetBuyLabel);
            this.TargetBuyGroupBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.TargetBuyGroupBox.Location = new System.Drawing.Point(3, 16);
            this.TargetBuyGroupBox.Name = "TargetBuyGroupBox";
            this.TargetBuyGroupBox.Size = new System.Drawing.Size(94, 37);
            this.TargetBuyGroupBox.TabIndex = 0;
            this.TargetBuyGroupBox.TabStop = false;
            this.TargetBuyGroupBox.Text = "Покупка";
            // 
            // TargetBuyLabel
            // 
            this.TargetBuyLabel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.TargetBuyLabel.Location = new System.Drawing.Point(3, 16);
            this.TargetBuyLabel.Name = "TargetBuyLabel";
            this.TargetBuyLabel.Size = new System.Drawing.Size(88, 18);
            this.TargetBuyLabel.TabIndex = 0;
            this.TargetBuyLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // GraphPictureBox
            // 
            this.GraphPictureBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.GraphPictureBox.Location = new System.Drawing.Point(0, 0);
            this.GraphPictureBox.Name = "GraphPictureBox";
            this.GraphPictureBox.Size = new System.Drawing.Size(350, 200);
            this.GraphPictureBox.TabIndex = 0;
            this.GraphPictureBox.TabStop = false;
            // 
            // RightPanel
            // 
            this.RightPanel.Controls.Add(this.TransactionsGridView);
            this.RightPanel.Controls.Add(this.CommandPanel);
            this.RightPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.RightPanel.Location = new System.Drawing.Point(350, 0);
            this.RightPanel.Name = "RightPanel";
            this.RightPanel.Size = new System.Drawing.Size(436, 369);
            this.RightPanel.TabIndex = 1;
            // 
            // TransactionsGridView
            // 
            this.TransactionsGridView.AllowUserToAddRows = false;
            this.TransactionsGridView.AllowUserToDeleteRows = false;
            this.TransactionsGridView.AllowUserToResizeColumns = false;
            this.TransactionsGridView.AllowUserToResizeRows = false;
            this.TransactionsGridView.BackgroundColor = System.Drawing.SystemColors.Window;
            this.TransactionsGridView.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.TransactionsGridView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.TransactionsGridView.Location = new System.Drawing.Point(0, 33);
            this.TransactionsGridView.MultiSelect = false;
            this.TransactionsGridView.Name = "TransactionsGridView";
            this.TransactionsGridView.ReadOnly = true;
            this.TransactionsGridView.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.TransactionsGridView.Size = new System.Drawing.Size(436, 336);
            this.TransactionsGridView.TabIndex = 3;
            this.TransactionsGridView.TabStop = false;
            // 
            // CommandPanel
            // 
            this.CommandPanel.Controls.Add(this.StatusButton);
            this.CommandPanel.Controls.Add(this.RemoveButton);
            this.CommandPanel.Dock = System.Windows.Forms.DockStyle.Top;
            this.CommandPanel.Location = new System.Drawing.Point(0, 0);
            this.CommandPanel.Name = "CommandPanel";
            this.CommandPanel.Padding = new System.Windows.Forms.Padding(10, 5, 10, 5);
            this.CommandPanel.Size = new System.Drawing.Size(436, 33);
            this.CommandPanel.TabIndex = 0;
            // 
            // StatusButton
            // 
            this.StatusButton.Dock = System.Windows.Forms.DockStyle.Left;
            this.StatusButton.FlatAppearance.BorderSize = 0;
            this.StatusButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.StatusButton.Location = new System.Drawing.Point(10, 5);
            this.StatusButton.Name = "StatusButton";
            this.StatusButton.Size = new System.Drawing.Size(75, 23);
            this.StatusButton.TabIndex = 1;
            this.StatusButton.TabStop = false;
            this.StatusButton.Text = "Пауза";
            this.StatusButton.UseVisualStyleBackColor = true;
            this.StatusButton.Click += new System.EventHandler(this.StatusButton_Click);
            // 
            // RemoveButton
            // 
            this.RemoveButton.Dock = System.Windows.Forms.DockStyle.Right;
            this.RemoveButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.RemoveButton.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.RemoveButton.ForeColor = System.Drawing.Color.Red;
            this.RemoveButton.Location = new System.Drawing.Point(403, 5);
            this.RemoveButton.Name = "RemoveButton";
            this.RemoveButton.Size = new System.Drawing.Size(23, 23);
            this.RemoveButton.TabIndex = 0;
            this.RemoveButton.TabStop = false;
            this.RemoveButton.Text = "X";
            this.RemoveButton.UseVisualStyleBackColor = true;
            this.RemoveButton.Click += new System.EventHandler(this.RemoveButton_Click);
            // 
            // SymbolControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.Color.Transparent;
            this.Controls.Add(this.RightPanel);
            this.Controls.Add(this.LeftPanel);
            this.Name = "SymbolControl";
            this.Size = new System.Drawing.Size(786, 369);
            this.LeftPanel.ResumeLayout(false);
            this.OptionsPanel.ResumeLayout(false);
            this.OptionsBreakdownPanel.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.OptionsBreakdownNumericUpDown)).EndInit();
            this.OptionsIntervalPanel.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.OptionsIntervalNumericUpDown)).EndInit();
            this.InfoGroupBox.ResumeLayout(false);
            this.InfoProfitGroupBox.ResumeLayout(false);
            this.InfoVolumeGroupBox.ResumeLayout(false);
            this.InfoPriceGroupBox.ResumeLayout(false);
            this.TargetGroupBox.ResumeLayout(false);
            this.TargetPriceGroupBox.ResumeLayout(false);
            this.TargetSellGroupBox.ResumeLayout(false);
            this.TargetBuyGroupBox.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.GraphPictureBox)).EndInit();
            this.RightPanel.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.TransactionsGridView)).EndInit();
            this.CommandPanel.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Panel LeftPanel;
        private System.Windows.Forms.GroupBox TargetGroupBox;
        private System.Windows.Forms.PictureBox GraphPictureBox;
        private System.Windows.Forms.GroupBox TargetSellGroupBox;
        private System.Windows.Forms.Label TargetSellLabel;
        private System.Windows.Forms.GroupBox TargetBuyGroupBox;
        private System.Windows.Forms.Label TargetBuyLabel;
        private System.Windows.Forms.GroupBox TargetPriceGroupBox;
        private System.Windows.Forms.Label TargetPriceLabel;
        private System.Windows.Forms.Panel OptionsPanel;
        private System.Windows.Forms.Panel RightPanel;
        private System.Windows.Forms.Panel CommandPanel;
        private System.Windows.Forms.DataGridView TransactionsGridView;
        private System.Windows.Forms.Button RemoveButton;
        private System.Windows.Forms.Button StatusButton;
        private System.Windows.Forms.Panel OptionsIntervalPanel;
        private System.Windows.Forms.NumericUpDown OptionsIntervalNumericUpDown;
        private System.Windows.Forms.Label OptionsIntervalLabel;
        private System.Windows.Forms.Panel OptionsBreakdownPanel;
        private System.Windows.Forms.NumericUpDown OptionsBreakdownNumericUpDown;
        private System.Windows.Forms.Label OptionsBreakdownLabel;
        private System.Windows.Forms.GroupBox InfoGroupBox;
        private System.Windows.Forms.GroupBox InfoProfitGroupBox;
        private System.Windows.Forms.Label InfoProfitLabel;
        private System.Windows.Forms.GroupBox InfoVolumeGroupBox;
        private System.Windows.Forms.Label InfoVolumeLabel;
        private System.Windows.Forms.GroupBox InfoPriceGroupBox;
        private System.Windows.Forms.Label InfoPriceLabel;
    }
}
