namespace DAFBreakdown.DAFGroup.Control
{
    partial class PortfolioControl
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
            this.SelectComboBox = new System.Windows.Forms.ComboBox();
            this.CashGroupBox = new System.Windows.Forms.GroupBox();
            this.CashLabel = new System.Windows.Forms.Label();
            this.FeeGroupBox = new System.Windows.Forms.GroupBox();
            this.FeeLabel = new System.Windows.Forms.Label();
            this.CashGroupBox.SuspendLayout();
            this.FeeGroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // SelectComboBox
            // 
            this.SelectComboBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.SelectComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.SelectComboBox.FormattingEnabled = true;
            this.SelectComboBox.Location = new System.Drawing.Point(0, 0);
            this.SelectComboBox.Name = "SelectComboBox";
            this.SelectComboBox.Size = new System.Drawing.Size(129, 21);
            this.SelectComboBox.TabIndex = 0;
            this.SelectComboBox.TabStop = false;
            this.SelectComboBox.SelectedValueChanged += new System.EventHandler(this.SelectComboBox_SelectedValueChanged);
            // 
            // CashGroupBox
            // 
            this.CashGroupBox.Controls.Add(this.CashLabel);
            this.CashGroupBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.CashGroupBox.Location = new System.Drawing.Point(0, 21);
            this.CashGroupBox.Name = "CashGroupBox";
            this.CashGroupBox.Size = new System.Drawing.Size(129, 33);
            this.CashGroupBox.TabIndex = 1;
            this.CashGroupBox.TabStop = false;
            this.CashGroupBox.Text = "Доступно:";
            // 
            // CashLabel
            // 
            this.CashLabel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.CashLabel.Location = new System.Drawing.Point(3, 16);
            this.CashLabel.Name = "CashLabel";
            this.CashLabel.Size = new System.Drawing.Size(123, 14);
            this.CashLabel.TabIndex = 0;
            this.CashLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // FeeGroupBox
            // 
            this.FeeGroupBox.Controls.Add(this.FeeLabel);
            this.FeeGroupBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.FeeGroupBox.Location = new System.Drawing.Point(0, 54);
            this.FeeGroupBox.Name = "FeeGroupBox";
            this.FeeGroupBox.Size = new System.Drawing.Size(129, 33);
            this.FeeGroupBox.TabIndex = 2;
            this.FeeGroupBox.TabStop = false;
            this.FeeGroupBox.Text = "Комиссия:";
            // 
            // FeeLabel
            // 
            this.FeeLabel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.FeeLabel.Location = new System.Drawing.Point(3, 16);
            this.FeeLabel.Name = "FeeLabel";
            this.FeeLabel.Size = new System.Drawing.Size(123, 14);
            this.FeeLabel.TabIndex = 0;
            this.FeeLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // PortfolioControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.Color.Transparent;
            this.Controls.Add(this.FeeGroupBox);
            this.Controls.Add(this.CashGroupBox);
            this.Controls.Add(this.SelectComboBox);
            this.Name = "PortfolioControl";
            this.Size = new System.Drawing.Size(129, 87);
            this.CashGroupBox.ResumeLayout(false);
            this.FeeGroupBox.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.ComboBox SelectComboBox;
        private System.Windows.Forms.GroupBox CashGroupBox;
        private System.Windows.Forms.Label CashLabel;
        private System.Windows.Forms.GroupBox FeeGroupBox;
        private System.Windows.Forms.Label FeeLabel;
    }
}
