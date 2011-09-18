namespace DAFBreakdown.DAFGroup.Control
{
    partial class IPPortControl
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
            this.ValueLabel = new System.Windows.Forms.Label();
            this.RemoveButton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // ValueLabel
            // 
            this.ValueLabel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ValueLabel.Location = new System.Drawing.Point(0, 0);
            this.ValueLabel.Name = "ValueLabel";
            this.ValueLabel.Size = new System.Drawing.Size(239, 20);
            this.ValueLabel.TabIndex = 0;
            this.ValueLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.ValueLabel.Click += new System.EventHandler(this.ValueLabel_Click);
            // 
            // RemoveButton
            // 
            this.RemoveButton.Dock = System.Windows.Forms.DockStyle.Right;
            this.RemoveButton.FlatAppearance.BorderSize = 0;
            this.RemoveButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.RemoveButton.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.RemoveButton.ForeColor = System.Drawing.Color.Red;
            this.RemoveButton.Location = new System.Drawing.Point(239, 0);
            this.RemoveButton.Name = "RemoveButton";
            this.RemoveButton.Size = new System.Drawing.Size(31, 20);
            this.RemoveButton.TabIndex = 1;
            this.RemoveButton.TabStop = false;
            this.RemoveButton.Text = "X";
            this.RemoveButton.UseVisualStyleBackColor = true;
            this.RemoveButton.Click += new System.EventHandler(this.RemoveButton_Click);
            // 
            // IPPortControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.Color.Transparent;
            this.Controls.Add(this.ValueLabel);
            this.Controls.Add(this.RemoveButton);
            this.Name = "IPPortControl";
            this.Size = new System.Drawing.Size(270, 20);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Label ValueLabel;
        private System.Windows.Forms.Button RemoveButton;
    }
}
