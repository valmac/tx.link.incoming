namespace DAFBreakdown.DAFGroup
{
    partial class LoginForm
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.TitleLabel = new System.Windows.Forms.Label();
            this.LoginTextBox = new System.Windows.Forms.TextBox();
            this.PasswordTextBox = new System.Windows.Forms.TextBox();
            this.OkButton = new System.Windows.Forms.Button();
            this.CanselButton = new System.Windows.Forms.Button();
            this.LoginLabel = new System.Windows.Forms.Label();
            this.PasswordLabel = new System.Windows.Forms.Label();
            this.ServersLabel = new System.Windows.Forms.Label();
            this.IPPortsPanel = new System.Windows.Forms.Panel();
            this.IPPortTextBox = new System.Windows.Forms.TextBox();
            this.AddButton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // TitleLabel
            // 
            this.TitleLabel.Dock = System.Windows.Forms.DockStyle.Top;
            this.TitleLabel.Location = new System.Drawing.Point(0, 0);
            this.TitleLabel.Name = "TitleLabel";
            this.TitleLabel.Size = new System.Drawing.Size(317, 34);
            this.TitleLabel.TabIndex = 0;
            this.TitleLabel.Text = "Введити логин и пароль для доступа к серверу ITinvest";
            this.TitleLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // LoginTextBox
            // 
            this.LoginTextBox.Location = new System.Drawing.Point(12, 51);
            this.LoginTextBox.Name = "LoginTextBox";
            this.LoginTextBox.Size = new System.Drawing.Size(212, 20);
            this.LoginTextBox.TabIndex = 1;
            this.LoginTextBox.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            // 
            // PasswordTextBox
            // 
            this.PasswordTextBox.Location = new System.Drawing.Point(12, 91);
            this.PasswordTextBox.Name = "PasswordTextBox";
            this.PasswordTextBox.PasswordChar = '*';
            this.PasswordTextBox.Size = new System.Drawing.Size(212, 20);
            this.PasswordTextBox.TabIndex = 2;
            this.PasswordTextBox.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            // 
            // OkButton
            // 
            this.OkButton.Location = new System.Drawing.Point(230, 51);
            this.OkButton.Name = "OkButton";
            this.OkButton.Size = new System.Drawing.Size(75, 23);
            this.OkButton.TabIndex = 3;
            this.OkButton.TabStop = false;
            this.OkButton.Text = "Ок";
            this.OkButton.UseVisualStyleBackColor = true;
            this.OkButton.Click += new System.EventHandler(this.OkButton_Click);
            // 
            // CanselButton
            // 
            this.CanselButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.CanselButton.Location = new System.Drawing.Point(230, 88);
            this.CanselButton.Name = "CanselButton";
            this.CanselButton.Size = new System.Drawing.Size(75, 23);
            this.CanselButton.TabIndex = 4;
            this.CanselButton.TabStop = false;
            this.CanselButton.Text = "Отмена";
            this.CanselButton.UseVisualStyleBackColor = true;
            this.CanselButton.Click += new System.EventHandler(this.CanselButton_Click);
            // 
            // LoginLabel
            // 
            this.LoginLabel.AutoSize = true;
            this.LoginLabel.Location = new System.Drawing.Point(12, 34);
            this.LoginLabel.Name = "LoginLabel";
            this.LoginLabel.Size = new System.Drawing.Size(41, 13);
            this.LoginLabel.TabIndex = 5;
            this.LoginLabel.Text = "Логин:";
            // 
            // PasswordLabel
            // 
            this.PasswordLabel.AutoSize = true;
            this.PasswordLabel.Location = new System.Drawing.Point(12, 75);
            this.PasswordLabel.Name = "PasswordLabel";
            this.PasswordLabel.Size = new System.Drawing.Size(48, 13);
            this.PasswordLabel.TabIndex = 6;
            this.PasswordLabel.Text = "Пароль:";
            // 
            // ServersLabel
            // 
            this.ServersLabel.AutoSize = true;
            this.ServersLabel.Location = new System.Drawing.Point(12, 118);
            this.ServersLabel.Name = "ServersLabel";
            this.ServersLabel.Size = new System.Drawing.Size(98, 13);
            this.ServersLabel.TabIndex = 8;
            this.ServersLabel.Text = "Список серверов:";
            // 
            // IPPortsPanel
            // 
            this.IPPortsPanel.AutoScroll = true;
            this.IPPortsPanel.BackColor = System.Drawing.SystemColors.Window;
            this.IPPortsPanel.Location = new System.Drawing.Point(12, 134);
            this.IPPortsPanel.Name = "IPPortsPanel";
            this.IPPortsPanel.Size = new System.Drawing.Size(171, 83);
            this.IPPortsPanel.TabIndex = 9;
            // 
            // IPPortTextBox
            // 
            this.IPPortTextBox.Location = new System.Drawing.Point(189, 168);
            this.IPPortTextBox.Name = "IPPortTextBox";
            this.IPPortTextBox.Size = new System.Drawing.Size(116, 20);
            this.IPPortTextBox.TabIndex = 10;
            // 
            // AddButton
            // 
            this.AddButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.AddButton.Location = new System.Drawing.Point(189, 194);
            this.AddButton.Name = "AddButton";
            this.AddButton.Size = new System.Drawing.Size(116, 23);
            this.AddButton.TabIndex = 11;
            this.AddButton.Text = "Добавить";
            this.AddButton.UseVisualStyleBackColor = true;
            this.AddButton.Click += new System.EventHandler(this.AddButton_Click);
            // 
            // LoginForm
            // 
            this.AcceptButton = this.OkButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.CanselButton;
            this.ClientSize = new System.Drawing.Size(317, 229);
            this.ControlBox = false;
            this.Controls.Add(this.AddButton);
            this.Controls.Add(this.IPPortTextBox);
            this.Controls.Add(this.IPPortsPanel);
            this.Controls.Add(this.ServersLabel);
            this.Controls.Add(this.PasswordLabel);
            this.Controls.Add(this.LoginLabel);
            this.Controls.Add(this.CanselButton);
            this.Controls.Add(this.OkButton);
            this.Controls.Add(this.PasswordTextBox);
            this.Controls.Add(this.LoginTextBox);
            this.Controls.Add(this.TitleLabel);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Name = "LoginForm";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "DAFGroup: Breakdown";
            this.TopMost = true;
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label TitleLabel;
        private System.Windows.Forms.TextBox LoginTextBox;
        private System.Windows.Forms.TextBox PasswordTextBox;
        private System.Windows.Forms.Button OkButton;
        private System.Windows.Forms.Button CanselButton;
        private System.Windows.Forms.Label LoginLabel;
        private System.Windows.Forms.Label PasswordLabel;
        private System.Windows.Forms.Label ServersLabel;
        private System.Windows.Forms.Panel IPPortsPanel;
        private System.Windows.Forms.TextBox IPPortTextBox;
        private System.Windows.Forms.Button AddButton;
    }
}