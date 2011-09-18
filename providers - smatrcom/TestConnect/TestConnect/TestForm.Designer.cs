namespace TestConnect
{
    partial class TestForm
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
            this.CreateButton = new System.Windows.Forms.Button();
            this.ConnectButton = new System.Windows.Forms.Button();
            this.DisconnectButton = new System.Windows.Forms.Button();
            this.StatusGroupBox = new System.Windows.Forms.GroupBox();
            this.StatusLabel = new System.Windows.Forms.Label();
            this.LoginGroupBox = new System.Windows.Forms.GroupBox();
            this.LoginTextBox = new System.Windows.Forms.TextBox();
            this.PasswordGroupBox = new System.Windows.Forms.GroupBox();
            this.PasswordTextBox = new System.Windows.Forms.TextBox();
            this.LastLabel = new System.Windows.Forms.Label();
            this.LastAskLabel = new System.Windows.Forms.Label();
            this.LastBidLabel = new System.Windows.Forms.Label();
            this.IPServerGroupBox = new System.Windows.Forms.GroupBox();
            this.IPTextBox = new System.Windows.Forms.TextBox();
            this.SymbolGroupBox = new System.Windows.Forms.GroupBox();
            this.SymbolTextBox = new System.Windows.Forms.TextBox();
            this.GetBarsButton = new System.Windows.Forms.Button();
            this.ToDateTimePicker = new System.Windows.Forms.DateTimePicker();
            this.InfoBarLabel = new System.Windows.Forms.Label();
            this.LastQuoteLabel = new System.Windows.Forms.Label();
            this.RightPanel = new System.Windows.Forms.Panel();
            this.CanselAllButton = new System.Windows.Forms.Button();
            this.OrderCanselMoveGroupBox = new System.Windows.Forms.GroupBox();
            this.OrderPriceMoveNumericUpDown = new System.Windows.Forms.NumericUpDown();
            this.OrderMoveButton = new System.Windows.Forms.Button();
            this.OrderCanselButton = new System.Windows.Forms.Button();
            this.OrderIdTextBox = new System.Windows.Forms.TextBox();
            this.OrderIdLabel = new System.Windows.Forms.Label();
            this.PlaceOrderGroupBox = new System.Windows.Forms.GroupBox();
            this.OrderCookieLabel = new System.Windows.Forms.Label();
            this.OrderSellButton = new System.Windows.Forms.Button();
            this.OrderBuyButton = new System.Windows.Forms.Button();
            this.OrderPriceNumericUpDown = new System.Windows.Forms.NumericUpDown();
            this.OrderPriceLabel = new System.Windows.Forms.Label();
            this.PortfoliosComboBox = new System.Windows.Forms.ComboBox();
            this.ErrorLabel = new System.Windows.Forms.Label();
            this.StatusGroupBox.SuspendLayout();
            this.LoginGroupBox.SuspendLayout();
            this.PasswordGroupBox.SuspendLayout();
            this.IPServerGroupBox.SuspendLayout();
            this.SymbolGroupBox.SuspendLayout();
            this.RightPanel.SuspendLayout();
            this.OrderCanselMoveGroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.OrderPriceMoveNumericUpDown)).BeginInit();
            this.PlaceOrderGroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.OrderPriceNumericUpDown)).BeginInit();
            this.SuspendLayout();
            // 
            // CreateButton
            // 
            this.CreateButton.Location = new System.Drawing.Point(12, 5);
            this.CreateButton.Name = "CreateButton";
            this.CreateButton.Size = new System.Drawing.Size(75, 23);
            this.CreateButton.TabIndex = 0;
            this.CreateButton.Text = "Create";
            this.CreateButton.UseVisualStyleBackColor = true;
            this.CreateButton.EnabledChanged += new System.EventHandler(this.CreateButton_EnabledChanged);
            this.CreateButton.Click += new System.EventHandler(this.CreateButton_Click);
            // 
            // ConnectButton
            // 
            this.ConnectButton.Enabled = false;
            this.ConnectButton.Location = new System.Drawing.Point(12, 52);
            this.ConnectButton.Name = "ConnectButton";
            this.ConnectButton.Size = new System.Drawing.Size(75, 23);
            this.ConnectButton.TabIndex = 1;
            this.ConnectButton.Text = "Connect";
            this.ConnectButton.UseVisualStyleBackColor = true;
            this.ConnectButton.Click += new System.EventHandler(this.ConnectButton_Click);
            // 
            // DisconnectButton
            // 
            this.DisconnectButton.Enabled = false;
            this.DisconnectButton.Location = new System.Drawing.Point(12, 81);
            this.DisconnectButton.Name = "DisconnectButton";
            this.DisconnectButton.Size = new System.Drawing.Size(75, 23);
            this.DisconnectButton.TabIndex = 2;
            this.DisconnectButton.Text = "Disconnect";
            this.DisconnectButton.UseVisualStyleBackColor = true;
            this.DisconnectButton.Click += new System.EventHandler(this.DisconnectButton_Click);
            // 
            // StatusGroupBox
            // 
            this.StatusGroupBox.Controls.Add(this.StatusLabel);
            this.StatusGroupBox.Location = new System.Drawing.Point(99, 0);
            this.StatusGroupBox.Name = "StatusGroupBox";
            this.StatusGroupBox.Padding = new System.Windows.Forms.Padding(2);
            this.StatusGroupBox.Size = new System.Drawing.Size(150, 30);
            this.StatusGroupBox.TabIndex = 3;
            this.StatusGroupBox.TabStop = false;
            this.StatusGroupBox.Text = "Status";
            // 
            // StatusLabel
            // 
            this.StatusLabel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.StatusLabel.Location = new System.Drawing.Point(2, 15);
            this.StatusLabel.Name = "StatusLabel";
            this.StatusLabel.Size = new System.Drawing.Size(146, 13);
            this.StatusLabel.TabIndex = 4;
            this.StatusLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // LoginGroupBox
            // 
            this.LoginGroupBox.Controls.Add(this.LoginTextBox);
            this.LoginGroupBox.Location = new System.Drawing.Point(12, 152);
            this.LoginGroupBox.Name = "LoginGroupBox";
            this.LoginGroupBox.Size = new System.Drawing.Size(109, 39);
            this.LoginGroupBox.TabIndex = 4;
            this.LoginGroupBox.TabStop = false;
            this.LoginGroupBox.Text = "Login";
            // 
            // LoginTextBox
            // 
            this.LoginTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.LoginTextBox.Location = new System.Drawing.Point(3, 16);
            this.LoginTextBox.Name = "LoginTextBox";
            this.LoginTextBox.Size = new System.Drawing.Size(103, 20);
            this.LoginTextBox.TabIndex = 5;
            // 
            // PasswordGroupBox
            // 
            this.PasswordGroupBox.Controls.Add(this.PasswordTextBox);
            this.PasswordGroupBox.Location = new System.Drawing.Point(12, 197);
            this.PasswordGroupBox.Name = "PasswordGroupBox";
            this.PasswordGroupBox.Size = new System.Drawing.Size(109, 39);
            this.PasswordGroupBox.TabIndex = 5;
            this.PasswordGroupBox.TabStop = false;
            this.PasswordGroupBox.Text = "Password";
            // 
            // PasswordTextBox
            // 
            this.PasswordTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.PasswordTextBox.Location = new System.Drawing.Point(3, 16);
            this.PasswordTextBox.Name = "PasswordTextBox";
            this.PasswordTextBox.PasswordChar = '*';
            this.PasswordTextBox.Size = new System.Drawing.Size(103, 20);
            this.PasswordTextBox.TabIndex = 5;
            // 
            // LastLabel
            // 
            this.LastLabel.AutoSize = true;
            this.LastLabel.Location = new System.Drawing.Point(96, 65);
            this.LastLabel.Name = "LastLabel";
            this.LastLabel.Size = new System.Drawing.Size(48, 13);
            this.LastLabel.TabIndex = 7;
            this.LastLabel.Text = "LastTick";
            this.LastLabel.Click += new System.EventHandler(this.GetPriceBy_Click);
            // 
            // LastAskLabel
            // 
            this.LastAskLabel.AutoSize = true;
            this.LastAskLabel.Location = new System.Drawing.Point(96, 52);
            this.LastAskLabel.Name = "LastAskLabel";
            this.LastAskLabel.Size = new System.Drawing.Size(25, 13);
            this.LastAskLabel.TabIndex = 8;
            this.LastAskLabel.Text = "Ask";
            this.LastAskLabel.Click += new System.EventHandler(this.GetPriceBy_Click);
            // 
            // LastBidLabel
            // 
            this.LastBidLabel.AutoSize = true;
            this.LastBidLabel.Location = new System.Drawing.Point(96, 78);
            this.LastBidLabel.Name = "LastBidLabel";
            this.LastBidLabel.Size = new System.Drawing.Size(22, 13);
            this.LastBidLabel.TabIndex = 9;
            this.LastBidLabel.Text = "Bid";
            this.LastBidLabel.Click += new System.EventHandler(this.GetPriceBy_Click);
            // 
            // IPServerGroupBox
            // 
            this.IPServerGroupBox.Controls.Add(this.IPTextBox);
            this.IPServerGroupBox.Location = new System.Drawing.Point(12, 110);
            this.IPServerGroupBox.Name = "IPServerGroupBox";
            this.IPServerGroupBox.Size = new System.Drawing.Size(106, 39);
            this.IPServerGroupBox.TabIndex = 10;
            this.IPServerGroupBox.TabStop = false;
            this.IPServerGroupBox.Text = "IP Server";
            // 
            // IPTextBox
            // 
            this.IPTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.IPTextBox.Location = new System.Drawing.Point(3, 16);
            this.IPTextBox.Name = "IPTextBox";
            this.IPTextBox.Size = new System.Drawing.Size(100, 20);
            this.IPTextBox.TabIndex = 5;
            this.IPTextBox.Text = "82.204.220.34";
            // 
            // SymbolGroupBox
            // 
            this.SymbolGroupBox.Controls.Add(this.SymbolTextBox);
            this.SymbolGroupBox.Location = new System.Drawing.Point(124, 110);
            this.SymbolGroupBox.Name = "SymbolGroupBox";
            this.SymbolGroupBox.Size = new System.Drawing.Size(98, 39);
            this.SymbolGroupBox.TabIndex = 11;
            this.SymbolGroupBox.TabStop = false;
            this.SymbolGroupBox.Text = "Symbol";
            // 
            // SymbolTextBox
            // 
            this.SymbolTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.SymbolTextBox.Location = new System.Drawing.Point(3, 16);
            this.SymbolTextBox.Name = "SymbolTextBox";
            this.SymbolTextBox.Size = new System.Drawing.Size(92, 20);
            this.SymbolTextBox.TabIndex = 5;
            this.SymbolTextBox.Text = "SBRF-9.11_FT";
            // 
            // GetBarsButton
            // 
            this.GetBarsButton.Enabled = false;
            this.GetBarsButton.Location = new System.Drawing.Point(124, 185);
            this.GetBarsButton.Name = "GetBarsButton";
            this.GetBarsButton.Size = new System.Drawing.Size(98, 23);
            this.GetBarsButton.TabIndex = 12;
            this.GetBarsButton.Text = "Get Bars";
            this.GetBarsButton.UseVisualStyleBackColor = true;
            this.GetBarsButton.EnabledChanged += new System.EventHandler(this.GetBarsButton_EnabledChanged);
            this.GetBarsButton.Click += new System.EventHandler(this.GetBarsButton_Click);
            // 
            // ToDateTimePicker
            // 
            this.ToDateTimePicker.CustomFormat = "";
            this.ToDateTimePicker.Format = System.Windows.Forms.DateTimePickerFormat.Short;
            this.ToDateTimePicker.Location = new System.Drawing.Point(124, 216);
            this.ToDateTimePicker.MinDate = new System.DateTime(2008, 1, 1, 0, 0, 0, 0);
            this.ToDateTimePicker.Name = "ToDateTimePicker";
            this.ToDateTimePicker.Size = new System.Drawing.Size(98, 20);
            this.ToDateTimePicker.TabIndex = 13;
            // 
            // InfoBarLabel
            // 
            this.InfoBarLabel.AutoSize = true;
            this.InfoBarLabel.Location = new System.Drawing.Point(127, 152);
            this.InfoBarLabel.Name = "InfoBarLabel";
            this.InfoBarLabel.Size = new System.Drawing.Size(43, 13);
            this.InfoBarLabel.TabIndex = 14;
            this.InfoBarLabel.Text = "LastBar";
            // 
            // LastQuoteLabel
            // 
            this.LastQuoteLabel.AutoSize = true;
            this.LastQuoteLabel.Location = new System.Drawing.Point(96, 91);
            this.LastQuoteLabel.Name = "LastQuoteLabel";
            this.LastQuoteLabel.Size = new System.Drawing.Size(37, 13);
            this.LastQuoteLabel.TabIndex = 15;
            this.LastQuoteLabel.Text = "Status";
            // 
            // RightPanel
            // 
            this.RightPanel.Controls.Add(this.CanselAllButton);
            this.RightPanel.Controls.Add(this.OrderCanselMoveGroupBox);
            this.RightPanel.Controls.Add(this.PlaceOrderGroupBox);
            this.RightPanel.Controls.Add(this.PortfoliosComboBox);
            this.RightPanel.Controls.Add(this.ErrorLabel);
            this.RightPanel.Dock = System.Windows.Forms.DockStyle.Right;
            this.RightPanel.Enabled = false;
            this.RightPanel.Location = new System.Drawing.Point(255, 0);
            this.RightPanel.Name = "RightPanel";
            this.RightPanel.Size = new System.Drawing.Size(299, 251);
            this.RightPanel.TabIndex = 16;
            // 
            // CanselAllButton
            // 
            this.CanselAllButton.Location = new System.Drawing.Point(3, 102);
            this.CanselAllButton.Name = "CanselAllButton";
            this.CanselAllButton.Size = new System.Drawing.Size(96, 23);
            this.CanselAllButton.TabIndex = 11;
            this.CanselAllButton.Text = "Отменить все";
            this.CanselAllButton.UseVisualStyleBackColor = true;
            this.CanselAllButton.Click += new System.EventHandler(this.CanselAllButton_Click);
            // 
            // OrderCanselMoveGroupBox
            // 
            this.OrderCanselMoveGroupBox.Controls.Add(this.OrderPriceMoveNumericUpDown);
            this.OrderCanselMoveGroupBox.Controls.Add(this.OrderMoveButton);
            this.OrderCanselMoveGroupBox.Controls.Add(this.OrderCanselButton);
            this.OrderCanselMoveGroupBox.Controls.Add(this.OrderIdTextBox);
            this.OrderCanselMoveGroupBox.Controls.Add(this.OrderIdLabel);
            this.OrderCanselMoveGroupBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.OrderCanselMoveGroupBox.Location = new System.Drawing.Point(0, 60);
            this.OrderCanselMoveGroupBox.Name = "OrderCanselMoveGroupBox";
            this.OrderCanselMoveGroupBox.Size = new System.Drawing.Size(299, 39);
            this.OrderCanselMoveGroupBox.TabIndex = 10;
            this.OrderCanselMoveGroupBox.TabStop = false;
            this.OrderCanselMoveGroupBox.Text = "Place Order Limit";
            // 
            // OrderPriceMoveNumericUpDown
            // 
            this.OrderPriceMoveNumericUpDown.Dock = System.Windows.Forms.DockStyle.Left;
            this.OrderPriceMoveNumericUpDown.Location = new System.Drawing.Point(205, 16);
            this.OrderPriceMoveNumericUpDown.Maximum = new decimal(new int[] {
            -1486618624,
            232830643,
            0,
            0});
            this.OrderPriceMoveNumericUpDown.Name = "OrderPriceMoveNumericUpDown";
            this.OrderPriceMoveNumericUpDown.Size = new System.Drawing.Size(67, 20);
            this.OrderPriceMoveNumericUpDown.TabIndex = 5;
            this.OrderPriceMoveNumericUpDown.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            // 
            // OrderMoveButton
            // 
            this.OrderMoveButton.Dock = System.Windows.Forms.DockStyle.Left;
            this.OrderMoveButton.Location = new System.Drawing.Point(153, 16);
            this.OrderMoveButton.Name = "OrderMoveButton";
            this.OrderMoveButton.Size = new System.Drawing.Size(52, 20);
            this.OrderMoveButton.TabIndex = 3;
            this.OrderMoveButton.Text = "Move";
            this.OrderMoveButton.UseVisualStyleBackColor = true;
            this.OrderMoveButton.Click += new System.EventHandler(this.OrderMoveButton_Click);
            // 
            // OrderCanselButton
            // 
            this.OrderCanselButton.Dock = System.Windows.Forms.DockStyle.Left;
            this.OrderCanselButton.Location = new System.Drawing.Point(101, 16);
            this.OrderCanselButton.Name = "OrderCanselButton";
            this.OrderCanselButton.Size = new System.Drawing.Size(52, 20);
            this.OrderCanselButton.TabIndex = 2;
            this.OrderCanselButton.Text = "Cansel";
            this.OrderCanselButton.UseVisualStyleBackColor = true;
            this.OrderCanselButton.Click += new System.EventHandler(this.OrderCanselButton_Click);
            // 
            // OrderIdTextBox
            // 
            this.OrderIdTextBox.Dock = System.Windows.Forms.DockStyle.Left;
            this.OrderIdTextBox.Location = new System.Drawing.Point(21, 16);
            this.OrderIdTextBox.Name = "OrderIdTextBox";
            this.OrderIdTextBox.Size = new System.Drawing.Size(80, 20);
            this.OrderIdTextBox.TabIndex = 6;
            // 
            // OrderIdLabel
            // 
            this.OrderIdLabel.Dock = System.Windows.Forms.DockStyle.Left;
            this.OrderIdLabel.Location = new System.Drawing.Point(3, 16);
            this.OrderIdLabel.Name = "OrderIdLabel";
            this.OrderIdLabel.Size = new System.Drawing.Size(18, 20);
            this.OrderIdLabel.TabIndex = 0;
            this.OrderIdLabel.Text = "Id";
            this.OrderIdLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // PlaceOrderGroupBox
            // 
            this.PlaceOrderGroupBox.Controls.Add(this.OrderCookieLabel);
            this.PlaceOrderGroupBox.Controls.Add(this.OrderSellButton);
            this.PlaceOrderGroupBox.Controls.Add(this.OrderBuyButton);
            this.PlaceOrderGroupBox.Controls.Add(this.OrderPriceNumericUpDown);
            this.PlaceOrderGroupBox.Controls.Add(this.OrderPriceLabel);
            this.PlaceOrderGroupBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.PlaceOrderGroupBox.Location = new System.Drawing.Point(0, 21);
            this.PlaceOrderGroupBox.Name = "PlaceOrderGroupBox";
            this.PlaceOrderGroupBox.Size = new System.Drawing.Size(299, 39);
            this.PlaceOrderGroupBox.TabIndex = 8;
            this.PlaceOrderGroupBox.TabStop = false;
            this.PlaceOrderGroupBox.Text = "Place Order Limit";
            // 
            // OrderCookieLabel
            // 
            this.OrderCookieLabel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.OrderCookieLabel.Location = new System.Drawing.Point(206, 16);
            this.OrderCookieLabel.Name = "OrderCookieLabel";
            this.OrderCookieLabel.Size = new System.Drawing.Size(90, 20);
            this.OrderCookieLabel.TabIndex = 4;
            this.OrderCookieLabel.Text = "Cookie";
            this.OrderCookieLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // OrderSellButton
            // 
            this.OrderSellButton.Dock = System.Windows.Forms.DockStyle.Left;
            this.OrderSellButton.Location = new System.Drawing.Point(154, 16);
            this.OrderSellButton.Name = "OrderSellButton";
            this.OrderSellButton.Size = new System.Drawing.Size(52, 20);
            this.OrderSellButton.TabIndex = 3;
            this.OrderSellButton.Text = "Sell";
            this.OrderSellButton.UseVisualStyleBackColor = true;
            this.OrderSellButton.Click += new System.EventHandler(this.PlaceOrder_Click);
            // 
            // OrderBuyButton
            // 
            this.OrderBuyButton.Dock = System.Windows.Forms.DockStyle.Left;
            this.OrderBuyButton.Location = new System.Drawing.Point(102, 16);
            this.OrderBuyButton.Name = "OrderBuyButton";
            this.OrderBuyButton.Size = new System.Drawing.Size(52, 20);
            this.OrderBuyButton.TabIndex = 2;
            this.OrderBuyButton.Text = "Buy";
            this.OrderBuyButton.UseVisualStyleBackColor = true;
            this.OrderBuyButton.Click += new System.EventHandler(this.PlaceOrder_Click);
            // 
            // OrderPriceNumericUpDown
            // 
            this.OrderPriceNumericUpDown.Dock = System.Windows.Forms.DockStyle.Left;
            this.OrderPriceNumericUpDown.Location = new System.Drawing.Point(35, 16);
            this.OrderPriceNumericUpDown.Maximum = new decimal(new int[] {
            -1486618624,
            232830643,
            0,
            0});
            this.OrderPriceNumericUpDown.Name = "OrderPriceNumericUpDown";
            this.OrderPriceNumericUpDown.Size = new System.Drawing.Size(67, 20);
            this.OrderPriceNumericUpDown.TabIndex = 5;
            this.OrderPriceNumericUpDown.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            // 
            // OrderPriceLabel
            // 
            this.OrderPriceLabel.Dock = System.Windows.Forms.DockStyle.Left;
            this.OrderPriceLabel.Location = new System.Drawing.Point(3, 16);
            this.OrderPriceLabel.Name = "OrderPriceLabel";
            this.OrderPriceLabel.Size = new System.Drawing.Size(32, 20);
            this.OrderPriceLabel.TabIndex = 0;
            this.OrderPriceLabel.Text = "Price";
            this.OrderPriceLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.OrderPriceLabel.Click += new System.EventHandler(this.GetPriceBy_Click);
            // 
            // PortfoliosComboBox
            // 
            this.PortfoliosComboBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.PortfoliosComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.PortfoliosComboBox.FormattingEnabled = true;
            this.PortfoliosComboBox.Location = new System.Drawing.Point(0, 0);
            this.PortfoliosComboBox.MaxDropDownItems = 12;
            this.PortfoliosComboBox.Name = "PortfoliosComboBox";
            this.PortfoliosComboBox.Size = new System.Drawing.Size(299, 21);
            this.PortfoliosComboBox.TabIndex = 9;
            // 
            // ErrorLabel
            // 
            this.ErrorLabel.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.ErrorLabel.Location = new System.Drawing.Point(0, 128);
            this.ErrorLabel.Name = "ErrorLabel";
            this.ErrorLabel.Size = new System.Drawing.Size(299, 123);
            this.ErrorLabel.TabIndex = 7;
            // 
            // TestForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(554, 251);
            this.Controls.Add(this.RightPanel);
            this.Controls.Add(this.LastQuoteLabel);
            this.Controls.Add(this.InfoBarLabel);
            this.Controls.Add(this.ToDateTimePicker);
            this.Controls.Add(this.GetBarsButton);
            this.Controls.Add(this.SymbolGroupBox);
            this.Controls.Add(this.LastBidLabel);
            this.Controls.Add(this.IPServerGroupBox);
            this.Controls.Add(this.LastAskLabel);
            this.Controls.Add(this.LastLabel);
            this.Controls.Add(this.PasswordGroupBox);
            this.Controls.Add(this.LoginGroupBox);
            this.Controls.Add(this.StatusGroupBox);
            this.Controls.Add(this.DisconnectButton);
            this.Controls.Add(this.ConnectButton);
            this.Controls.Add(this.CreateButton);
            this.Name = "TestForm";
            this.Text = "SmartCOM Test";
            this.StatusGroupBox.ResumeLayout(false);
            this.LoginGroupBox.ResumeLayout(false);
            this.LoginGroupBox.PerformLayout();
            this.PasswordGroupBox.ResumeLayout(false);
            this.PasswordGroupBox.PerformLayout();
            this.IPServerGroupBox.ResumeLayout(false);
            this.IPServerGroupBox.PerformLayout();
            this.SymbolGroupBox.ResumeLayout(false);
            this.SymbolGroupBox.PerformLayout();
            this.RightPanel.ResumeLayout(false);
            this.OrderCanselMoveGroupBox.ResumeLayout(false);
            this.OrderCanselMoveGroupBox.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.OrderPriceMoveNumericUpDown)).EndInit();
            this.PlaceOrderGroupBox.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.OrderPriceNumericUpDown)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button CreateButton;
        private System.Windows.Forms.Button ConnectButton;
        private System.Windows.Forms.Button DisconnectButton;
        private System.Windows.Forms.GroupBox StatusGroupBox;
        private System.Windows.Forms.Label StatusLabel;
        private System.Windows.Forms.GroupBox LoginGroupBox;
        private System.Windows.Forms.TextBox LoginTextBox;
        private System.Windows.Forms.GroupBox PasswordGroupBox;
        private System.Windows.Forms.TextBox PasswordTextBox;
        private System.Windows.Forms.Label LastLabel;
        private System.Windows.Forms.Label LastAskLabel;
        private System.Windows.Forms.Label LastBidLabel;
        private System.Windows.Forms.GroupBox IPServerGroupBox;
        private System.Windows.Forms.TextBox IPTextBox;
        private System.Windows.Forms.GroupBox SymbolGroupBox;
        private System.Windows.Forms.TextBox SymbolTextBox;
        private System.Windows.Forms.Button GetBarsButton;
        private System.Windows.Forms.DateTimePicker ToDateTimePicker;
        private System.Windows.Forms.Label InfoBarLabel;
        private System.Windows.Forms.Label LastQuoteLabel;
        private System.Windows.Forms.Panel RightPanel;
        private System.Windows.Forms.GroupBox PlaceOrderGroupBox;
        private System.Windows.Forms.Button OrderSellButton;
        private System.Windows.Forms.Button OrderBuyButton;
        private System.Windows.Forms.Label OrderPriceLabel;
        private System.Windows.Forms.Label ErrorLabel;
        private System.Windows.Forms.Label OrderCookieLabel;
        private System.Windows.Forms.NumericUpDown OrderPriceNumericUpDown;
        private System.Windows.Forms.ComboBox PortfoliosComboBox;
        private System.Windows.Forms.GroupBox OrderCanselMoveGroupBox;
        private System.Windows.Forms.NumericUpDown OrderPriceMoveNumericUpDown;
        private System.Windows.Forms.Button OrderMoveButton;
        private System.Windows.Forms.Button OrderCanselButton;
        private System.Windows.Forms.TextBox OrderIdTextBox;
        private System.Windows.Forms.Label OrderIdLabel;
        private System.Windows.Forms.Button CanselAllButton;
    }
}

