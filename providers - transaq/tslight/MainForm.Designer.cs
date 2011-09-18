/*
 * User: Korenev
 * Date: 20.09.2010
 * Time: 12:06
 * 
 */
namespace tslight
{
	partial class MainForm
	{
		/// <summary>
		/// Designer variable used to keep track of non-visual components.
		/// </summary>
		private System.ComponentModel.IContainer components = null;
		
		/// <summary>
		/// Disposes resources used by the form.
		/// </summary>
		/// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
		protected override void Dispose(bool disposing)
		{
			if (disposing) {
				if (components != null) {
					components.Dispose();
				}
			}
			base.Dispose(disposing);
		}
		
		/// <summary>
		/// This method is required for Windows Forms designer support.
		/// Do not change the method contents inside the source code editor. The Forms designer might
		/// not be able to load this method if it was changed manually.
		/// </summary>
		private void InitializeComponent()
		{
			this.components = new System.ComponentModel.Container();
			this.btn_Connect = new System.Windows.Forms.Button();
			this.txt_Status = new System.Windows.Forms.Label();
			this.panel1 = new System.Windows.Forms.Panel();
			this.txt_Connect = new System.Windows.Forms.Label();
			this.ctl_Connect = new System.Windows.Forms.Panel();
			this.btn_Test = new System.Windows.Forms.Button();
			this.timer_Handler = new System.Windows.Forms.Timer(this.components);
			this.txt_Info = new System.Windows.Forms.TextBox();
			this.ctl_Tabs = new System.Windows.Forms.TabControl();
			this.tab_Chart = new System.Windows.Forms.TabPage();
			this.ctl_Chart = new System.Windows.Forms.Panel();
			this.tab_Security = new System.Windows.Forms.TabPage();
			this.txt_Selected_Code = new System.Windows.Forms.TextBox();
			this.btn_Security_Select = new System.Windows.Forms.Button();
			this.dg_Security = new System.Windows.Forms.DataGridView();
			this.security_id = new System.Windows.Forms.DataGridViewTextBoxColumn();
			this.security_name = new System.Windows.Forms.DataGridViewTextBoxColumn();
			this.security_code = new System.Windows.Forms.DataGridViewTextBoxColumn();
			this.lotsize = new System.Windows.Forms.DataGridViewTextBoxColumn();
			this.tab_Param = new System.Windows.Forms.TabPage();
			this.lbl_Port = new System.Windows.Forms.Label();
			this.edt_Port = new System.Windows.Forms.TextBox();
			this.lbl_Client = new System.Windows.Forms.Label();
			this.txt_Client = new System.Windows.Forms.TextBox();
			this.lbl_IP = new System.Windows.Forms.Label();
			this.edt_IP = new System.Windows.Forms.TextBox();
			this.lbl_Password = new System.Windows.Forms.Label();
			this.edt_Password = new System.Windows.Forms.TextBox();
			this.lbl_Login = new System.Windows.Forms.Label();
			this.edt_Login = new System.Windows.Forms.TextBox();
			this.lbl_Info = new System.Windows.Forms.Label();
			this.lbl_Timeframe = new System.Windows.Forms.Label();
			this.lst_Timeframe = new System.Windows.Forms.ComboBox();
			this.btn_Start = new System.Windows.Forms.Button();
			this.edt_Security_Code = new System.Windows.Forms.TextBox();
			this.lbl_Security = new System.Windows.Forms.Label();
			this.lbl_Long = new System.Windows.Forms.Label();
			this.lbl_Entry = new System.Windows.Forms.Label();
			this.lbl_Exit = new System.Windows.Forms.Label();
			this.lbl_Short = new System.Windows.Forms.Label();
			this.txt_LongEntry = new System.Windows.Forms.Label();
			this.txt_LongExit = new System.Windows.Forms.Label();
			this.txt_ShortExit = new System.Windows.Forms.Label();
			this.txt_ShortEntry = new System.Windows.Forms.Label();
			this.edt_Position_Limit = new System.Windows.Forms.TextBox();
			this.label1 = new System.Windows.Forms.Label();
			this.btn_Buy = new System.Windows.Forms.Button();
			this.btn_Sell = new System.Windows.Forms.Button();
			this.panel2 = new System.Windows.Forms.Panel();
			this.ctl_Tabs.SuspendLayout();
			this.tab_Chart.SuspendLayout();
			this.tab_Security.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this.dg_Security)).BeginInit();
			this.tab_Param.SuspendLayout();
			this.SuspendLayout();
			// 
			// btn_Connect
			// 
			this.btn_Connect.Location = new System.Drawing.Point(692, 479);
			this.btn_Connect.Name = "btn_Connect";
			this.btn_Connect.Size = new System.Drawing.Size(80, 22);
			this.btn_Connect.TabIndex = 0;
			this.btn_Connect.Text = "Подключить";
			this.btn_Connect.UseVisualStyleBackColor = true;
			this.btn_Connect.Click += new System.EventHandler(this.btn_Connect_Click);
			// 
			// txt_Status
			// 
			this.txt_Status.Location = new System.Drawing.Point(19, 483);
			this.txt_Status.Name = "txt_Status";
			this.txt_Status.Size = new System.Drawing.Size(596, 14);
			this.txt_Status.TabIndex = 1;
			// 
			// panel1
			// 
			this.panel1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.panel1.Location = new System.Drawing.Point(15, 479);
			this.panel1.Name = "panel1";
			this.panel1.Size = new System.Drawing.Size(605, 22);
			this.panel1.TabIndex = 2;
			// 
			// txt_Connect
			// 
			this.txt_Connect.BackColor = System.Drawing.SystemColors.ControlLight;
			this.txt_Connect.ForeColor = System.Drawing.Color.Black;
			this.txt_Connect.Location = new System.Drawing.Point(632, 483);
			this.txt_Connect.Name = "txt_Connect";
			this.txt_Connect.Size = new System.Drawing.Size(49, 14);
			this.txt_Connect.TabIndex = 3;
			this.txt_Connect.Text = "offline";
			this.txt_Connect.TextAlign = System.Drawing.ContentAlignment.TopCenter;
			// 
			// ctl_Connect
			// 
			this.ctl_Connect.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.ctl_Connect.Location = new System.Drawing.Point(626, 479);
			this.ctl_Connect.Name = "ctl_Connect";
			this.ctl_Connect.Size = new System.Drawing.Size(59, 22);
			this.ctl_Connect.TabIndex = 4;
			// 
			// btn_Test
			// 
			this.btn_Test.Location = new System.Drawing.Point(573, 272);
			this.btn_Test.Name = "btn_Test";
			this.btn_Test.Size = new System.Drawing.Size(80, 22);
			this.btn_Test.TabIndex = 5;
			this.btn_Test.Text = "Test";
			this.btn_Test.UseVisualStyleBackColor = true;
			this.btn_Test.Click += new System.EventHandler(this.btn_Test_Click);
			// 
			// timer_Handler
			// 
			this.timer_Handler.Enabled = true;
			this.timer_Handler.Tick += new System.EventHandler(this.Timer_Handler_Tick);
			// 
			// txt_Info
			// 
			this.txt_Info.Location = new System.Drawing.Point(6, 243);
			this.txt_Info.Multiline = true;
			this.txt_Info.Name = "txt_Info";
			this.txt_Info.ReadOnly = true;
			this.txt_Info.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
			this.txt_Info.Size = new System.Drawing.Size(519, 186);
			this.txt_Info.TabIndex = 6;
			// 
			// ctl_Tabs
			// 
			this.ctl_Tabs.Controls.Add(this.tab_Chart);
			this.ctl_Tabs.Controls.Add(this.tab_Security);
			this.ctl_Tabs.Controls.Add(this.tab_Param);
			this.ctl_Tabs.Location = new System.Drawing.Point(15, 12);
			this.ctl_Tabs.Name = "ctl_Tabs";
			this.ctl_Tabs.SelectedIndex = 0;
			this.ctl_Tabs.Size = new System.Drawing.Size(539, 461);
			this.ctl_Tabs.TabIndex = 7;
			// 
			// tab_Chart
			// 
			this.tab_Chart.Controls.Add(this.ctl_Chart);
			this.tab_Chart.Location = new System.Drawing.Point(4, 22);
			this.tab_Chart.Name = "tab_Chart";
			this.tab_Chart.Padding = new System.Windows.Forms.Padding(3);
			this.tab_Chart.Size = new System.Drawing.Size(531, 435);
			this.tab_Chart.TabIndex = 0;
			this.tab_Chart.Text = "График";
			this.tab_Chart.UseVisualStyleBackColor = true;
			// 
			// ctl_Chart
			// 
			this.ctl_Chart.Location = new System.Drawing.Point(6, 6);
			this.ctl_Chart.Name = "ctl_Chart";
			this.ctl_Chart.Size = new System.Drawing.Size(519, 423);
			this.ctl_Chart.TabIndex = 0;
			// 
			// tab_Security
			// 
			this.tab_Security.Controls.Add(this.txt_Selected_Code);
			this.tab_Security.Controls.Add(this.btn_Security_Select);
			this.tab_Security.Controls.Add(this.dg_Security);
			this.tab_Security.Location = new System.Drawing.Point(4, 22);
			this.tab_Security.Name = "tab_Security";
			this.tab_Security.Padding = new System.Windows.Forms.Padding(3);
			this.tab_Security.Size = new System.Drawing.Size(531, 435);
			this.tab_Security.TabIndex = 1;
			this.tab_Security.Text = "Инструменты";
			this.tab_Security.UseVisualStyleBackColor = true;
			// 
			// txt_Selected_Code
			// 
			this.txt_Selected_Code.Location = new System.Drawing.Point(237, 408);
			this.txt_Selected_Code.Name = "txt_Selected_Code";
			this.txt_Selected_Code.Size = new System.Drawing.Size(100, 20);
			this.txt_Selected_Code.TabIndex = 9;
			// 
			// btn_Security_Select
			// 
			this.btn_Security_Select.Location = new System.Drawing.Point(145, 407);
			this.btn_Security_Select.Name = "btn_Security_Select";
			this.btn_Security_Select.Size = new System.Drawing.Size(75, 22);
			this.btn_Security_Select.TabIndex = 8;
			this.btn_Security_Select.Text = "Выбрать";
			this.btn_Security_Select.UseVisualStyleBackColor = true;
			this.btn_Security_Select.Click += new System.EventHandler(this.btn_Security_Select_Click);
			// 
			// dg_Security
			// 
			this.dg_Security.AllowDrop = true;
			this.dg_Security.AllowUserToAddRows = false;
			this.dg_Security.AllowUserToDeleteRows = false;
			this.dg_Security.AllowUserToResizeRows = false;
			this.dg_Security.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
			this.dg_Security.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
									this.security_id,
									this.security_name,
									this.security_code,
									this.lotsize});
			this.dg_Security.Location = new System.Drawing.Point(17, 18);
			this.dg_Security.MultiSelect = false;
			this.dg_Security.Name = "dg_Security";
			this.dg_Security.RowHeadersWidth = 20;
			this.dg_Security.RowTemplate.Height = 18;
			this.dg_Security.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect;
			this.dg_Security.Size = new System.Drawing.Size(388, 380);
			this.dg_Security.TabIndex = 0;
			this.dg_Security.SelectionChanged += new System.EventHandler(this.dg_Security_SelectionChanged);
			// 
			// security_id
			// 
			this.security_id.DataPropertyName = "security_id";
			this.security_id.Frozen = true;
			this.security_id.HeaderText = "id";
			this.security_id.MinimumWidth = 50;
			this.security_id.Name = "security_id";
			this.security_id.ReadOnly = true;
			this.security_id.Width = 50;
			// 
			// security_name
			// 
			this.security_name.DataPropertyName = "security_name";
			this.security_name.HeaderText = "Название";
			this.security_name.MinimumWidth = 100;
			this.security_name.Name = "security_name";
			this.security_name.ReadOnly = true;
			this.security_name.Width = 150;
			// 
			// security_code
			// 
			this.security_code.DataPropertyName = "security_code";
			this.security_code.HeaderText = "Код";
			this.security_code.MinimumWidth = 50;
			this.security_code.Name = "security_code";
			this.security_code.ReadOnly = true;
			// 
			// lotsize
			// 
			this.lotsize.DataPropertyName = "lotsize";
			this.lotsize.HeaderText = "Лот";
			this.lotsize.MinimumWidth = 30;
			this.lotsize.Name = "lotsize";
			this.lotsize.ReadOnly = true;
			this.lotsize.Width = 50;
			// 
			// tab_Param
			// 
			this.tab_Param.Controls.Add(this.lbl_Port);
			this.tab_Param.Controls.Add(this.edt_Port);
			this.tab_Param.Controls.Add(this.lbl_Client);
			this.tab_Param.Controls.Add(this.txt_Client);
			this.tab_Param.Controls.Add(this.lbl_IP);
			this.tab_Param.Controls.Add(this.edt_IP);
			this.tab_Param.Controls.Add(this.lbl_Password);
			this.tab_Param.Controls.Add(this.edt_Password);
			this.tab_Param.Controls.Add(this.lbl_Login);
			this.tab_Param.Controls.Add(this.edt_Login);
			this.tab_Param.Controls.Add(this.lbl_Info);
			this.tab_Param.Controls.Add(this.txt_Info);
			this.tab_Param.Location = new System.Drawing.Point(4, 22);
			this.tab_Param.Name = "tab_Param";
			this.tab_Param.Padding = new System.Windows.Forms.Padding(3);
			this.tab_Param.Size = new System.Drawing.Size(531, 435);
			this.tab_Param.TabIndex = 2;
			this.tab_Param.Text = "Параметры";
			this.tab_Param.UseVisualStyleBackColor = true;
			// 
			// lbl_Port
			// 
			this.lbl_Port.Location = new System.Drawing.Point(22, 118);
			this.lbl_Port.Name = "lbl_Port";
			this.lbl_Port.Size = new System.Drawing.Size(100, 14);
			this.lbl_Port.TabIndex = 17;
			this.lbl_Port.Text = "Порт";
			// 
			// edt_Port
			// 
			this.edt_Port.Location = new System.Drawing.Point(135, 115);
			this.edt_Port.Name = "edt_Port";
			this.edt_Port.Size = new System.Drawing.Size(122, 20);
			this.edt_Port.TabIndex = 16;
			// 
			// lbl_Client
			// 
			this.lbl_Client.Location = new System.Drawing.Point(22, 144);
			this.lbl_Client.Name = "lbl_Client";
			this.lbl_Client.Size = new System.Drawing.Size(100, 14);
			this.lbl_Client.TabIndex = 15;
			this.lbl_Client.Text = "Код клиента";
			// 
			// txt_Client
			// 
			this.txt_Client.Location = new System.Drawing.Point(135, 141);
			this.txt_Client.Name = "txt_Client";
			this.txt_Client.ReadOnly = true;
			this.txt_Client.Size = new System.Drawing.Size(122, 20);
			this.txt_Client.TabIndex = 14;
			// 
			// lbl_IP
			// 
			this.lbl_IP.Location = new System.Drawing.Point(22, 92);
			this.lbl_IP.Name = "lbl_IP";
			this.lbl_IP.Size = new System.Drawing.Size(100, 14);
			this.lbl_IP.TabIndex = 13;
			this.lbl_IP.Text = "IP адрес";
			// 
			// edt_IP
			// 
			this.edt_IP.Location = new System.Drawing.Point(135, 89);
			this.edt_IP.Name = "edt_IP";
			this.edt_IP.Size = new System.Drawing.Size(122, 20);
			this.edt_IP.TabIndex = 12;
			// 
			// lbl_Password
			// 
			this.lbl_Password.Location = new System.Drawing.Point(22, 66);
			this.lbl_Password.Name = "lbl_Password";
			this.lbl_Password.Size = new System.Drawing.Size(100, 14);
			this.lbl_Password.TabIndex = 11;
			this.lbl_Password.Text = "Пароль";
			// 
			// edt_Password
			// 
			this.edt_Password.Location = new System.Drawing.Point(135, 63);
			this.edt_Password.Name = "edt_Password";
			this.edt_Password.Size = new System.Drawing.Size(122, 20);
			this.edt_Password.TabIndex = 10;
			this.edt_Password.UseSystemPasswordChar = true;
			// 
			// lbl_Login
			// 
			this.lbl_Login.Location = new System.Drawing.Point(22, 40);
			this.lbl_Login.Name = "lbl_Login";
			this.lbl_Login.Size = new System.Drawing.Size(100, 14);
			this.lbl_Login.TabIndex = 9;
			this.lbl_Login.Text = "Логин";
			// 
			// edt_Login
			// 
			this.edt_Login.Location = new System.Drawing.Point(135, 37);
			this.edt_Login.Name = "edt_Login";
			this.edt_Login.Size = new System.Drawing.Size(122, 20);
			this.edt_Login.TabIndex = 8;
			// 
			// lbl_Info
			// 
			this.lbl_Info.Location = new System.Drawing.Point(6, 225);
			this.lbl_Info.Name = "lbl_Info";
			this.lbl_Info.Size = new System.Drawing.Size(198, 15);
			this.lbl_Info.TabIndex = 7;
			this.lbl_Info.Text = "Дополнительные сведения (Info)";
			// 
			// lbl_Timeframe
			// 
			this.lbl_Timeframe.Location = new System.Drawing.Point(573, 65);
			this.lbl_Timeframe.Name = "lbl_Timeframe";
			this.lbl_Timeframe.Size = new System.Drawing.Size(87, 15);
			this.lbl_Timeframe.TabIndex = 5;
			this.lbl_Timeframe.Text = "Таймфрэйм";
			// 
			// lst_Timeframe
			// 
			this.lst_Timeframe.FormattingEnabled = true;
			this.lst_Timeframe.Location = new System.Drawing.Point(672, 62);
			this.lst_Timeframe.Name = "lst_Timeframe";
			this.lst_Timeframe.Size = new System.Drawing.Size(100, 21);
			this.lst_Timeframe.TabIndex = 4;
			this.lst_Timeframe.SelectedIndexChanged += new System.EventHandler(this.lst_Timeframe_SelectedIndexChanged);
			// 
			// btn_Start
			// 
			this.btn_Start.Location = new System.Drawing.Point(692, 272);
			this.btn_Start.Name = "btn_Start";
			this.btn_Start.Size = new System.Drawing.Size(80, 22);
			this.btn_Start.TabIndex = 3;
			this.btn_Start.Text = "Старт";
			this.btn_Start.UseVisualStyleBackColor = true;
			this.btn_Start.Click += new System.EventHandler(this.btn_Start_Click);
			// 
			// edt_Security_Code
			// 
			this.edt_Security_Code.Location = new System.Drawing.Point(672, 34);
			this.edt_Security_Code.Name = "edt_Security_Code";
			this.edt_Security_Code.Size = new System.Drawing.Size(100, 20);
			this.edt_Security_Code.TabIndex = 2;
			// 
			// lbl_Security
			// 
			this.lbl_Security.Location = new System.Drawing.Point(573, 36);
			this.lbl_Security.Name = "lbl_Security";
			this.lbl_Security.Size = new System.Drawing.Size(95, 15);
			this.lbl_Security.TabIndex = 1;
			this.lbl_Security.Text = "Инструмент (код)";
			// 
			// lbl_Long
			// 
			this.lbl_Long.Location = new System.Drawing.Point(627, 144);
			this.lbl_Long.Name = "lbl_Long";
			this.lbl_Long.Size = new System.Drawing.Size(40, 14);
			this.lbl_Long.TabIndex = 9;
			this.lbl_Long.Text = "Long";
			// 
			// lbl_Entry
			// 
			this.lbl_Entry.Location = new System.Drawing.Point(673, 124);
			this.lbl_Entry.Name = "lbl_Entry";
			this.lbl_Entry.Size = new System.Drawing.Size(40, 14);
			this.lbl_Entry.TabIndex = 11;
			this.lbl_Entry.Text = "Entry";
			this.lbl_Entry.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			// 
			// lbl_Exit
			// 
			this.lbl_Exit.Location = new System.Drawing.Point(721, 124);
			this.lbl_Exit.Name = "lbl_Exit";
			this.lbl_Exit.Size = new System.Drawing.Size(40, 14);
			this.lbl_Exit.TabIndex = 12;
			this.lbl_Exit.Text = "Exit";
			this.lbl_Exit.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			// 
			// lbl_Short
			// 
			this.lbl_Short.Location = new System.Drawing.Point(627, 170);
			this.lbl_Short.Name = "lbl_Short";
			this.lbl_Short.Size = new System.Drawing.Size(40, 14);
			this.lbl_Short.TabIndex = 14;
			this.lbl_Short.Text = "Short";
			// 
			// txt_LongEntry
			// 
			this.txt_LongEntry.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.txt_LongEntry.Location = new System.Drawing.Point(673, 141);
			this.txt_LongEntry.Name = "txt_LongEntry";
			this.txt_LongEntry.Size = new System.Drawing.Size(40, 20);
			this.txt_LongEntry.TabIndex = 16;
			this.txt_LongEntry.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			// 
			// txt_LongExit
			// 
			this.txt_LongExit.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.txt_LongExit.Location = new System.Drawing.Point(721, 141);
			this.txt_LongExit.Name = "txt_LongExit";
			this.txt_LongExit.Size = new System.Drawing.Size(40, 20);
			this.txt_LongExit.TabIndex = 17;
			this.txt_LongExit.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			// 
			// txt_ShortExit
			// 
			this.txt_ShortExit.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.txt_ShortExit.Location = new System.Drawing.Point(721, 167);
			this.txt_ShortExit.Name = "txt_ShortExit";
			this.txt_ShortExit.Size = new System.Drawing.Size(40, 20);
			this.txt_ShortExit.TabIndex = 19;
			this.txt_ShortExit.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			// 
			// txt_ShortEntry
			// 
			this.txt_ShortEntry.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.txt_ShortEntry.Location = new System.Drawing.Point(673, 167);
			this.txt_ShortEntry.Name = "txt_ShortEntry";
			this.txt_ShortEntry.Size = new System.Drawing.Size(40, 20);
			this.txt_ShortEntry.TabIndex = 18;
			this.txt_ShortEntry.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			// 
			// edt_Position_Limit
			// 
			this.edt_Position_Limit.Location = new System.Drawing.Point(672, 89);
			this.edt_Position_Limit.Name = "edt_Position_Limit";
			this.edt_Position_Limit.ReadOnly = true;
			this.edt_Position_Limit.Size = new System.Drawing.Size(100, 20);
			this.edt_Position_Limit.TabIndex = 21;
			// 
			// label1
			// 
			this.label1.Location = new System.Drawing.Point(573, 91);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(95, 15);
			this.label1.TabIndex = 20;
			this.label1.Text = "Лимит (лотов)";
			// 
			// btn_Buy
			// 
			this.btn_Buy.Location = new System.Drawing.Point(589, 225);
			this.btn_Buy.Name = "btn_Buy";
			this.btn_Buy.Size = new System.Drawing.Size(75, 23);
			this.btn_Buy.TabIndex = 22;
			this.btn_Buy.Text = "Buy";
			this.btn_Buy.UseVisualStyleBackColor = true;
			this.btn_Buy.Click += new System.EventHandler(this.btn_Buy_Click);
			// 
			// btn_Sell
			// 
			this.btn_Sell.Location = new System.Drawing.Point(679, 225);
			this.btn_Sell.Name = "btn_Sell";
			this.btn_Sell.Size = new System.Drawing.Size(75, 23);
			this.btn_Sell.TabIndex = 23;
			this.btn_Sell.Text = "Sell";
			this.btn_Sell.UseVisualStyleBackColor = true;
			this.btn_Sell.Click += new System.EventHandler(this.btn_Sell_Click);
			// 
			// panel2
			// 
			this.panel2.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.panel2.Location = new System.Drawing.Point(573, 216);
			this.panel2.Name = "panel2";
			this.panel2.Size = new System.Drawing.Size(199, 41);
			this.panel2.TabIndex = 24;
			// 
			// MainForm
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size(787, 515);
			this.Controls.Add(this.btn_Sell);
			this.Controls.Add(this.btn_Buy);
			this.Controls.Add(this.edt_Position_Limit);
			this.Controls.Add(this.label1);
			this.Controls.Add(this.txt_ShortExit);
			this.Controls.Add(this.txt_ShortEntry);
			this.Controls.Add(this.txt_LongExit);
			this.Controls.Add(this.txt_LongEntry);
			this.Controls.Add(this.lbl_Short);
			this.Controls.Add(this.lbl_Exit);
			this.Controls.Add(this.lbl_Entry);
			this.Controls.Add(this.lbl_Long);
			this.Controls.Add(this.btn_Start);
			this.Controls.Add(this.lbl_Timeframe);
			this.Controls.Add(this.ctl_Tabs);
			this.Controls.Add(this.lst_Timeframe);
			this.Controls.Add(this.btn_Test);
			this.Controls.Add(this.txt_Connect);
			this.Controls.Add(this.edt_Security_Code);
			this.Controls.Add(this.txt_Status);
			this.Controls.Add(this.lbl_Security);
			this.Controls.Add(this.btn_Connect);
			this.Controls.Add(this.panel1);
			this.Controls.Add(this.ctl_Connect);
			this.Controls.Add(this.panel2);
			this.MaximizeBox = false;
			this.Name = "MainForm";
			this.Text = "TS light";
			this.Load += new System.EventHandler(this.MainFormLoad);
			this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.MainFormFormClosing);
			this.ctl_Tabs.ResumeLayout(false);
			this.tab_Chart.ResumeLayout(false);
			this.tab_Security.ResumeLayout(false);
			this.tab_Security.PerformLayout();
			((System.ComponentModel.ISupportInitialize)(this.dg_Security)).EndInit();
			this.tab_Param.ResumeLayout(false);
			this.tab_Param.PerformLayout();
			this.ResumeLayout(false);
			this.PerformLayout();
		}
		private System.Windows.Forms.TextBox edt_Port;
		private System.Windows.Forms.Label lbl_Port;
		private System.Windows.Forms.TextBox edt_IP;
		private System.Windows.Forms.TextBox edt_Password;
		private System.Windows.Forms.TextBox edt_Login;
		private System.Windows.Forms.Panel panel2;
		private System.Windows.Forms.Button btn_Sell;
		private System.Windows.Forms.Button btn_Buy;
		private System.Windows.Forms.TextBox edt_Position_Limit;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.TextBox txt_Client;
		private System.Windows.Forms.Label lbl_Client;
		private System.Windows.Forms.Label lbl_Login;
		private System.Windows.Forms.Label lbl_Password;
		private System.Windows.Forms.Label lbl_IP;
		private System.Windows.Forms.Label lbl_Info;
		private System.Windows.Forms.Label txt_ShortEntry;
		private System.Windows.Forms.Label lbl_Short;
		private System.Windows.Forms.Label txt_ShortExit;
		private System.Windows.Forms.Label lbl_Exit;
		private System.Windows.Forms.Label lbl_Entry;
		private System.Windows.Forms.Label txt_LongExit;
		private System.Windows.Forms.Label lbl_Long;
		private System.Windows.Forms.Label txt_LongEntry;
		private System.Windows.Forms.Panel ctl_Chart;
		private System.Windows.Forms.TabPage tab_Chart;
		private System.Windows.Forms.Label lbl_Timeframe;
		private System.Windows.Forms.ComboBox lst_Timeframe;
		private System.Windows.Forms.TextBox txt_Selected_Code;
		private System.Windows.Forms.TextBox edt_Security_Code;
		private System.Windows.Forms.Button btn_Start;
		private System.Windows.Forms.Button btn_Security_Select;
		private System.Windows.Forms.Label lbl_Security;
		private System.Windows.Forms.DataGridViewTextBoxColumn lotsize;
		private System.Windows.Forms.DataGridViewTextBoxColumn security_code;
		private System.Windows.Forms.DataGridViewTextBoxColumn security_name;
		private System.Windows.Forms.DataGridViewTextBoxColumn security_id;
		private System.Windows.Forms.DataGridView dg_Security;
		private System.Windows.Forms.TabPage tab_Param;
		private System.Windows.Forms.TabPage tab_Security;
		private System.Windows.Forms.TabControl ctl_Tabs;
		private System.Windows.Forms.TextBox txt_Info;
		private System.Windows.Forms.Timer timer_Handler;
		private System.Windows.Forms.Button btn_Test;
		private System.Windows.Forms.Panel ctl_Connect;
		private System.Windows.Forms.Label txt_Connect;
		private System.Windows.Forms.Panel panel1;
		private System.Windows.Forms.Label txt_Status;
		private System.Windows.Forms.Button btn_Connect;
	}
}
