namespace QScalp.TradeLogDialog
{
  partial class TradeLogForm
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
      if(disposing && (components != null))
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
      this.logDataBox = new System.Windows.Forms.TextBox();
      this.groupBox1 = new System.Windows.Forms.GroupBox();
      this.labelTurnover = new System.Windows.Forms.Label();
      this.labelTradesCount = new System.Windows.Forms.Label();
      this.label2 = new System.Windows.Forms.Label();
      this.label1 = new System.Windows.Forms.Label();
      this.groupBox2 = new System.Windows.Forms.GroupBox();
      this.label4 = new System.Windows.Forms.Label();
      this.labelAveragePerLot = new System.Windows.Forms.Label();
      this.labelOverallPerLot = new System.Windows.Forms.Label();
      this.label3 = new System.Windows.Forms.Label();
      this.groupBox3 = new System.Windows.Forms.GroupBox();
      this.label12 = new System.Windows.Forms.Label();
      this.labelAverage = new System.Windows.Forms.Label();
      this.label9 = new System.Windows.Forms.Label();
      this.labelOverall = new System.Windows.Forms.Label();
      this.groupBox1.SuspendLayout();
      this.groupBox2.SuspendLayout();
      this.groupBox3.SuspendLayout();
      this.SuspendLayout();
      // 
      // logDataBox
      // 
      this.logDataBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                  | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.logDataBox.HideSelection = false;
      this.logDataBox.Location = new System.Drawing.Point(12, 12);
      this.logDataBox.Multiline = true;
      this.logDataBox.Name = "logDataBox";
      this.logDataBox.ReadOnly = true;
      this.logDataBox.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
      this.logDataBox.Size = new System.Drawing.Size(318, 231);
      this.logDataBox.TabIndex = 0;
      this.logDataBox.WordWrap = false;
      // 
      // groupBox1
      // 
      this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.groupBox1.Controls.Add(this.labelTurnover);
      this.groupBox1.Controls.Add(this.labelTradesCount);
      this.groupBox1.Controls.Add(this.label2);
      this.groupBox1.Controls.Add(this.label1);
      this.groupBox1.Location = new System.Drawing.Point(13, 249);
      this.groupBox1.Name = "groupBox1";
      this.groupBox1.Size = new System.Drawing.Size(317, 50);
      this.groupBox1.TabIndex = 1;
      this.groupBox1.TabStop = false;
      this.groupBox1.Text = "Статистика";
      // 
      // labelTurnover
      // 
      this.labelTurnover.Location = new System.Drawing.Point(256, 22);
      this.labelTurnover.Name = "labelTurnover";
      this.labelTurnover.Size = new System.Drawing.Size(55, 13);
      this.labelTurnover.TabIndex = 1;
      this.labelTurnover.Text = "?";
      this.labelTurnover.TextAlign = System.Drawing.ContentAlignment.TopRight;
      // 
      // labelTradesCount
      // 
      this.labelTradesCount.Location = new System.Drawing.Point(108, 22);
      this.labelTradesCount.Name = "labelTradesCount";
      this.labelTradesCount.Size = new System.Drawing.Size(35, 13);
      this.labelTradesCount.TabIndex = 1;
      this.labelTradesCount.Text = "?";
      this.labelTradesCount.TextAlign = System.Drawing.ContentAlignment.TopRight;
      // 
      // label2
      // 
      this.label2.AutoSize = true;
      this.label2.Location = new System.Drawing.Point(168, 22);
      this.label2.Name = "label2";
      this.label2.Size = new System.Drawing.Size(82, 13);
      this.label2.TabIndex = 0;
      this.label2.Text = "Оборот, лотов:";
      // 
      // label1
      // 
      this.label1.AutoSize = true;
      this.label1.Location = new System.Drawing.Point(14, 22);
      this.label1.Name = "label1";
      this.label1.Size = new System.Drawing.Size(88, 13);
      this.label1.TabIndex = 0;
      this.label1.Text = "Кол-во трейдов:";
      // 
      // groupBox2
      // 
      this.groupBox2.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.groupBox2.Controls.Add(this.label4);
      this.groupBox2.Controls.Add(this.labelAveragePerLot);
      this.groupBox2.Controls.Add(this.labelOverallPerLot);
      this.groupBox2.Controls.Add(this.label3);
      this.groupBox2.Location = new System.Drawing.Point(13, 305);
      this.groupBox2.Name = "groupBox2";
      this.groupBox2.Size = new System.Drawing.Size(317, 50);
      this.groupBox2.TabIndex = 2;
      this.groupBox2.TabStop = false;
      this.groupBox2.Text = "Результат на 1 лот";
      // 
      // label4
      // 
      this.label4.AutoSize = true;
      this.label4.Location = new System.Drawing.Point(158, 22);
      this.label4.Name = "label4";
      this.label4.Size = new System.Drawing.Size(102, 13);
      this.label4.TabIndex = 0;
      this.label4.Text = "Средний трейд, пт:";
      // 
      // labelAveragePerLot
      // 
      this.labelAveragePerLot.Location = new System.Drawing.Point(266, 22);
      this.labelAveragePerLot.Name = "labelAveragePerLot";
      this.labelAveragePerLot.Size = new System.Drawing.Size(45, 13);
      this.labelAveragePerLot.TabIndex = 1;
      this.labelAveragePerLot.Text = "?";
      this.labelAveragePerLot.TextAlign = System.Drawing.ContentAlignment.TopRight;
      // 
      // labelOverallPerLot
      // 
      this.labelOverallPerLot.Location = new System.Drawing.Point(77, 22);
      this.labelOverallPerLot.Name = "labelOverallPerLot";
      this.labelOverallPerLot.Size = new System.Drawing.Size(55, 13);
      this.labelOverallPerLot.TabIndex = 1;
      this.labelOverallPerLot.Text = "?";
      this.labelOverallPerLot.TextAlign = System.Drawing.ContentAlignment.TopRight;
      // 
      // label3
      // 
      this.label3.AutoSize = true;
      this.label3.Location = new System.Drawing.Point(14, 22);
      this.label3.Name = "label3";
      this.label3.Size = new System.Drawing.Size(57, 13);
      this.label3.TabIndex = 0;
      this.label3.Text = "Всего, пт:";
      // 
      // groupBox3
      // 
      this.groupBox3.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.groupBox3.Controls.Add(this.label12);
      this.groupBox3.Controls.Add(this.labelAverage);
      this.groupBox3.Controls.Add(this.label9);
      this.groupBox3.Controls.Add(this.labelOverall);
      this.groupBox3.Location = new System.Drawing.Point(13, 361);
      this.groupBox3.Name = "groupBox3";
      this.groupBox3.Size = new System.Drawing.Size(317, 50);
      this.groupBox3.TabIndex = 3;
      this.groupBox3.TabStop = false;
      this.groupBox3.Text = "Общий результат";
      // 
      // label12
      // 
      this.label12.AutoSize = true;
      this.label12.Location = new System.Drawing.Point(158, 22);
      this.label12.Name = "label12";
      this.label12.Size = new System.Drawing.Size(102, 13);
      this.label12.TabIndex = 0;
      this.label12.Text = "Средний трейд, пт:";
      // 
      // labelAverage
      // 
      this.labelAverage.Location = new System.Drawing.Point(266, 22);
      this.labelAverage.Name = "labelAverage";
      this.labelAverage.Size = new System.Drawing.Size(45, 13);
      this.labelAverage.TabIndex = 1;
      this.labelAverage.Text = "?";
      this.labelAverage.TextAlign = System.Drawing.ContentAlignment.TopRight;
      // 
      // label9
      // 
      this.label9.AutoSize = true;
      this.label9.Location = new System.Drawing.Point(14, 22);
      this.label9.Name = "label9";
      this.label9.Size = new System.Drawing.Size(57, 13);
      this.label9.TabIndex = 0;
      this.label9.Text = "Всего, пт:";
      // 
      // labelOverall
      // 
      this.labelOverall.Location = new System.Drawing.Point(77, 22);
      this.labelOverall.Name = "labelOverall";
      this.labelOverall.Size = new System.Drawing.Size(55, 13);
      this.labelOverall.TabIndex = 1;
      this.labelOverall.Text = "?";
      this.labelOverall.TextAlign = System.Drawing.ContentAlignment.TopRight;
      // 
      // TradeLogForm
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.ClientSize = new System.Drawing.Size(342, 423);
      this.Controls.Add(this.groupBox3);
      this.Controls.Add(this.groupBox2);
      this.Controls.Add(this.groupBox1);
      this.Controls.Add(this.logDataBox);
      this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
      this.MaximizeBox = false;
      this.MinimizeBox = false;
      this.Name = "TradeLogForm";
      this.ShowInTaskbar = false;
      this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
      this.Text = "Торговый журнал";
      this.groupBox1.ResumeLayout(false);
      this.groupBox1.PerformLayout();
      this.groupBox2.ResumeLayout(false);
      this.groupBox2.PerformLayout();
      this.groupBox3.ResumeLayout(false);
      this.groupBox3.PerformLayout();
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.TextBox logDataBox;
    private System.Windows.Forms.GroupBox groupBox1;
    private System.Windows.Forms.GroupBox groupBox2;
    private System.Windows.Forms.GroupBox groupBox3;
    private System.Windows.Forms.Label label2;
    private System.Windows.Forms.Label label1;
    private System.Windows.Forms.Label labelTradesCount;
    private System.Windows.Forms.Label label4;
    private System.Windows.Forms.Label label3;
    private System.Windows.Forms.Label labelTurnover;
    private System.Windows.Forms.Label labelAveragePerLot;
    private System.Windows.Forms.Label labelOverallPerLot;
    private System.Windows.Forms.Label label12;
    private System.Windows.Forms.Label labelAverage;
    private System.Windows.Forms.Label label9;
    private System.Windows.Forms.Label labelOverall;
  }
}