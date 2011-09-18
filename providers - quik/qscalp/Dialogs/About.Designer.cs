namespace QScalp.AboutDialog
{
  partial class AboutForm
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
      this.groupBox1 = new System.Windows.Forms.GroupBox();
      this.button1 = new System.Windows.Forms.Button();
      this.siteLink = new System.Windows.Forms.LinkLabel();
      this.copy = new System.Windows.Forms.Label();
      this.progDescr = new System.Windows.Forms.Label();
      this.progName = new System.Windows.Forms.Label();
      this.groupBox1.SuspendLayout();
      this.SuspendLayout();
      // 
      // groupBox1
      // 
      this.groupBox1.Controls.Add(this.button1);
      this.groupBox1.Controls.Add(this.siteLink);
      this.groupBox1.Controls.Add(this.copy);
      this.groupBox1.Controls.Add(this.progDescr);
      this.groupBox1.Controls.Add(this.progName);
      this.groupBox1.Dock = System.Windows.Forms.DockStyle.Fill;
      this.groupBox1.Location = new System.Drawing.Point(5, 0);
      this.groupBox1.Margin = new System.Windows.Forms.Padding(10);
      this.groupBox1.Name = "groupBox1";
      this.groupBox1.Size = new System.Drawing.Size(233, 205);
      this.groupBox1.TabIndex = 1;
      this.groupBox1.TabStop = false;
      // 
      // button1
      // 
      this.button1.DialogResult = System.Windows.Forms.DialogResult.OK;
      this.button1.Location = new System.Drawing.Point(79, 162);
      this.button1.Name = "button1";
      this.button1.Size = new System.Drawing.Size(75, 23);
      this.button1.TabIndex = 4;
      this.button1.Text = "OK";
      this.button1.UseVisualStyleBackColor = true;
      // 
      // siteLink
      // 
      this.siteLink.ActiveLinkColor = System.Drawing.Color.Blue;
      this.siteLink.Dock = System.Windows.Forms.DockStyle.Top;
      this.siteLink.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
      this.siteLink.Location = new System.Drawing.Point(3, 119);
      this.siteLink.Margin = new System.Windows.Forms.Padding(5);
      this.siteLink.Name = "siteLink";
      this.siteLink.Size = new System.Drawing.Size(227, 22);
      this.siteLink.TabIndex = 3;
      this.siteLink.TabStop = true;
      this.siteLink.Text = "http://www.moroshkin.com";
      this.siteLink.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
      this.siteLink.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.siteLink_LinkClicked);
      // 
      // copy
      // 
      this.copy.Dock = System.Windows.Forms.DockStyle.Top;
      this.copy.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
      this.copy.Location = new System.Drawing.Point(3, 100);
      this.copy.Margin = new System.Windows.Forms.Padding(5);
      this.copy.Name = "copy";
      this.copy.Size = new System.Drawing.Size(227, 19);
      this.copy.TabIndex = 2;
      this.copy.Text = "© 2011 Николай Морошкин";
      this.copy.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
      // 
      // progDescr
      // 
      this.progDescr.Dock = System.Windows.Forms.DockStyle.Top;
      this.progDescr.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
      this.progDescr.Location = new System.Drawing.Point(3, 50);
      this.progDescr.Margin = new System.Windows.Forms.Padding(5);
      this.progDescr.Name = "progDescr";
      this.progDescr.Size = new System.Drawing.Size(227, 50);
      this.progDescr.TabIndex = 1;
      this.progDescr.Text = "Торговый привод для\nбиржевого терминала QUIK";
      this.progDescr.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
      // 
      // progName
      // 
      this.progName.Dock = System.Windows.Forms.DockStyle.Top;
      this.progName.Enabled = false;
      this.progName.Font = new System.Drawing.Font("Microsoft Sans Serif", 14F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
      this.progName.Location = new System.Drawing.Point(3, 16);
      this.progName.Margin = new System.Windows.Forms.Padding(5);
      this.progName.Name = "progName";
      this.progName.Size = new System.Drawing.Size(227, 34);
      this.progName.TabIndex = 0;
      this.progName.Text = "-";
      this.progName.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
      // 
      // AboutForm
      // 
      this.AcceptButton = this.button1;
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
      this.CancelButton = this.button1;
      this.ClientSize = new System.Drawing.Size(243, 210);
      this.Controls.Add(this.groupBox1);
      this.MaximizeBox = false;
      this.MinimizeBox = false;
      this.Name = "AboutForm";
      this.Padding = new System.Windows.Forms.Padding(5, 0, 5, 5);
      this.ShowIcon = false;
      this.ShowInTaskbar = false;
      this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Hide;
      this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
      this.Text = "О программе";
      this.groupBox1.ResumeLayout(false);
      this.ResumeLayout(false);

    }

    #endregion

    private System.Windows.Forms.GroupBox groupBox1;
    private System.Windows.Forms.Label progName;
    private System.Windows.Forms.LinkLabel siteLink;
    private System.Windows.Forms.Label progDescr;
    private System.Windows.Forms.Button button1;
    private System.Windows.Forms.Label copy;
  }
}