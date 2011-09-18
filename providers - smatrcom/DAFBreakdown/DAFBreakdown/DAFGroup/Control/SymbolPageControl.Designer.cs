namespace DAFBreakdown.DAFGroup.Control
{
    partial class SymbolTabControl
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
            this.SymbolInfo = new DAFBreakdown.DAFGroup.Control.SymbolControl();
            this.SuspendLayout();
            // 
            // SymbolInfo
            // 
            this.SymbolInfo.BackColor = System.Drawing.Color.Transparent;
            this.SymbolInfo.Dock = System.Windows.Forms.DockStyle.Fill;
            this.SymbolInfo.Location = new System.Drawing.Point(0, 0);
            this.SymbolInfo.Name = "SymbolInfo";
            this.SymbolInfo.Size = new System.Drawing.Size(786, 369);
            this.SymbolInfo.TabIndex = 0;
            this.SymbolInfo.TabStop = false;
            // 
            // TikersControl
            // 
            this.Size = new System.Drawing.Size(456, 324);
            this.ResumeLayout(false);

        }

        #endregion

        private SymbolControl SymbolInfo;

    }
}
