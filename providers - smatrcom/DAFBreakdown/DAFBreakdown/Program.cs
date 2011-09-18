using System;
using System.Reflection;
using System.Threading;
using System.Windows.Forms;

namespace DAFBreakdown
{
    static class Program
    {
        private class DAFContext : ApplicationContext
        {
            private DAFGroup.LogoForm Logo;
            private DAFGroup.DAFLog dafLog;
            private BreakdownForm Breakdown;
            private DAFGroup.SmartAccount Account;

            public DAFContext()
            {
                dafLog = new DAFGroup.DAFLog();
                Application.ApplicationExit += new EventHandler(this.OnApplicationExit);
                Logo = new DAFBreakdown.DAFGroup.LogoForm(Assembly.GetExecutingAssembly().GetName().Version.ToString(), 3, DAFLogoClosing, DAFLogoClosed);
                Account = new DAFGroup.SmartAccount();
                Account.Load("Account.cfg");
            }

            private void DAFLogoClosing(object sender, EventArgs e)
            {
                if (!Account.Redy)
                {
                    DAFGroup.LoginForm AccountForm = new DAFGroup.LoginForm();
                    if (AccountForm.ShowDialog() == DialogResult.OK)
                    {
                        Account = AccountForm.GetAccount;
                        Account.Save("Account.cfg");
                    }
                    AccountForm.Dispose();
                }
                if (!Account.Redy)
                {
                    Logo.UpDateError("Учётная запись не найдена!");
                    dafLog.AddMessages("Start", "Учётная запись не найдена!");
                    Thread.Sleep(1000);
                }
                else
                    Breakdown = new BreakdownForm(Account, dafLog, BreakdownClosed);
            }

            private void DAFLogoClosed(object sender, EventArgs e)
            {
                if (Breakdown != null)
                    Breakdown.Show();
                else
                    Application.Exit();
            }

            private void BreakdownClosed(object sender, EventArgs e)
            {
                Application.Exit();
            }

            private void OnApplicationExit(object sender, EventArgs e)
            {
                dafLog.Dispose();
            }
        }

        /// <summary>
        /// Главная точка входа для приложения.
        /// </summary>
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new DAFContext());
        }
    }
}
