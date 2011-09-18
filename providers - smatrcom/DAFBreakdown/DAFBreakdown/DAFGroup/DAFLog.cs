using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;

namespace DAFBreakdown.DAFGroup
{
    public class DAFLog
    {
        private bool bStop;
        private string sLogName;
        private Thread ThreadScanMessages;
        private Queue<string> Messages;
        private StreamWriter MessageWriter;
        private DateTime WriterDate;

        public DAFLog()
        {
            bStop = false;
            sLogName = "";
            Messages = new Queue<string>();
            Messages.Clear();
            AddMessages("Лог","Запуск.");
            ThreadScanMessages = new Thread(ScanMessages);
            ThreadScanMessages.Start();
        }

        public DAFLog(string logname)
        {
            bStop = false;
            sLogName = logname;
            Messages = new Queue<string>();
            Messages.Clear();
            AddMessages("Лог", "Запуск.");
            ThreadScanMessages = new Thread(ScanMessages);
            ThreadScanMessages.Start();
        }

        public void Dispose()
        {
            AddMessages("Лог", "Выход.");
            bStop = true;
        }

        private void CreateWriter()
        {
            try
            {
                if (!Directory.Exists(@"Logs"))
                    Directory.CreateDirectory(@"Logs");
                WriterDate = DateTime.Now;
                if (MessageWriter != null)
                    MessageWriter.Close();
                MessageWriter = new StreamWriter(@"Logs\" + DateTime.Now.ToString("yyyy-MM-dd") + (sLogName != "" ? "-" + sLogName : "")+ ".log", true);
                MessageWriter.AutoFlush = true;
            }
            catch (Exception)
            {
                bStop = true;
            }
        }

        public void AddMessages(string InSender, string InMessage)
        {
            if (InMessage != "")
                try
                {
                    lock (Messages)
                        Messages.Enqueue("[" + DateTime.Now.ToString("HH:mm:ss") + "] " + (InSender != "" ? "[" + InSender + "] " : "") + InMessage);
                }
                catch { }
        }

        private void SaveMessage(string Messge)
        {
            if (DateTime.Now.Date != WriterDate)
                CreateWriter();
            try
            {
                MessageWriter.WriteLine(Messge);
            }
            catch { }
        }

        private void ScanMessages()
        {
            while (!bStop || (bStop && Messages.Count > 0))
            {
                while (Messages.Count > 0)
                {
                    lock (Messages)
                        SaveMessage(Messages.Dequeue());
                    Thread.Sleep(1);
                }
                Thread.Sleep(500);
            }
            if (MessageWriter != null)
                MessageWriter.Close();
        }
    }
}
