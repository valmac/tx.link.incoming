using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace TestConnect
{
    public class DAFWriter
    {
        private bool InfoAppend;
        private string InfoName;
        private string InfoPath;
        private DateTime InfoClock;
        private StreamWriter InfoWriter;

        public DAFWriter(string Path, string Name, bool Append)
        {
            InfoPath = Path;
            InfoName = Name;
            InfoAppend = Append;
            InfoClock = DateTime.MinValue;
            if (!Directory.Exists(Path))
                Directory.CreateDirectory(Path);
            WriteLine("{0:T} Ready", DateTime.Now);
        }

        public void WriteLine(string Message, params object[] Arg)
        {
            try
            {
                if (InfoWriter != null && InfoClock.Date != DateTime.Now.Date)
                {
                    InfoWriter.Close();
                    InfoWriter.Dispose();
                    InfoWriter = null;
                    InfoClock = DateTime.Now;
                }
                if (InfoWriter == null)
                {
                    InfoWriter = new StreamWriter(InfoPath + @"\" + DateTime.Now.ToString("yyyy-MM-dd") + "." + InfoName, InfoAppend, System.Text.Encoding.Default);
                    InfoWriter.AutoFlush = true;
                }
                InfoWriter.WriteLine(Message, Arg);
            }
            catch { }
        }
    }

    public class DAFWriters
    {
        private List<DAFWriterItem> InfoWriters;

        public class DAFWriterItem
        {
            private string InfoName;
            private string InfoPath;
            private DAFWriter InfoWriter;

            public DAFWriterItem(string Path, string Name, bool Append)
            {
                InfoName = Name;
                InfoPath = Path;
                InfoWriter = new DAFWriter(Path, InfoName, Append);
            }

            public string Name { get { return InfoName; } }
            public string Path { get { return InfoPath; } }
            public DAFWriter Writer { get { return InfoWriter; } }
        }

        public DAFWriters()
        {
            InfoWriters = new List<DAFWriterItem>();
        }

        public void WriteLine(string Path, string Name, string Message, params object[] Arg)
        {
            if (!InfoWriters.Any(tempWriters => tempWriters.Name == Name && tempWriters.Path == Path))
                InfoWriters.Add(new DAFWriterItem(Path, Name, false));
            if (InfoWriters.Any(tempWriters => tempWriters.Name == Name && tempWriters.Path == Path))
                InfoWriters.Find(tempWriters => tempWriters.Name == Name && tempWriters.Path == Path).Writer.WriteLine(Message, Arg);
        }
    }
}
