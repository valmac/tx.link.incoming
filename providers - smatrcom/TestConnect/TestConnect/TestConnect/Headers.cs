using System;
using System.Globalization;
using System.Threading;
using System.Reflection;

namespace TestConnect
{
    public class Info
    {
        public static string GetVersion
        {
            get { return Assembly.GetExecutingAssembly().GetName().Version.ToString(); }
        }
    }
    
    public class Bar
    {
        public enum Type { Open, Max, Min, Close, AVG }

        private string InfoCode;
        private System.DateTime InfoClock;
        private double InfoOpen;
        private double InfoHigh;
        private double InfoLow;
        private double InfoClose;
        private double InfoVolume;

        public Bar(string code, System.DateTime clock, double open, double high, double low, double close, double volume)
        {
            InfoCode = code;
            InfoClock = clock;
            InfoOpen = open;
            InfoHigh = high;
            InfoLow = low;
            InfoClose = close;
            InfoVolume = volume;
        }

        public string Code { get { return InfoCode; } }
        public DateTime Clock { get { return InfoClock; } }
        public double Open { get { return InfoOpen; } }
        public double High { get { return InfoHigh; } }
        public double Low { get { return InfoLow; } }
        public double Close { get { return InfoClose; } }
        public double Volume { get { return InfoVolume; } }
        public double GetBy(Type type)
        {
            return (type == Type.Open ? InfoOpen : type == Type.Max ? InfoHigh : type == Type.Min ? InfoLow : type == Type.Close ? InfoClose : type == Type.AVG ? (InfoLow + InfoHigh) / 2.0d : 0.0d);
        }

        public override string ToString()
        {
            return "Bar[" + InfoCode + "] " + InfoClock.ToString() + " Open:" + InfoOpen + " High:" + InfoHigh + " Low:" + InfoLow + " Close:" + InfoClose + " Volume:" + InfoVolume;
        }
    }

    public class Quote
    {
        public event EventHandler EventUpDate;
        public delegate void EventHandler();

        private string InfoCode;
        private double InfoAsk;
        private double InfoBid;
        private int InfoAskVolume;
        private int InfoBidVolume;
        private int InfoStatus;
        private double InfoLastPrice;
        private int InfoLastVolume;
        private DateTime InfoLastClock;
        private StClientLib.StOrder_Action InfoLastAction;

        public Quote(string Code, DateTime Clock, double Last, double Volume, int Status, EventHandler OnEventUpDate)
        {
            InfoCode = Code;
            InfoAsk = 0.0d;
            InfoBid = 0.0d;
            InfoAskVolume = 0;
            InfoBidVolume = 0;
            InfoStatus = Status;
            InfoLastPrice = Last;
            InfoLastVolume = (int)Volume;
            InfoLastClock = Clock;
            if (OnEventUpDate != null)
                EventUpDate += new EventHandler(OnEventUpDate);
            new Thread(ThreadUpdate).Start();
        }

        private void ThreadUpdate()
        {
            if (EventUpDate != null)
                EventUpDate();
        }

        public void UpDate(int Status)
        {
            InfoStatus = Status;
            new Thread(ThreadUpdate).Start();
        }
        public void UpDate(double Ask, double AskVolume, double Bid, double BidVolume)
        {
            InfoAsk = Ask;
            InfoBid = Bid;
            InfoAskVolume = (int)AskVolume;
            InfoBidVolume = (int)BidVolume;
            new Thread(ThreadUpdate).Start();
        }
        public void UpDate(DateTime Clock, double Price, double Volume, StClientLib.StOrder_Action Action)
        {
            InfoLastClock = Clock;
            InfoLastPrice = Price;
            InfoLastVolume = (int)Volume;
            InfoLastAction = Action;
            new Thread(ThreadUpdate).Start();
        }

        public string Code { get { return InfoCode; } }
        public double Ask { get { return InfoAsk; } }
        public double Bid { get { return InfoBid; } }
        public int AskVolume { get { return InfoAskVolume; } }
        public int BidVolume { get { return InfoBidVolume; } }
        public int Status { get { return InfoStatus; } }
        public double LastPrice { get { return InfoLastPrice; } }
        public int LastVolume { get { return InfoLastVolume; } }
        public DateTime LastClock { get { return InfoLastClock; } }
        public StClientLib.StOrder_Action LastAction { get { return InfoLastAction; } }
    }

    public class Tiker
    {
        private string sCode;
        private string sShortName;
        private string sLongName;
        private double dStep;
        private double dStepPrice;
        private int iDecimals;
        private string sSecExtId;
        private string sSecExchName;
        private System.DateTime dtExpiryDate;
        private double dDaysBeforeExpiry;

        public Tiker(string code, string shortname, string longname, double step, double stepprice, double decimals, string sec_ext_id, string sec_exch_name, System.DateTime expiryDate, double daysbeforeexpiry)
        {
            sCode = code;
            sShortName = shortname;
            sLongName = longname;
            dStep = step;
            dStepPrice = stepprice;
            iDecimals = (int)decimals;
            sSecExtId = sec_ext_id;
            sSecExchName = sec_exch_name;
            dtExpiryDate = expiryDate;
            dDaysBeforeExpiry = daysbeforeexpiry;
        }

        public double ToMoney(double Punkts)
        {
            return dStepPrice / dStep * Punkts;
        }

        public override string ToString()
        {
            CultureInfo ci = new CultureInfo("en-us");
            return "[" + sSecExchName + ":" + sCode + "] Step: " + dStep.ToString("G", ci) + " Money: " + ToMoney(1).ToString("G", ci) + " Short:" + sShortName + " Expiry: " + dtExpiryDate.ToShortDateString() + "(" + (int)dDaysBeforeExpiry + ")";
        }

        public string Code { get { return sCode; } }
        public string ShortName { get { return sShortName; } }
        public string LongName { get { return sLongName; } }
        public double Step { get { return dStep; } }
        public double StepPrice { get { return dStepPrice; } }
        public int Decimals { get { return iDecimals; } }
        public string SecExtId { get { return sSecExtId; } }
        public string SecExchName { get { return sSecExchName; } }
        public System.DateTime ExpiryDate { get { return dtExpiryDate; } }
        public double DaysBeforeExpiry { get { return dDaysBeforeExpiry; } }
    }
}
