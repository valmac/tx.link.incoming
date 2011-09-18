using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Xml.Serialization;

namespace DAFBreakdown.DAFGroup
{
    public enum Mode { Active, Stop, Pause };

    public class SmartAccount
    {
        private int iLastIndex;
        private string sLogin;
        private string sPassword;
        private List<string> ListIPPort;

        public SmartAccount()
        {
            iLastIndex = -1;
            sLogin = "";
            sPassword = "";
            ListIPPort = new List<string>();
        }

        public string Login { get { return sLogin; } set { sLogin = value; } }
        public string Password { get { return sPassword; } set { sPassword = value; } }
        public List<string> IPPorts { get { return ListIPPort; } set { ListIPPort = value; } }
        public bool Redy { get { return (sLogin != "" && ListIPPort != null && ListIPPort.Count > 0 ? true : false); } }

        public void AddIPPort(string IPPort)
        {
            if (IPPort != "" && ListIPPort.IndexOf(IPPort) == -1)
                ListIPPort.Add(IPPort);
        }

        public void Remove(string IPPort)
        {
            if (IPPort != "" && ListIPPort.IndexOf(IPPort) != -1)
                ListIPPort.Remove(IPPort);
        }

        public bool GetNext(out string IP, out short Port)
        {
            bool bReturn = false;
            IP = ""; Port = 0;
            if (ListIPPort != null && ListIPPort.Count > 0)
            {
                iLastIndex++;
                iLastIndex = (iLastIndex < 0 || iLastIndex > ListIPPort.Count - 1) ? 0 : iLastIndex;
                string sIPPort = ListIPPort[iLastIndex];
                if (sIPPort.Length > 0 && sIPPort.Length > sIPPort.IndexOf(":"))
                {
                    IP = sIPPort.Substring(0, sIPPort.IndexOf(":"));
                    Port = System.Convert.ToInt16(sIPPort.Substring(sIPPort.IndexOf(":") + 1, sIPPort.Length - sIPPort.IndexOf(":") - 1));
                    bReturn = true;
                }
            }
            return bReturn;
        }

        public override string ToString()
        {
            return "Account: " + (sLogin != "" ? sLogin + "IP: " + (ListIPPort != null && ListIPPort.Count > 0 ? ListIPPort.Count.ToString() : "None") : "UnKnow");
        }

        public bool Load(string filename)
        {
            bool bReturn = false;
            if (File.Exists(filename))
            {
                try
                {
                    using (Stream stream = new FileStream(filename, FileMode.Open))
                    {
                        XmlSerializer serializer = new XmlSerializer(this.GetType());
                        SmartAccount tempAccount = (SmartAccount)serializer.Deserialize(stream);
                        sLogin = tempAccount.Login;
                        sPassword = tempAccount.Password;
                        ListIPPort = tempAccount.IPPorts;
                        bReturn = true;
                        stream.Close();
                    }
                }
                catch
                {
                    bReturn = false;
                }
            }

            return bReturn;
        }

        public void Save(string filename)
        {
            try
            {
                if (File.Exists(filename))
                    File.Delete(filename);
                using (Stream writer = new FileStream(filename, FileMode.CreateNew))
                {
                    XmlSerializer serializer = new XmlSerializer(this.GetType());
                    serializer.Serialize(writer, this);
                    writer.Close();
                }
            }
            catch { }
        }
    }

    public class PortfolioInfo
    {
        private string sName;
        private string sExch;
        private double dCash;
        private double dFee;

        public PortfolioInfo(string name, string exch)
        {
            sName = name;
            sExch = exch;
            dCash = 0.0d;
            dFee = 0.0d;
        }

        public string Name { get { return sName; } }
        public string Exch { get { return sExch; } }
        public double Cash { get { return dCash; } set { dCash = value; } }
        public double Fee { get { return dFee; } set { dFee = value; } }
        public override string ToString()
        {
            return sName;
        }

        public string ToStringLong()
        {
            return "[" + sExch + ":" + sName + "] Cash: " + dCash.ToString("### ##0.00;") + " Fee: " + dFee.ToString("### ##0.00;");
        }

        public bool UpDate(string name, double cash, double fee)
        {
            bool bRetunr = false;
            if (name == sName)
            {
                dCash = cash;
                dFee = fee;
                bRetunr = true;
            }
            return bRetunr;
        }
    }

    public class FavoriteTiker
    {
        private string sPortfolio;
        private Tiker infoTiker;

        public FavoriteTiker(string portfolio, Tiker tiker)
        {
            sPortfolio = portfolio;
            infoTiker = tiker;
        }

        public string Portfolio { get { return sPortfolio; } }
        public Tiker InfoTiker { get { return infoTiker; } }
        public override string ToString()
        {
            return "Favorite: " + Portfolio + " " + infoTiker.ToString();
        }
        public string[] GetInfoRow()
        {
            return new string[] { sPortfolio, infoTiker.ShortName, infoTiker.CodeClass };
        }
    }

    public class Tiker
    {
        private string sCode;
        private string sCodeClass;
        private string sShortName;
        private string sLongName;
        private double dStep;
        private double dStepPrice;
        private double dDecimals;
        private string sExchName;
        private DateTime dtExpiryDate;
        private int iDaysBeforeExpiry;

        public Tiker(string codeclass, string code, string shortname, string longname, double step, double stepprice, double decimals, string exchname, DateTime expirydate, int daysbeforeexpiry)
        {
            sCodeClass = codeclass;
            sCode = code;
            sShortName = shortname;
            sLongName = longname;
            dStep = step;
            dStepPrice = stepprice;
            dDecimals = decimals;
            sExchName = exchname;
            dtExpiryDate = expirydate;
            iDaysBeforeExpiry = daysbeforeexpiry;
        }

        public double ToMoney(double Punkts)
        {
            return dStepPrice / dStep * Punkts;
        }
        public string DoubleToString(double InDouble)
        {
            return InDouble.ToString("F" + (dDecimals).ToString("00;"), new CultureInfo("en-us"));
        }
        public override string ToString()
        {
            return "[" + sCodeClass + ":" + sLongName + "] Step: " + DoubleToString(dStep) + " StepPrice: " + DoubleToString(dStepPrice) + " Short:" + sShortName + " Code: " + sCode + " ExchName: " + sExchName + " Expiry: " + dtExpiryDate.ToShortDateString() + " (" + iDaysBeforeExpiry + ")"; ;
        }
        public string[] GetInfoRow()
        {
            return new string[] { sShortName, sCodeClass, DoubleToString(dStep), DoubleToString(dStepPrice), DoubleToString(ToMoney(1)) };
        }

        public string Code { get { return sCode; } }
        public string CodeClass { get { return sCodeClass; } }
        public string LongName { get { return sLongName; } }
        public string ShortName { get { return sShortName; } }
        public double Step { get { return dStep; } }
        public double StepPrice { get { return dStepPrice; } }
        public double Decimals { get { return dDecimals; } }
        public string ExchName { get { return sExchName; } }
        public DateTime ExpiryDate { get { return dtExpiryDate; } }
        public int DaysBeforeExpiry { get { return iDaysBeforeExpiry; } }
    }

    public class Trade
    {
        private double dPrice;
        private int iVolume;
        private DateTime dtClock;
        private string sTradeNo;

        public Trade(double price, int volume, DateTime clock, string tradeno)
        {
            dPrice = price;
            iVolume = volume;
            dtClock = clock;
            sTradeNo = tradeno;
        }

        public double Price { get { return dPrice; } }
        public int Volume { get { return iVolume; } }
        public DateTime Clock { get { return dtClock; } }
        public string TradeNo { get { return sTradeNo; } }

        public override string ToString()
        {
            return "[Trade: " + sTradeNo + "] " + dtClock.ToString() + " Price: " + dPrice.ToString("### ##0.0000;") + "(" + iVolume + ")";
        }
    }

    public class Order
    {
        public enum OrderAction { Buy, Sell }
        public enum OrderStatus { None, Pending, Cansel, Filled };

        private OrderAction action;
        private OrderStatus status;        
        private double dPrice;
        private int iVolume;
        private DateTime dtClockCreate;
        private DateTime dtClockFinish;
        private string sOrderId;
        private string sOrderNo;
        private int iTransId;
        private List<Trade> ListTrades;

        public Order(OrderAction action, double price, int volume, int transid)
        {
            this.action = action;
            this.dPrice = price;
            this.iVolume = volume;
            this.iTransId = transid;
            this.status = OrderStatus.None;
            this.dtClockCreate = DateTime.MinValue;
            this.dtClockFinish = DateTime.MinValue;
            this.sOrderId = "";
            this.sOrderNo = "";
            this.ListTrades = new List<Trade>();
        }

        public int Filled()
        {
            int iFilled = 0;
            for (int iIndex = 0; iIndex < ListTrades.Count; iIndex++)
                iFilled += Math.Abs(ListTrades[iIndex].Volume);
            return iFilled;
        }

        public double AVGPrice()
        {
            double dAVGPrice = 0;
            int iFilled = 0;
            for (int iIndex = 0; iIndex < ListTrades.Count; iIndex++)
            {
                dAVGPrice += ListTrades[iIndex].Price;
                iFilled += Math.Abs(ListTrades[iIndex].Volume);
            }
            return (iVolume > 0 && iFilled == iVolume) ? dAVGPrice / iFilled : dAVGPrice;
        }

        public bool IsFilled()
        {
            return (status == OrderStatus.Filled && iVolume == Filled()) ? true : false;
        }

        public OrderStatus Status { get { return status; } }
        public OrderAction Action { get { return action; } }
        public double Price { get { return dPrice; } }
        public int Volume { get { return iVolume; } }
        public DateTime ClockCreate { get { return dtClockCreate; } }
        public DateTime ClockFinish { get { return dtClockFinish; } }
        public string OrderId { get { return sOrderId; } }
        public string OrderNo { get { return sOrderNo; } }
        public int TransId { get { return iTransId; } }
        public List<Trade> Trades { get { return ListTrades; } }
    }
}
