using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Threading;
using StClientLib;

namespace DAFBreakdown.DAFGroup
{
    public class SmartCOM
    {
        private DAFLog dafLog;
        private SmartAccount Account;
        private StServer SmartServer;
        private List<Tiker> AllTikers;
        private List<PortfolioInfo> AllPortfolios;
        
        private bool bStop;
        private bool bFirst;
        private bool bDisconnected;
        private int iDisconnectCount;

        private Thread ThreadIdleInit;
        private Thread ThreadIdleCreate;
        private Thread ThreadIdleConnect;

        // События
        public event EventHandler Destroyed;
        public event EventHandler ReadyTikers;
        public event EventHandler ReadyPortfolios;
        public event EventHandler ChangedConnection;
        public delegate void EventHandler();
        public event EventHandler<PortfolioEventArgs> ChangedPortfolio;

        // Свойства
        public List<Tiker> GetTikers { get { return AllTikers; } }
        public List<PortfolioInfo> GetPortfolios { get { return AllPortfolios; } }
        public bool IsReady { get { return (SmartServer != null ? true : false); } }
        public bool IsConnected { get { return (IsReady && SmartServer.IsConnected() ? true : false); } }

        public SmartCOM(SmartAccount InAccount, DAFLog InLog)
        {
            bFirst = true;
            dafLog = InLog;
            Account = InAccount;
            AllTikers = new List<Tiker>();
            AllPortfolios = new List<PortfolioInfo>();
        }

        public void Init()
        {
            if (!bStop)
            {
                if (ThreadIdleInit == null || (ThreadIdleInit != null && !ThreadIdleInit.IsAlive))
                {
                    dafLog.AddMessages("SmartCOM:Init", "Запуск.");
                    ThreadIdleInit = new Thread(Start);
                    ThreadIdleInit.Start();
                }
                else
                    dafLog.AddMessages("SmartCOM:Init", "Обнаружен поток инициализации");
            }
        }

        public void Dispose()
        {
            bStop = true;
            if (IsReady)
            {
                // Перед выходом рекомендуется отключиться от сервера котировок, если подключены.
                if (IsConnected)
                {
                    // Передаём команду на отключение
                    dafLog.AddMessages("SmartCOM:Dispose", "Wait for disconnect...");
                    try
                    {
                        SmartServer.disconnect();
                    }
                    catch (Exception Error)
                    {
                        dafLog.AddMessages("SmartCOM:Dispose", "Ошибка, при disconnect, " + Error.Message);
                    }
                }
                else
                    Release();
            }
            if (Destroyed != null)
                Destroyed();
        }

        private void Release()
        {
            if (IsReady)
            {
                dafLog.AddMessages("SmartCOM:Release", "Start");
                try
                {
                    // Освобождаем обработчики
                    SmartServer.Connected -= null;
                    SmartServer.Disconnected -= null;
                    SmartServer.AddSymbol -= null;
                    SmartServer.AddPortfolio -= null;
                    SmartServer.SetPortfolio -= null;
                    SmartServer.AddBar -= null;
                    SmartServer.AddTick -= null;
                    SmartServer.AddTrade -= null;
                    SmartServer.UpdateOrder -= null;
                    SmartServer.UpdateQuote -= null;
                    SmartServer.UpdateBidAsk -= null;
                    if (!bStop && SmartServer.GetType().IsCOMObject)
                        Marshal.ReleaseComObject(SmartServer);
                    SmartServer = null;
                }
                catch (Exception Error)
                {
                    dafLog.AddMessages("SmartCOM:Dispose", "Ошибка при освобождении, " + Error.Message);
                }
            }
        }

        private void Kill()
        {
            bool bError = false;
            bool bFindProccess = false;
            string sServiceName = "smartcom2";
            Process[] ProcessActive = Process.GetProcesses();
            foreach (Process ProcessCur in ProcessActive)
                if (ProcessCur.ProcessName.ToLower().IndexOf((sServiceName).ToLower()) == 0)
                {
                    bFindProccess = true;
                    try
                    {
                        ProcessCur.Kill();
                        bError = false;
                    }
                    catch (Exception Error)
                    {
                        bError = true;
                        sServiceName = ProcessCur.ProcessName;
                        dafLog.AddMessages("SmartCOM:Kill", "Ошибка при уничтожении процесса SmartCOM, " + Error.Message);
                    }
                    if (!bError)
                        dafLog.AddMessages("SmartCOM:Kill", "Процесс SmartCOM, Успешно уничтожен");
                }
            if ((bFindProccess) && (bError))
            {
                System.ServiceProcess.ServiceController SmartService = new System.ServiceProcess.ServiceController(sServiceName);
                if (SmartService.Status != System.ServiceProcess.ServiceControllerStatus.Stopped)
                {
                    try
                    {
                        SmartService.Stop();
                        SmartService.WaitForStatus(System.ServiceProcess.ServiceControllerStatus.Stopped, new TimeSpan(0, 0, 30));
                        bError = false;
                    }
                    catch (Exception Error)
                    {
                        bError = true;
                        dafLog.AddMessages("SmartCOM:Kill", "Ошибка при остановке службы SmartCOM, " + Error.Message);
                    }
                    if (!bError)
                        dafLog.AddMessages("SmartCOM:Kill", "Служба SmartCOM, Остановлена");
                }
            }
        }

        public void ListenStart()
        {
            if (IsConnected)
            {
                dafLog.AddMessages("SmartCOM:ListenStart", "Start");
                try
                {
                    foreach (PortfolioInfo tempPortfolio in AllPortfolios)
                        SmartServer.ListenPortfolio(tempPortfolio.Name);
                }
                catch (Exception Error)
                {
                    dafLog.AddMessages("SmartCOM:ListenStart", "Error, " + Error.Message);
                }
            }
        }

        public void ListenStop()
        {
            if (IsReady)
            {
                dafLog.AddMessages("SmartCOM:ListenStop", "Start");
                try
                {
                    foreach (PortfolioInfo tempPortfolio in AllPortfolios)
                        SmartServer.CancelPortfolio(tempPortfolio.Name);
                }
                catch (Exception Error)
                {
                    dafLog.AddMessages("SmartCOM:ListenStop", "Error, " + Error.Message);
                }
            }
        }

        public void Start()
        {
            if (!bStop)
            {
                if (ThreadIdleCreate != null && ThreadIdleCreate.IsAlive)
                {
                    dafLog.AddMessages("SmartCOM:Start", "Обнаружен поток: ожидания создания, ожидаем завершения.");
                    DateTime dtLastThread = DateTime.Now;
                    while (!bStop && ThreadIdleCreate.IsAlive)
                    {
                        if ((DateTime.Now - dtLastThread).TotalSeconds > 180 && ThreadIdleCreate.IsAlive)
                        {
                            dafLog.AddMessages("SmartCOM:Start", "Прерываем поток: ожидания создания.");
                            ThreadIdleCreate.Abort();
                        }
                        Thread.Sleep(100);
                    }
                    ThreadIdleCreate = null;
                }
                dafLog.AddMessages("SmartCOM:Start", "Запуск.");
                ThreadIdleCreate = new Thread(IdleCreate);
                ThreadIdleCreate.Start();
            }
            else
                dafLog.AddMessages("SmartCOM:Start", "Запуск отменён.");
        }

        private void IdleCreate()
        {
            if (iDisconnectCount > 3)
            {
                Release();
                Kill();
            }
            if (!IsReady)
            {
                bool bError = false;
                try
                {
                    // Создаём SmartCOM
                    SmartServer = new StServer();
                }
                catch (Exception Error)
                {
                    dafLog.AddMessages("SmartCOM:IdleCreate", "Ошибка, при запуске SmartCom: " + Error.Message);
                    SmartServer = null;
                    bError = true;
                }
                if (!bError)
                {
                    dafLog.AddMessages("SmartCOM:IdleCreate", "SmartCOM Ожидание запуска.");
                    while (!bStop && !IsReady)
                        Thread.Sleep(100);
                    if (!bStop && IsReady)
                    {
                        iDisconnectCount = 0;
                        dafLog.AddMessages("SmartCOM:IdleCreate", "SmartCOM Запущен");
                        // Объявляем обработчики для событий
                        SmartServer.Connected += new _IStClient_ConnectedEventHandler(Connected);
                        SmartServer.Disconnected += new _IStClient_DisconnectedEventHandler(Disconnected);
                        SmartServer.AddSymbol += new _IStClient_AddSymbolEventHandler(AddSymbol);
                        SmartServer.AddPortfolio += new _IStClient_AddPortfolioEventHandler(AddPortfolio);
                        SmartServer.SetPortfolio += new _IStClient_SetPortfolioEventHandler(SetPortfolio);
                        SmartServer.AddBar += new _IStClient_AddBarEventHandler(AddBar);
                        SmartServer.AddTick += new _IStClient_AddTickEventHandler(AddTick);
                        SmartServer.AddTrade += new _IStClient_AddTradeEventHandler(AddTrade);
                        SmartServer.UpdateOrder += new _IStClient_UpdateOrderEventHandler(UpdateOrder);
                        SmartServer.UpdateQuote += new _IStClient_UpdateQuoteEventHandler(UpdateQuote);
                        SmartServer.UpdateBidAsk += new _IStClient_UpdateBidAskEventHandler(UpdateBidAsk);
                    }
                }
            }
            if (!bStop && IsReady)
            {
                Thread.Sleep(1000);
                if (ThreadIdleConnect == null || (ThreadIdleConnect != null && !ThreadIdleConnect.IsAlive))
                {
                    dafLog.AddMessages("SmartCOM:IdleCreate", "Запускаем поток: Ожидания содинения с сервером котировок...");
                    ThreadIdleConnect = new Thread(IdleConnect);
                    ThreadIdleConnect.Start();
                }
                else
                    dafLog.AddMessages("SmartCOM:IdleCreate", "Обнаружен поток: Ожидания содинения с сервером котировок.");
            }
        }

        private void IdleConnect()
        {
            Connect();
            DateTime dtClockDisConnect = DateTime.Now;
            dafLog.AddMessages("SmartCOM:IdleConnect", "Запускаем ожидание, 30 секунд до " + dtClockDisConnect.AddSeconds(30).ToString());
            bool bStartReConnect = false; bDisconnected = false;
            while (!bStop && !IsConnected && !bStartReConnect && !bDisconnected)
            {
                Thread.Sleep(100);
                if ((DateTime.Now - dtClockDisConnect).TotalSeconds > 30)
                    bStartReConnect = true;
            }
            if (bStartReConnect && !IsConnected)
                Disconnected("Превышено время ожидания...");
        }

        private void Connect()
        {
            if (IsReady)
            {
                // Проверка на случай если соединение установлено.
                if (!IsConnected)
                {
                    // поспим секундочку :-)
                    Thread.Sleep(3000);
                    string sIp; short iPort;
                    if (Account.GetNext(out sIp, out iPort))
                    {
                        dafLog.AddMessages("SmartCOM:Connect", "Connect to ITServer: " + sIp + ":" + iPort + " Login: " + Account.Login);
                        try
                        {
                            SmartServer.connect(sIp, iPort, Account.Login, Account.Password);
                        }
                        catch (Exception Error)
                        {
                            iDisconnectCount = 666;
                            dafLog.AddMessages("SmartCOM:Connect", "Error, " + Error.Message);
                        }
                    }
                    else
                        dafLog.AddMessages("SmartCOM:Connect", "IP not found.");
                }
                else
                {
                    dafLog.AddMessages("SmartCOM:Connect", "Connection already established.");
                    Connected();
                }
            }
        }

        private void Connected()
        {
            iDisconnectCount = 0;
            dafLog.AddMessages("SmartCOM:Connected", "Connection established...");
            if (ChangedConnection != null)
                ChangedConnection();
            // Начинаем прослушку
            if (bFirst)
            {
                bFirst = false;
                // Запрашиваем справочники
                SmartServer.GetPrortfolioList();
                SmartServer.GetSymbols();
            }
            else
                ListenStart();
        }

        private void Disconnected(string reason)
        {
            iDisconnectCount++;
            bDisconnected = true;
            dafLog.AddMessages("SmartCOM:Disconnected", reason);
            if (ChangedConnection != null)
                ChangedConnection();
            ListenStop();
            if (!bStop)
                Init();
            else
                Dispose();
        }

        private void AddSymbol(int row, int nrows, string symbol, string short_name, string long_name, string type, int decimals, int lot_size, double punkt, double step, string sec_ext_id, string sec_exch_name, System.DateTime expiry_date, double days_before_expiry)
        {
            if (type != "OPT" && type != "OPTM")
                lock (AllTikers)
                    AllTikers.Add(new Tiker(type, symbol, short_name, long_name, step, punkt, decimals, sec_exch_name, expiry_date, (int)days_before_expiry));
            if (row == nrows - 1)
            {
                dafLog.AddMessages("SmartCOM:AddSymbol", "Tikers ready, Count: " + AllTikers.Count);
                if (ReadyTikers != null)
                    ReadyTikers();
            }
        }

        private void AddPortfolio(int row, int nrows, string portfolioName, string portfolioExch)
        {
            lock (AllPortfolios)
                AllPortfolios.Add(new PortfolioInfo(portfolioName, portfolioExch));
            if (row == nrows - 1)
            {
                dafLog.AddMessages("SmartCOM:AddPortfolio", "Portfolios ready, Count: " + AllPortfolios.Count);
                if (ReadyPortfolios != null)
                    ReadyPortfolios();
            }
        }

        private void SetPortfolio(string portfolio, double cash, double leverage, double comission, double saldo)
        {
            if (ChangedPortfolio != null)
                ChangedPortfolio(this, new PortfolioEventArgs(portfolio, cash, comission));
        }

        private void AddBar(string symbol, StClientLib.StBarInterval interval, System.DateTime datetime, double open, double high, double low, double close, double volume)
        {

        }

        private void AddTick(string symbol, System.DateTime datetime, double price, double volume, string tradeno)
        {

        }

        private void AddTrade(string portfolio, string symbol, string orderid, double price, double amount, System.DateTime datetime, string tradeno)
        {

        }

        private void UpdateOrder(string portfolio, string symbol, StClientLib.StOrder_State state, StClientLib.StOrder_Action action, StClientLib.StOrder_Type type, StClientLib.StOrder_Validity validity, double price, double amount, double stop, double filled, System.DateTime datetime, string orderid, string orderno, int status_mask)
        {

        }

        private void UpdateQuote(string symbol, System.DateTime datetime, double open, double high, double low, double close, double last, double volume, double size, double bid, double ask, double bidsize, double asksize, double open_int, double go_buy, double go_sell, double go_base, double go_base_backed, double high_limit, double low_limit, int trading_status)
        {

        }

        private void UpdateBidAsk(string symbol, int row, int nrows, double bid, double bidsize, double ask, double asksize)
        {

        }
    }
}
