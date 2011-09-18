using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Threading;
using System.Windows.Forms;
using StClientLib;

/// <summary Проверка функций SmartCOM>
/// Автор: Алексей Фомин, 2011 (c) DAFGroup
/// 
/// Сервера, IT Invest
/// Основной        82.204.220.34
/// Дополнительный  213.247.232.236
/// Only RTS        87.118.223.109
/// 
/// Тестовый        89.175.35.229
/// </summary>
namespace TestConnect
{
    public partial class TestForm : Form
    {
        private int InfoCookie;         // Индификатор приказа
        private Quote LastQuote;        // Котировка инструмента
        private List<Bar> InfoBars;     // Список баров для инструмента
        private StServer SmartServer;   // SmartCOM
        private List<Tiker> InfoTikers; // Список всех инструментов

        private DAFWriters Writers;     // Лог

        // Создан ли SmartCOM
        private bool IsReady { get { return (SmartServer != null); } }
        // Установлено ли соединение
        private bool IsConnected
        {
            get
            {
                bool bReturn = false;
                if (IsReady)
                {
                    try
                    {
                        bReturn = SmartServer.IsConnected();
                    }
                    catch (Exception Error)
                    {
                        ErrorLabel.Text = "Ошибка при проверке статуса соединения, " + Error.Message;
                    }
                }
                return bReturn;
            }
        }

        public TestForm()
        {
            Writers = new DAFWriters();

            InitializeComponent();
            ToDateTimePicker.Value = DateTime.Now.AddDays(-5);
            ToDateTimePicker.MaxDate = DateTime.Now;
            InfoBars = new List<Bar>();
            InfoTikers = new List<Tiker>();
            Text += ", Version: " + Info.GetVersion;
            Writers.WriteLine("Enegy", "log", "{0} Version: {1}", DateTime.Now, Info.GetVersion);
        }

        private void CreateButton_Click(object sender, EventArgs e)
        {
            Writers.WriteLine("Enegy", "log", "{0} Click: {1}", DateTime.Now, "Create");
            ErrorLabel.Text = "Create";
            if (!IsReady) // если SmartCOM не создан
            {
                try
                {
                    SmartServer = new StServer(); // Создать и назначить обработчики событий
                    SmartServer.Connected += new _IStClient_ConnectedEventHandler(SmartServer_Connected);
                    SmartServer.Disconnected += new _IStClient_DisconnectedEventHandler(SmartServer_Disconnected);
                    SmartServer.AddTick += new _IStClient_AddTickEventHandler(SmartServer_AddTick);
                    SmartServer.UpdateBidAsk += new _IStClient_UpdateBidAskEventHandler(SmartServer_UpdateBidAsk);
                    SmartServer.UpdateQuote += new _IStClient_UpdateQuoteEventHandler(SmartServer_UpdateQuote);
                    SmartServer.AddBar += new _IStClient_AddBarEventHandler(SmartServer_AddBar);
                    SmartServer.AddTickHistory += new _IStClient_AddTickHistoryEventHandler(SmartServer_AddTickHistory);
                    SmartServer.AddPortfolio += new _IStClient_AddPortfolioEventHandler(SmartServer_AddPortfolio);
                    SmartServer.UpdateOrder += new _IStClient_UpdateOrderEventHandler(SmartServer_UpdateOrder);
                    SmartServer.UpdatePosition += new _IStClient_UpdatePositionEventHandler(SmartServer_UpdatePosition);
                    SmartServer.SetPortfolio += new _IStClient_SetPortfolioEventHandler(SmartServer_SetPortfolio);
                    SmartServer.AddTrade += new _IStClient_AddTradeEventHandler(SmartServer_AddTrade);
                    SmartServer.AddSymbol += new _IStClient_AddSymbolEventHandler(SmartServer_AddSymbol);
                    SmartServer.SetMyClosePos += new _IStClient_SetMyClosePosEventHandler(SmartServer_SetMyClosePos);
                    SmartServer.SetMyOrder += new _IStClient_SetMyOrderEventHandler(SmartServer_SetMyOrder);
                    SmartServer.SetMyTrade += new _IStClient_SetMyTradeEventHandler(SmartServer_SetMyTrade);

                    SmartServer.OrderSucceeded += new _IStClient_OrderSucceededEventHandler(SmartServer_OrderSucceeded);
                    SmartServer.OrderFailed += new _IStClient_OrderFailedEventHandler(SmartServer_OrderFailed);
                    SmartServer.OrderCancelFailed += new _IStClient_OrderCancelFailedEventHandler(SmartServer_OrderCancelFailed);
                    SmartServer.OrderCancelSucceeded += new _IStClient_OrderCancelSucceededEventHandler(SmartServer_OrderCancelSucceeded);
                    SmartServer.OrderMoveFailed += new _IStClient_OrderMoveFailedEventHandler(SmartServer_OrderMoveFailed);
                    SmartServer.OrderMoveSucceeded += new _IStClient_OrderMoveSucceededEventHandler(SmartServer_OrderMoveSucceeded);

                    CreateButton.Enabled = false;
                }
                catch (Exception Error)
                {
                    ErrorLabel.Text = "Ошибка при создании, " + Error.Message;
                }
            }
            if (IsConnected) // если соединение установлено, вручную вызвать событие connected, для начала подписки
                SmartServer_Connected();
            else
                ReDrawStatus(); // иначе обновить статус соединения
        }
        private void CreateButton_EnabledChanged(object sender, EventArgs e)
        {
            ConnectButton.Enabled = !CreateButton.Enabled;
            DisconnectButton.Enabled = !CreateButton.Enabled;
            GetBarsButton.Enabled = !CreateButton.Enabled;
            RightPanel.Enabled = !CreateButton.Enabled;
        }

        private void ConnectButton_Click(object sender, EventArgs e)
        {
            Writers.WriteLine("Enegy", "log", "{0} Click: {1}", DateTime.Now, "Connect");
            ErrorLabel.Text = "Connect";
            if (IsReady) // если создан SmartCOM
            {
                if (!IsConnected) // и соединение не установлено
                {
                    try
                    {
                        // подключится к серверу
                        SmartServer.connect(IPTextBox.Text, 8090, LoginTextBox.Text, PasswordTextBox.Text);
                    }
                    catch (Exception Error)
                    {
                        ErrorLabel.Text = "Ошибка при подключении, " + Error.Message;
                    }
                }
                else // обновить статус соединения
                    ReDrawStatus();
            }
        }
        private void DisconnectButton_Click(object sender, EventArgs e)
        {
            Writers.WriteLine("Enegy", "log", "{0} Click: {1}", DateTime.Now, "Disconnect");
            ErrorLabel.Text = "Disconnect";
            if (IsConnected)
            {
                try
                {
                    // отказаться от подписок
                    SmartServer.CancelTicks(SymbolTextBox.Text);
                    SmartServer.CancelQuotes(SymbolTextBox.Text);
                    SmartServer.CancelBidAsks(SymbolTextBox.Text);
                    foreach (string tempPortfolio in PortfoliosComboBox.Items)
                        SmartServer.CancelPortfolio(tempPortfolio);
                }
                catch (Exception Error)
                {
                    ErrorLabel.Text = "Ошибка при завершении подписки, " + Error.Message;
                }
                try
                {
                    SmartServer.disconnect();
                }
                catch (Exception Error)
                {
                    ErrorLabel.Text = "Ошибка при отключении, " + Error.Message;
                }
            }
        }

        private void GetBarsButton_Click(object sender, EventArgs e)
        {
            ErrorLabel.Text = "";
            if (IsConnected && SymbolTextBox.Text != "") // если соединение установлено и указан инструмент
            {
                GetBarsButton.Enabled = false;
                InfoBarLabel.Text = "Start";
                InfoBars.Clear(); // Очистить список баров
                try
                {
                    // запросить 500 баров начиная с текущего
                    SmartServer.GetBars(SymbolTextBox.Text, StBarInterval.StBarInterval_60Min, DateTime.Now, 500);
                }
                catch (Exception Error)
                {
                    Writers.WriteLine("Enegy", "log", "{0} Ошибка в GetBars {1}", DateTime.Now, Error.Message);
                }
            }
        }
        private void GetBarsButton_EnabledChanged(object sender, EventArgs e)
        {
            ToDateTimePicker.Enabled = GetBarsButton.Enabled;
        }

        private void ReDrawStatus()
        {
            if (StatusLabel.InvokeRequired) // проверка на главный поток
                StatusLabel.BeginInvoke(new System.Windows.Forms.MethodInvoker(ReDrawStatus));
            else
            {   // Обновить статус соединения
                StatusLabel.Text = IsConnected ? "Connected" : IsReady ? "Disconnected" : "No Create";
                StatusLabel.ForeColor = IsConnected ? Color.Green : IsReady ? Color.Tomato : Color.DarkGray;
            }
        }

        private void UpDateQuote()
        {
            if (LastQuoteLabel.InvokeRequired) // проверка на главный поток
                LastQuoteLabel.BeginInvoke(new System.Windows.Forms.MethodInvoker(UpDateQuote));
            else
            {   // Обновить информацию по инструменту
                LastAskLabel.Text = "Ask: " + LastQuote.Ask + " (" + LastQuote.AskVolume + ")";
                LastBidLabel.Text = "Bid: " + LastQuote.Bid + " (" + LastQuote.BidVolume + ")";
                LastLabel.Text = LastQuote.LastClock.ToLongTimeString() + " " + LastQuote.LastPrice + " (" + LastQuote.LastVolume + ") -> " + (LastQuote.LastAction == StOrder_Action.StOrder_Action_Buy ? "B" : LastQuote.LastAction == StOrder_Action.StOrder_Action_Sell ? "S" : LastQuote.LastAction.ToString());
                LastQuoteLabel.Text = "Status: " + LastQuote.Status;
            }
        }

        private void ThreadBarsSave()
        {
            Writers.WriteLine("GetBars", SymbolTextBox.Text + ".Bars", "*** Save Start {0}", DateTime.Now);
            foreach (Bar tempBar in InfoBars)
                Writers.WriteLine("GetBars", SymbolTextBox.Text + ".Bars", "{0} -> {1}", InfoBars.IndexOf(tempBar), tempBar.ToString());
            Writers.WriteLine("GetBars", SymbolTextBox.Text + ".Bars", "*** Save Finish {0}", DateTime.Now);
            if (GetBarsButton.InvokeRequired)
                GetBarsButton.BeginInvoke(new System.Windows.Forms.MethodInvoker(delegate { GetBarsButton.Enabled = true; }));
            else
                GetBarsButton.Enabled = true;
        }

        #region Обработчики SmartCOM
        private void SmartServer_Connected()
        {
            // соединение установлено
            Writers.WriteLine("Enegy", "log", "{0} Connected: {1}", DateTime.Now, IsConnected.ToString());
            if (IsConnected)
            {
                try
                {
                    if (InfoTikers.Count == 0) // список инструментов пуст, запросить
                    {
                        Writers.WriteLine("Enegy", "log", "{0} Get: {1}", DateTime.Now, "Symbols");
                        SmartServer.GetSymbols();
                    }
                    Writers.WriteLine("Enegy", "log", "{0} Listen: {1}", DateTime.Now, "Ticks");
                    SmartServer.ListenTicks(SymbolTextBox.Text);    // подписаться на получение всех сделок
                    Writers.WriteLine("Enegy", "log", "{0} Listen: {1}", DateTime.Now, "BidAsks");
                    SmartServer.ListenBidAsks(SymbolTextBox.Text);  // подписаться на получение стакана
                    Writers.WriteLine("Enegy", "log", "{0} Listen: {1}", DateTime.Now, "Quotes");
                    SmartServer.ListenQuotes(SymbolTextBox.Text);   // подписаться на получение информации по инструменту
                    Writers.WriteLine("Enegy", "log", "{0} Get: {1}", DateTime.Now, "Prortfolios");
                    SmartServer.GetPrortfolioList();                // запросить список доступных счетов
                }
                catch (Exception Error)
                {
                    ErrorLabel.Text = "Ошибка при подписке, " + Error.Message;
                }
            }
            ErrorLabel.Text = "Connected";
            ReDrawStatus(); // обновить статус соединения
        }
        private void SmartServer_Disconnected(string reason)
        {
            // Соединение разорвано
            Writers.WriteLine("Enegy", "log", "{0} Disconnected: {1}", DateTime.Now, reason);
            ErrorLabel.Text = "Disconnected: " + reason;
            ReDrawStatus(); // обновить статус соединения
        }

        private void SmartServer_AddTick(string symbol, System.DateTime datetime, double price, double volume, string tradeno, StClientLib.StOrder_Action action)
        {
            if (LastQuote != null) // обновить котировку
            {
                long LastNo = 0; long.TryParse(tradeno, out LastNo);
                if (LastQuote.LastNo == 0)
                {
                    Writers.WriteLine("History", "Ticks", "*****************************");
                    Writers.WriteLine("History", "Ticks", "{0} Get: {1} From {2}, Count {3}, LastNo: {4}", DateTime.Now, "Ticks", datetime, 50000, LastNo);
                    Writers.WriteLine("History", "Ticks", "*****************************");
                    SmartServer.GetTrades(symbol, datetime, 50000); // запросить последние 500 сделок по инструменту
                }
                LastQuote.UpDate(datetime, price, volume, LastNo, action);
            }
        }
        private void SmartServer_UpdateBidAsk(string symbol, int row, int nrows, double bid, double bidsize, double ask, double asksize)
        {
            if (row == 0 && LastQuote != null) // обновить котировку
                LastQuote.UpDate(ask, asksize, bid, bidsize);
        }
        private void SmartServer_UpdateQuote(string symbol, System.DateTime datetime, double open, double high, double low, double close, double last, double volume, double size, double bid, double ask, double bidsize, double asksize, double open_int, double go_buy, double go_sell, double go_base, double go_base_backed, double high_limit, double low_limit, int trading_status)
        {
            if (LastQuote == null || (LastQuote != null && symbol != SymbolTextBox.Text))
            {
                LastQuote = new Quote(symbol, datetime, last, volume, trading_status, UpDateQuote); // создать котировку для инструмета
                if (InfoTikers.Any(tempTiker => tempTiker.Code == symbol)) // если выбранный инструмент, то изменить разрядность и шаг
                {
                    OrderPriceNumericUpDown.DecimalPlaces = InfoTikers.Find(tempTiker => tempTiker.Code == symbol).Decimals;
                    OrderPriceNumericUpDown.Increment = System.Convert.ToDecimal(InfoTikers.Find(tempTiker => tempTiker.Code == symbol).Step);
                }
            }
            else
                LastQuote.UpDate(trading_status); // Обновить статус торгов по инструменту
        }

        private void SmartServer_AddPortfolio(int row, int nrows, string portfolioName, string portfolioExch)
        {
            // доступен счёт
            Writers.WriteLine("Enegy", "log", "{0} Portfolio {1}/{2} {3}:{4} {5}", DateTime.Now, row, nrows, portfolioExch, portfolioName, (portfolioName.IndexOf("RF") > -1 ? "Listen" : ""));
            if (portfolioName.IndexOf("RF-01") > -1) // работаем только с РТС
            {
                if (PortfoliosComboBox.Items.IndexOf(portfolioName) == -1) // если данный счёт не известен то запомним
                {
                    PortfoliosComboBox.Items.Add(portfolioName);
                    if (PortfoliosComboBox.SelectedIndex == -1 && PortfoliosComboBox.Items.Count > 0)
                        PortfoliosComboBox.SelectedIndex = 0;
                }
                SmartServer.ListenPortfolio(portfolioName); // пдпишимся на прослушку
                try
                {
                    SmartServer.GetMyClosePos(portfolioName); // запросить закрытые позиции
                }
                catch (Exception Error)
                {
                    Writers.WriteLine("Enegy", "log", "{0} Ошибка в GetMyClosePos {1}, {2}", DateTime.Now, portfolioName, Error.Message);
                }
                try
                {
                    SmartServer.GetMyOrders(0, portfolioName); // запросить все приказы по счёту
                }
                catch (Exception Error)
                {
                    Writers.WriteLine("Enegy", "log", "{0} Ошибка в GetMyOrders {1}, {2}", DateTime.Now, portfolioName, Error.Message);
                }
                try
                {
                    SmartServer.GetMyTrades(portfolioName); // запросить все сделки по счёту
                }
                catch (Exception Error)
                {
                    Writers.WriteLine("Enegy", "log", "{0} Ошибка в GetMyTrades {1}, {2}", DateTime.Now, portfolioName, Error.Message);
                }
            }
        }

        private void SmartServer_UpdatePosition(string portfolio, string symbol, double avprice, double amount, double planned)
        {
            Writers.WriteLine("Portfolio", "Position", "{0} {1}:{2} Avg: {3} ({4}) Planed: {5}", DateTime.Now, portfolio, symbol, avprice, amount, planned);
        }

        private void SmartServer_SetPortfolio(string portfolio, double cash, double leverage, double comission, double saldo)
        {
            Writers.WriteLine("Portfolio", "Portfolio", "{0} {1} Cash: {2} Fee: {3} Saldo: {4} Leverage: {5}", DateTime.Now, portfolio, cash, comission, saldo, leverage);
        }
        
        private void SmartServer_UpdateOrder(string portfolio, string symbol, StClientLib.StOrder_State state, StClientLib.StOrder_Action action, StClientLib.StOrder_Type type, StClientLib.StOrder_Validity validity, double price, double amount, double stop, double filled, System.DateTime datetime, string orderid, string orderno, int status_mask, int cookie)
        {
            if (InfoCookie < cookie)
                InfoCookie = cookie;
            Writers.WriteLine("Portfolio", "Trade", "{0} Order {1}:{2}:[{3}]:{4}:{5} {6} Price: {7} ({8}/{9}) {10} Stop: {11} {12} {13} Mask:{14} {15}", DateTime.Now, portfolio, symbol, cookie, orderid, orderno, action, price, amount, filled, type, stop, datetime, state, status_mask, validity);
        }
        
        /*
        private void SmartServer_UpdateOrder(string portfolio, string symbol, StClientLib.StOrder_State state, StClientLib.StOrder_Action action, StClientLib.StOrder_Type type, StClientLib.StOrder_Validity validity, double price, double amount, double stop, double filled, System.DateTime datetime, string orderid, string orderno, int status_mask)
        {
            Writers.WriteLine("Portfolio", "Trade", "{0} Order {1}:{2}:{3}:{4} {5} Price: {6} ({7}/{8}) {9} Stop: {10} {11} {12} Mask:{13} {14}", DateTime.Now, portfolio, symbol, orderid, orderno, action, price, amount, filled, type, stop, datetime, state, status_mask, validity);
        }
        */
        private void SmartServer_AddTrade(string portfolio, string symbol, string orderid, double price, double amount, System.DateTime datetime, string tradeno)
        {
            Writers.WriteLine("Portfolio", "Trade", "{0} Trade {1}:{2}:{3}:{4} {5} Price: {6} ({7})", DateTime.Now, portfolio, symbol, orderid, tradeno, datetime, price, amount);
        }

        private void SmartServer_AddSymbol(int row, int nrows, string symbol, string short_name, string long_name, string type, int decimals, int lot_size, double punkt, double step, string sec_ext_id, string sec_exch_name, System.DateTime expiry_date, double days_before_expiry)
        {
            // добавить инструмент в список
            InfoTikers.Add(new Tiker(symbol, short_name, long_name, step, punkt, decimals, sec_ext_id, sec_exch_name, expiry_date, days_before_expiry));
            Writers.WriteLine("Enegy", "Symbol", "{0} {1}/{2} {3}:{4}:{5}:{6}:{7}:{8} {9} {10} {11} {12} {13} {14}", DateTime.Now, row, nrows, sec_ext_id, sec_exch_name, symbol, short_name, long_name, type, decimals, lot_size, punkt, step, expiry_date, days_before_expiry);
            if (symbol == SymbolTextBox.Text) // если выбранный инструмент, то изменить разрядность и шаг
            {
                OrderPriceNumericUpDown.DecimalPlaces = decimals;
                OrderPriceNumericUpDown.Increment = System.Convert.ToDecimal(step);
            }
        }

        private void SmartServer_SetMyClosePos(int row, int nrows, string portfolio, string symbol, double amount, double price_buy, double price_sell, System.DateTime postime, string buy_order, string sell_order)
        {
            Writers.WriteLine("History", "Closed", "{0} {1}/{2} {3}:{4} {5} ({6}) Buy[{7}:{8}] Sell[{9}:{10}]", DateTime.Now, row, nrows, portfolio, symbol, postime, amount, buy_order, price_buy, sell_order, price_sell);
        }
        
        private void SmartServer_SetMyOrder(int row, int nrows, string portfolio, string symbol, StClientLib.StOrder_State state, StClientLib.StOrder_Action action, StClientLib.StOrder_Type type, StClientLib.StOrder_Validity validity, double price, double amount, double stop, double filled, System.DateTime datetime, string id, string no, int cookie)
        {
            Writers.WriteLine("History", "Trade", "{0} Order {1}/{2} {3}:{4}:[{5}]:{6}:{7} {8} {9} Price: {10} ({11}/{12}) Stop: {13} {14} {15} {16}", DateTime.Now, row, nrows, portfolio, symbol, cookie, id, no, action, type, price, amount, filled, stop, state, datetime, validity);
        }
        
        /*
        private void SmartServer_SetMyOrder(int row, int nrows, string portfolio, string symbol, StClientLib.StOrder_State state, StClientLib.StOrder_Action action, StClientLib.StOrder_Type type, StClientLib.StOrder_Validity validity, double price, double amount, double stop, double filled, System.DateTime datetime, string id, string no)
        {
            Writers.WriteLine("History", "Trade", "{0} Order {1}/{2} {3}:{4}:{5}:{6} {7} {8} Price: {9} ({10}/{11}) Stop: {12} {13} {14} {15}", DateTime.Now, row, nrows, portfolio, symbol, id, no, action, type, price, amount, filled, stop, state, datetime, validity);
        }
        */
        private void SmartServer_SetMyTrade(int row, int nrows, string portfolio, string symbol, System.DateTime datetime, double price, double volume, string tradeno, StClientLib.StOrder_Action buysell, string orderno)
        {
            Writers.WriteLine("History", "Trade", "{0} Trade {1}/{2} {3} {4}:{5} {6} Price: {7} ({8}) {9}:{10}", DateTime.Now, row, nrows, datetime, portfolio, symbol, buysell, price, volume, tradeno, orderno);
        }

        private void SmartServer_AddBar(int row, int nrows, string symbol, StClientLib.StBarInterval interval, System.DateTime datetime, double open, double high, double low, double close, double volume)
        {
            if (datetime > ToDateTimePicker.Value)  // добавить новый бар в список
            {
                InfoBars.Add(new Bar(symbol, datetime, open, high, low, close, volume));
                InfoBarLabel.Text = datetime.ToShortDateString() + "\n" + datetime.ToLongTimeString() + " (" + InfoBars.Count + ")";
                Writers.WriteLine("History", "Bars", "{0}/{1} {2} {3} [O: {4} H: {5} L: {6} C: {7} V: {8}] {9}", row.ToString("000;"), nrows.ToString("000;"), datetime, symbol, open, high, low, close, volume, interval);
            }
            if (row == nrows - 1) // если пришёл последний бар в запросе
            {
                if (InfoBars.Count == 0 || datetime > ToDateTimePicker.Value) // если время последнего бара больше выбранного
                    try
                    {
                        // запросить 500 баров начиная с последнего в списке
                        SmartServer.GetBars(SymbolTextBox.Text, StBarInterval.StBarInterval_60Min, (InfoBars.Count == 0 ? DateTime.Now : InfoBars.Last().Clock.AddMinutes(-60)), 500);
                    }
                    catch (Exception Error)
                    {
                        Writers.WriteLine("Enegy", "log", "{0} Ошибка в GetBars {1}", DateTime.Now, Error.Message);
                    }
                else
                    new Thread(ThreadBarsSave).Start(); // иначе, считать, что получены все и сохранить список баров
            }
        }

        private void SmartServer_AddTickHistory(int row, int nrows, string symbol, System.DateTime datetime, double price, double volume, string tradeno, StClientLib.StOrder_Action action)
        {
            Writers.WriteLine("History", "Ticks", "{0}/{1} {2} {3} Price: {4} ({5}) {6} {7}", row, nrows, datetime, symbol, price, volume, tradeno, action);
            if (row == nrows - 1 && DateTime.Parse(datetime.ToShortDateString() + " 11:00:00") < datetime)
            {
                Writers.WriteLine("History", "Ticks", "*****************************");
                Writers.WriteLine("History", "Ticks", "{0} Get: {1} From {2}, Count {3}", DateTime.Now, "Ticks", datetime, 500);
                Writers.WriteLine("History", "Ticks", "*****************************");
                SmartServer.GetTrades(symbol, datetime, 500); // запросить последние 500 сделок по инструменту
            }
        }

        private void SmartServer_OrderSucceeded(int cookie, string orderid)
        {
            ErrorLabel.Text = "OrderAdd(" + orderid + ":" + cookie + ")";
            if (InfoCookie == cookie) // если индификатор равен последнему, запомнить Id, для отмены или перемещения
                OrderIdTextBox.Text = orderid;
            Writers.WriteLine("Portfolio", "Trade", "{0} Order:Place:Succeeded {1}:{2}", DateTime.Now, cookie, orderid);
        }
        private void SmartServer_OrderFailed(int cookie, string orderid, string reason)
        {
            ErrorLabel.Text = "OrderAdd(" + orderid + ":" + cookie + ") Failed: " + reason;
            if (InfoCookie == cookie)
                OrderIdTextBox.Text = "";
            Writers.WriteLine("Portfolio", "Trade", "{0} Order:Place:Failed {1}:{2} {3}", DateTime.Now, cookie, orderid, reason);
        }

        private void SmartServer_OrderCancelFailed(string orderid)
        {
            ErrorLabel.Text = "OrderCansel:" + orderid + " Failed";
            Writers.WriteLine("Portfolio", "Trade", "{0} Order:Cancel:Failed {1}", DateTime.Now, orderid);
        }
        private void SmartServer_OrderCancelSucceeded(string orderid)
        {
            ErrorLabel.Text = "OrderCansel:" + orderid;
            Writers.WriteLine("Portfolio", "Trade", "{0} Order:Cancel:Succeeded {1}", DateTime.Now, orderid);
        }

        private void SmartServer_OrderMoveFailed(string orderid)
        {
            ErrorLabel.Text = "OrderMove:" + orderid + " Failed";
            Writers.WriteLine("Portfolio", "Trade", "{0} Order:Move:Failed {1}", DateTime.Now, orderid);
        }
        private void SmartServer_OrderMoveSucceeded(string orderid)
        {
            ErrorLabel.Text = "OrderMove:" + orderid;
            Writers.WriteLine("Portfolio", "Trade", "{0} Order:Move:Succeeded {1}", DateTime.Now, orderid);
        }
        #endregion

        private void GetPriceBy_Click(object sender, EventArgs e)
        {
            if (LastQuote != null)
            {
                // Запомнить цену для выставления или перемещения приказа
                if (sender == LastLabel || sender == OrderPriceLabel)
                    OrderPriceNumericUpDown.Value = System.Convert.ToDecimal(LastQuote.LastPrice);
                if (sender == LastAskLabel)
                    OrderPriceNumericUpDown.Value = System.Convert.ToDecimal(LastQuote.Ask);
                if (sender == LastBidLabel)
                    OrderPriceNumericUpDown.Value = System.Convert.ToDecimal(LastQuote.Bid);
                OrderPriceMoveNumericUpDown.Value = OrderPriceNumericUpDown.Value;
            }
            else
                OrderPriceNumericUpDown.Value = 0;
        }

        private void PlaceOrder_Click(object sender, EventArgs e)
        {
            if (IsConnected && SymbolTextBox.Text != "" && OrderPriceNumericUpDown.Value > 0 && PortfoliosComboBox.Text != "")
            {
                InfoCookie++;
                OrderCookieLabel.Text = InfoCookie.ToString();
                Writers.WriteLine("Portfolio", "Trade", "{0} Order:Place {1} Price: {2}", DateTime.Now, InfoCookie, (double)OrderPriceNumericUpDown.Value);
                try
                {
                    // Выставить приказ
                    SmartServer.PlaceOrder(PortfoliosComboBox.Text, SymbolTextBox.Text, (sender == OrderBuyButton ? StOrder_Action.StOrder_Action_Buy : StOrder_Action.StOrder_Action_Sell), StOrder_Type.StOrder_Type_Limit, StOrder_Validity.StOrder_Validity_Day, (double)OrderPriceNumericUpDown.Value, 1, 0, InfoCookie);
                }
                catch (Exception Error)
                {
                    ErrorLabel.Text = "Ошибка при выставлении приказа: " + Error.Message;
                    Writers.WriteLine("Portfolio", "Trade", "{0} Order:Place {1}, {2}", DateTime.Now, InfoCookie, "Ошибка при выставлении приказа: " + Error.Message);
                }
            }
        }

        private void OrderMoveButton_Click(object sender, EventArgs e)
        {
            if (IsConnected && OrderIdTextBox.Text != "" && OrderPriceMoveNumericUpDown.Value > 0 && PortfoliosComboBox.Text != "")
            {
                Writers.WriteLine("Portfolio", "Trade", "{0} Order:Move {1} Price: {2}", DateTime.Now, OrderIdTextBox.Text, (double)OrderPriceMoveNumericUpDown.Value);
                try
                {
                    // переместить приказ
                    SmartServer.MoveOrder(PortfoliosComboBox.Text, OrderIdTextBox.Text, (double)OrderPriceMoveNumericUpDown.Value);
                }
                catch (Exception Error)
                {
                    ErrorLabel.Text = "Ошибка при перемещении приказа: " + Error.Message;
                    Writers.WriteLine("Portfolio", "Trade", "{0} Order:Move {1}, {2}", DateTime.Now, OrderIdTextBox.Text, "Ошибка при перемещении приказа: " + Error.Message);
                }
            }
        }

        private void OrderCanselButton_Click(object sender, EventArgs e)
        {
            if (IsConnected && OrderIdTextBox.Text != "" && SymbolTextBox.Text != "" && PortfoliosComboBox.Text != "")
            {
                Writers.WriteLine("Portfolio", "Trade", "{0} Order:Cansel {1}", DateTime.Now, OrderIdTextBox.Text);
                try
                {
                    // Отменить приказ
                    SmartServer.CancelOrder(PortfoliosComboBox.Text, SymbolTextBox.Text, OrderIdTextBox.Text);
                }
                catch (Exception Error)
                {
                    ErrorLabel.Text = "Ошибка при отмене приказа: " + Error.Message;
                    Writers.WriteLine("Portfolio", "Trade", "{0} Order:Cansel {1}, {2}", DateTime.Now, OrderIdTextBox.Text, "Ошибка при отмене приказа: " + Error.Message);
                }
            }
        }

        private void CanselAllButton_Click(object sender, EventArgs e)
        {
            if (IsConnected)
            {
                Writers.WriteLine("Portfolio", "Trade", "{0} CanselAll", DateTime.Now);
                try
                {
                    // Отменить все приказы
                    SmartServer.CancelAllOrders();
                }
                catch (Exception Error)
                {
                    ErrorLabel.Text = "Ошибка при отмене всех приказов: " + Error.Message;
                    Writers.WriteLine("Portfolio", "Trade", "{0} CanselAll {1}", DateTime.Now, "Ошибка при отмене всех приказов: " + Error.Message);
                }
            }
        }
    }
}
