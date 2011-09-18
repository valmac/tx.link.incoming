using System;
using System.IO;
using System.Data;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;
using System.Windows.Forms.DataVisualization.Charting;
using System.Net;
using System.Net.Sockets;
using System.Xml;
using System.Xml.Linq;


namespace tslight
{
	/// Description of MainForm.
	public partial class MainForm : Form
	{
		public string AppDir; // путь к папке приложения
		public bool bConnecting; // флаг процесса подключения к серверу
		public bool bDisconnecting; // флаг процесса отключения от сервера
		public bool bConnected; // флаг наличия подключения к серверу
		public bool bStarted; // флаг торгового процесса 
		public string sLogin; // логин пользователя для сервера Transaq
		public string sPassword; // пароль пользователя для сервера Transaq
		public string ServerIP; // IP адрес сервера Transaq
		public string ServerPort; // номер порта сервера Transaq
		public string ClientCode; // код клиента 
		
		public IList<string> DataList; // список строк с полученными от сервера данными
		public TXmlConnector oConn; // коннектор
		
		public bool LogFlag = true; // флаг записи лог-файла
		public StreamWriter LogFile; // переменная лог-файла
		
		public DataSet_tslight DTS; // DataSet для таблиц с данными

		public int SecurityID; // id выбранного инструмента
		public int TimeframeID; // id выбранного таймфрэйма
		public int PositionLimit; // размер позиции для стратегии
		public int CandleStatus; // статус процесса получения свечей
		public bool CurrentCandle; // флаг данных для текущей свечи
		public int ts_CurrentCandle = 0; // секундный "будильник" для запроса текущей свечи
		public int NowSeconds; // текущее время в секундах
		
		public FinancialChart Graph; // объект для отображения графика
		
		public DateTime LastDT; // время последней полученной свечи
		public StrategyClass Strategy; // объект стратегии
		public bool LongPos; // флаг длинной позиции
		public bool ShortPos; // флаг короткой позиции
		
		public char PointChar; // символ для замены точки в числах
		
		//================================================================================
		public MainForm()
		{
			// определение папки, в которой запущена программа
			string path = Application.ExecutablePath;
			AppDir = path.Substring(0, path.LastIndexOf('\\')+1);

			// The InitializeComponent() call is required for Windows Forms designer support.
			InitializeComponent();

			// создание объекта для отображение графика
			Graph = new FinancialChart(ctl_Chart, 100, AppDir);
			
			// определение разделителя в числах на компьютере (запятая или точка)
			PointChar = ',';
			string str = (1.2).ToString();
			if (str.IndexOf('.') > 0) PointChar = '.';
		}

		//================================================================================
		void MainFormLoad(object sender, EventArgs e)
		{
			string path = AppDir;

			// параметры по умолчанию
			ServerIP = "195.128.78.60";
			ServerPort = "3939";
			ClientCode = "";
			
			edt_Login.Text = sLogin;
			edt_Password.Text = sPassword;
			edt_IP.Text = ServerIP;
			edt_Port.Text = ServerPort;
			//txt_Client.Text = ClientCode;
			
			// открытие лог-файла
			if(LogFlag)
			{
				path = AppDir + "log.txt";
				if (File.Exists(path)) File.Delete(path);
				File.Create(path).Close();
				LogFile = File.AppendText(path);
				WriteLog("START LOGGING");
			}
			
			// список для хранения получаемых через коннектор данных до обработки программой
			DataList = new List<string> (1000);
			oConn = new TXmlConnector();
			oConn.DataList = DataList;

			Init_Data();

			// запуск будильника для получения данных текущей свечи
			ts_CurrentCandle = 1;
			
			// создание объекта стратегии
			Strategy = new StrategyClass(3,8);
		}
		
		//================================================================================
		void MainFormFormClosing(object sender, FormClosingEventArgs e)
		{
			if (bConnected || bConnecting)
			{
				Transaq_Disconnect();
			}
			if (LogFlag)
			{
				WriteLog("STOP LOGGING");
				LogFile.Close();
			}
		}

		//================================================================================
		public void WriteLog(string log_str)
		{
			if (LogFlag)
			{
				LogFile.WriteLine(DateTime.Now.ToString("HH:mm:ss")+" "+log_str);
			}
		}
		
		//================================================================================
		public void ShowStatus(string status_str)
		{
			// вывод сообщения в строке статуса формы
			txt_Status.Text = status_str;
			txt_Status.Refresh();
		}
		
		//================================================================================
		public void Init_Data()
		{
			// создание объекта DataSet с таблицами 
			DTS = new DataSet_tslight();
		}
		
		//================================================================================
		void Transaq_Connect()
		{
			// подключение коннектора к серверу Транзак
			bConnected = false;
			bConnecting = true;
			bDisconnecting = false;

			// чтение параметров из формы
			sLogin = edt_Login.Text;
			sPassword = edt_Password.Text;
			ServerIP = edt_IP.Text;
			ServerPort = edt_Port.Text;
			//ClientCode = txt_Client.Text;
			
			// проверка наличия параметров
			if (sLogin.Length == 0)
			{
				ShowStatus("Не указан логин");
				return;
			}
			if (sPassword.Length == 0)
			{
				ShowStatus("Не указан пароль");
				return;
			}
			if (ServerIP.Length == 0)
			{
				ShowStatus("Не указан IP-адрес");
				return;
			}
			if (ServerPort.Length == 0)
			{
				ShowStatus("Не указан порт");
				return;
			}
			
			// очистка таблиц с данными
			DTS.t_timeframe.Clear();
			DTS.t_security.Clear();
			
			Transaq_Reflect();
			
			// формирование текста команды
			string cmd = "<command id=\"connect\">";
			cmd = cmd + "<login>" + sLogin + "</login>";
			cmd = cmd + "<password>" + sPassword + "</password>";
			cmd = cmd + "<host>" + ServerIP + "</host>";
			cmd = cmd + "<port>" + ServerPort + "</port>";
			cmd = cmd + "<logsdir>" + AppDir + "</logsdir>";
			cmd = cmd + "";
			cmd = cmd + "</command>";

			// отправка команды
			string res = oConn.SendCommand(cmd);
			WriteLog("Transaq_Connect. " + oConn.debug_str);
			
			Transaq_Reflect();
		}
		
		//================================================================================
		void Transaq_Disconnect()
		{
			// отключение коннектора от сервера Транзак
			bConnected = false;
			bConnecting = false;
			bDisconnecting = true;
			
			// отсановка процесса торговли
			if (bStarted) Trading_Stop();
			
			Transaq_Reflect();
			
			string cmd = "<command id=\"disconnect\"/>";

			string res = oConn.SendCommand(cmd);
			WriteLog("Transaq_Disconnect. " + oConn.debug_str);

			Transaq_Reflect();
		}
		
		//================================================================================
		void Transaq_History(int HistoryLength, bool ResetFlag)
		{
			// запрос исторических данных для инструмента
			string cmd = "<command id=\"gethistorydata\" ";
			cmd = cmd + "secid=\"" + SecurityID.ToString() + "\" ";
			cmd = cmd + "period=\"" + TimeframeID.ToString() + "\" ";
			cmd = cmd + "count=\"" + HistoryLength.ToString() + "\" ";
			string s = "false";
			if (ResetFlag) s = "true";
			cmd = cmd + "reset=\""+s+"\"/>";
			
			string res = oConn.SendCommand(cmd);
			WriteLog("Transaq_History. cmd = " + cmd);
		}
		
		//================================================================================
		void Transaq_Reflect()
		{
			// отображение состояния подключения на форме
			string status = "offline";
			string button = "Подключить";
			string msg = "";
			Color clr = Color.FromArgb(212,208,200);
			
			if (bConnected)
			{
				status = "online";
				button = "Отключить";
				clr = Color.FromArgb(127,200,127);
			}
			else
			{
				if (bConnecting)
				{
					status = "connect";
					button = "Подключаю";
					msg = "Подключение к серверу...";
					clr = Color.FromArgb(200,127,127);
				}
				if (bDisconnecting)
				{
					status = "disconn";
					button = "Отключаю";
					msg = "Отключение от сервера...";
					clr = Color.FromArgb(200,127,127);
				}
			}
			txt_Connect.Text = status;
			btn_Connect.Text = button;
			//txt_Connect.BackColor = clr;
			//ctl_Connect.BackColor = clr;

			btn_Connect.Refresh();
			txt_Connect.Refresh();
			ctl_Connect.Refresh();
			if (msg.Length > 0) ShowStatus(msg);
		}
		
		//================================================================================
		public void Transaq_HandleData(string data)
		{
			// обработка данных, полученных коннектором от сервера Транзак
			string sTime = DateTime.Now.ToString("HH:mm:ss.ff");
			string info = "";

			// включить полученные данные в строку вывода в лог-файл
			//info = info + data;
			
			// создание объекта для работы с XML-форматом
			XmlReaderSettings xs = new XmlReaderSettings();
			xs.IgnoreWhitespace = true;
			xs.ConformanceLevel = ConformanceLevel.Fragment;
			xs.ProhibitDtd = false;
			XmlReader xr = XmlReader.Create(new System.IO.StringReader(data), xs);
			string section = "";
			string line = "";
			string str = "";
			string ename = "";
			string evalue = "";
			string attr = "";
			//string values = "";

			// обработка "узлов" 
			while (xr.Read())
			{
				switch (xr.NodeType)
				{
					case XmlNodeType.Element:
					case XmlNodeType.EndElement:
						ename = xr.Name; break;
					case XmlNodeType.Text:
					case XmlNodeType.CDATA:
					case XmlNodeType.Comment:
					case XmlNodeType.XmlDeclaration:
						evalue = xr.Value; break;
					case XmlNodeType.DocumentType:
						ename = xr.Name; evalue = xr.Value; break;
					default: break;
				}
				
				//................................................................................
				// определяем узел верхнего уровня - "секцию"
				if (xr.Depth == 0)
				{
					if (xr.NodeType == XmlNodeType.Element)
					{
						section = ename;
						line = "";
						str = "";
						for (int i=0; i<xr.AttributeCount; i++)
						{
							str = str + xr.GetAttribute(i) + ";";
						}
					}
					if (xr.NodeType == XmlNodeType.EndElement)
					{
						//line = "";
						//section = "";
					}
					if (xr.NodeType == XmlNodeType.Text) 
					{
						str = str + evalue + ";";
					}
				}
				//................................................................................
				// данные для рынков
				if (section == "markets")
				{
					//xe = (XElement)XNode.ReadFrom(xr);
					
					if (ename == "market")
					{
						if (xr.NodeType == XmlNodeType.Element) 
						{
							line = "";
							str = "";
							for (int i=0; i<xr.AttributeCount; i++)
							{
								str = str + xr.GetAttribute(i) + ";";
							}
						}
						if (xr.NodeType == XmlNodeType.EndElement) 
						{
							line = "add market: " + str;
							str = "";
						}
						if (xr.NodeType == XmlNodeType.Text) 
						{
							str = str + evalue + ";";
						}
					}
				}
				//................................................................................
				// данные для таймфреймов
				if (section == "candlekinds")
				{
					if (ename == "kind")
					{
						if (xr.NodeType == XmlNodeType.Element) 
						{
							line = "";
							str = "";
						}
						if (xr.NodeType == XmlNodeType.EndElement) 
						{
							line = "add kind: " + str;
							Add_Timeframe(str);
							str = "";
						}
					}
					else
					{
						if (xr.NodeType == XmlNodeType.Text) 
						{
							str = str + evalue + ";";
						}
					}
				}
				//................................................................................
				// данные для инструментов
				if (section == "securities")
				{
					if (ename == "security")
					{
						if (xr.NodeType == XmlNodeType.Element) 
						{
							line = "";
							str = "";
							for (int i=0; i<xr.AttributeCount; i++)
							{
								str = str + xr.GetAttribute(i) + ";";
							}
						}
						if (xr.NodeType == XmlNodeType.EndElement) 
						{
							line = "add security: " + str;
							Add_Security(str);
							str = "";
						}
					}
					else
					{
						if (xr.NodeType == XmlNodeType.Element) 
						{
							for (int i=0; i<xr.AttributeCount; i++)
							{
								str = str + xr.GetAttribute(i) + ";";
							}
						}
						if (xr.NodeType == XmlNodeType.Text) 
						{
							str = str + evalue + ";";
						}
					}
				}
				//................................................................................
				// данные по свечам
				if (section == "candles")
				{
					if (ename == "candles")
					{
						if (xr.NodeType == XmlNodeType.Element) 
						{
							line = "";
							for (int i=0; i<xr.AttributeCount; i++)
							{
								str = str + xr.GetAttribute(i) + ";";
							}
							// определение параметров подкачки свечей: бумага, период, статус
							string[] attrs = str.Split(';');
							if (attrs.Length > 2) 
							{
								//if (attrs[2] == "1") CandleStatus = 1;
								CandleStatus = int.Parse(attrs[2]);
							}
						}
						if (xr.NodeType == XmlNodeType.EndElement) 
						{
							// заказанное количество свечей получено
							if (CandleStatus == 1)
							{
								if (!CurrentCandle)
								{
									Graph.LoadData(DTS.t_candle);
									LastDT = DTS.t_candle[DTS.t_candle.Count-1].date;
									Strategy.LastDT = LastDT.AddDays(-10);
									CurrentCandle = true;
								}
							}
							// продолжение следует
							if (CandleStatus == 2)
							{

							}
						}
					}
					if (ename == "candle")
					{
						if (xr.NodeType == XmlNodeType.Element) 
						{
							line = "";
							str = "";
							for (int i=0; i<xr.AttributeCount; i++)
							{
								str = str + xr.GetAttribute(i) + ";";
							}
							if (CurrentCandle)
							{
								line = "current candle: " + str;
								Current_Candle(str);
							}
							else
							{
								line = "add candle: " + str;
								Add_Candle(str);
							}
						}
					}
				}
				//................................................................................
				// данные по клиенту
				if (section == "client")
				{
					if (ename == "client")
					{
						if (xr.NodeType == XmlNodeType.Element)
						{
							line = "";
							str = "";
							for (int i=0; i<xr.AttributeCount; i++)
							{
								str = str + xr.GetAttribute(i) + ";";
							}
							// определение параметров клиента
							string[] с_attrs = str.Split(';');
							if (с_attrs.Length > 0) 
							{
								ClientCode = с_attrs[0];
							}
							line = "add client: " + str;
						}
					}
					else
					{
						line = "";
						if (xr.NodeType == XmlNodeType.Text) 
						{
							str = str + evalue + ";";
							line = "set: " + ename + "=" + evalue;
						}
					}
				}
				//................................................................................
				// данные для позиций
				if (section == "positions")
				{
					line = "";
					if (xr.NodeType == XmlNodeType.Text) 
					{
						line = ename + ": " + evalue;
					}
					
				}
				//................................................................................
				if (section == "overnight")
				{
					if (xr.NodeType == XmlNodeType.Element)
					{
						line = "";
						str = "";
						for (int i=0; i<xr.AttributeCount; i++)
						{
							str = str + "<" + xr.GetAttribute(i) + ">;";
						}
						line = "set overnight status: " + str;
					}
				}
				//................................................................................
				// данные о статусе соединения с сервером
				if (section == "server_status")
				{
					if (xr.NodeType == XmlNodeType.Element)
					{
						line = "";
						str = "";
						for (int i=0; i<xr.AttributeCount; i++)
						{
							attr = xr.GetAttribute(i);
							str = str + i.ToString()+":<" + attr + ">;";
							if (i == 0)
							{
								if (attr == "true") 
								{
									str = str + "attr:" + attr + ";";
									if (bConnecting && !bConnected)
									{
										Connect_Open();
									}
									bConnecting = false;
									bConnected = true;
								}
								if (attr == "false") 
								{
									str = str + "attr:" + attr + ";";
									if (bDisconnecting)
									{
										Connect_Close();
									}
									bDisconnecting = false;
									bConnected = false;
								}
								Transaq_Reflect();
							}
						}
						line = "set server_status: " + str;
					}
					if (xr.NodeType == XmlNodeType.Text)
					{
						bConnecting = false;
						Transaq_Reflect();
						ShowStatus("Server status: "+evalue);
					}
				}
				//................................................................................
				if (line.Length > 0)
				{
					//line = new string(' ',xr.Depth*2) + line;
					if (info.Length > 0) info = info + (char)13 + (char)10;
					info = info + line;
				}
			}
			if (info.Length > 0) WriteLog(info);
			// вывод дополнительной информации для удобства отладки
			//txt_Info.Text = info;
		}
		
		//================================================================================
		void Timer_Handler_Tick(object sender, EventArgs e)
		{
			// обработчик таймера для выполнения текущих процессов
			DateTime now_dt = DateTime.Now;
			NowSeconds = (now_dt.Hour * 60 + now_dt.Minute) * 60 + now_dt.Second;
				
			if (DataList.Count > 0)
			{
				// обработка полученных данных
				string data = DataList[0];
				DataList.RemoveAt(0);
				Transaq_HandleData(data);
			}
			
			if (bStarted)
			{
				if (ts_CurrentCandle > 0 && ts_CurrentCandle < NowSeconds)
				{
					// подкачка текущей свечи
					if (CurrentCandle)
					{
						Transaq_History(1, true);
					}
					ts_CurrentCandle = NowSeconds + 1;
				}
				if (LastDT > Strategy.LastDT)
				{
					// обработка стратегии при появлении новой законченной свечи
					Handle_Strategy();
					Strategy.LastDT = LastDT;
				}
			}
		}

		//================================================================================
		public void Add_Timeframe(string data)
		{
			// добавление записи о таймфрейме
			string[] tf = data.Split(';');
			if (tf.Length < 3) return;
			
			int id = int.Parse(tf[0]);
			int length = int.Parse(tf[1]);
			string name = tf[2];
			
			DTS.t_timeframe.Add_Row(id, length, name);
		}
		
		//================================================================================
		public void Add_Security(string data)
		{
			// добавление записи об инструменте
			string[] sec = data.Split(';');
			if (sec.Length < 14) return;
			
			int id = int.Parse(sec[0]);
			bool active = bool.Parse(sec[1]);
			string code = sec[2];
			string name = sec[3];
			int decimals = int.Parse(sec[4]);
			int market = int.Parse(sec[5]);
			string type = sec[6];
			//bool usecredit = bool.Parse(sec[7]);
			//bool bymarket = bool.Parse(sec[8]);
			//bool nosplit = bool.Parse(sec[9]);
			//bool immorcancel = bool.Parse(sec[10]);
			//bool cancelbalance = bool.Parse(sec[11]);
			double minstep = double.Parse(sec[12].Replace('.',PointChar));
			int lotsize = int.Parse(sec[13]);
			
			// ограничимся инструментами ММВБ, только акции
			if (market == 1 && active && type == "SHARE") 
			{
				DTS.t_security.Add_Row(id, name, code, lotsize);
			}
		}
		
		//================================================================================
		public void Add_Candle(string data)
		{
			// добавление записи для свечи
			string[] candle = data.Split(';');
			if (candle.Length < 6) return;
			
			DateTime dt = DateTime.Parse(candle[0]);
			double open = double.Parse(candle[1].Replace('.',PointChar));
			double close = double.Parse(candle[2].Replace('.',PointChar));
			double high = double.Parse(candle[3].Replace('.',PointChar));
			double low = double.Parse(candle[4].Replace('.',PointChar));
			int volume = int.Parse(candle[5]);
			
			DataRow_candle row = DTS.t_candle.Add_Row(dt, open, high, low, close, volume);
		}
		
		//================================================================================
		public void Current_Candle(string data)
		{
			// обработка данных для текущей свечи
			string[] candle = data.Split(';');
			if (candle.Length < 6) return;
			
			DateTime dt = DateTime.Parse(candle[0]);
			double open = double.Parse(candle[1].Replace('.',PointChar));
			double close = double.Parse(candle[2].Replace('.',PointChar));
			double high = double.Parse(candle[3].Replace('.',PointChar));
			double low = double.Parse(candle[4].Replace('.',PointChar));
			int volume = int.Parse(candle[5]);

			DataRow_candle last = DTS.t_candle[DTS.t_candle.Count-1];

			// новые данные по текущей свече
			if (dt == last.date)
			{
				last.open = open;
				last.high = high;
				last.low = low;
				last.close = close;
				last.volume = volume;
				Graph.LoadPoint(Graph.period-1, last);
			}
			// данные для новой свечи
			if (dt > last.date)
			{
				LastDT = last.date;
				DTS.t_candle.Add_Row(dt, open, high, low, close, volume);
				Graph.LoadData(DTS.t_candle);
				SetMarkers();
			}
			ShowStatus(DateTime.Now.ToString("HH:mm:ss")+"   Close: "+close.ToString());
		}
		
		//================================================================================
		void Connect_Open()
		{
			// процедура при установлении соедниения с сервером Транзак
			dg_Security.DataSource = DTS.t_security;
			edt_Security_Code.Text = "GAZP";
			edt_Position_Limit.Text = PositionLimit.ToString();
			
			//lst_Timeframe.SelectedIndex = -1;
			lst_Timeframe.Items.Clear();
			for (int i=0; i<DTS.t_timeframe.Count; i++)
			{
				lst_Timeframe.Items.Add(DTS.t_timeframe[i].timeframe_name);
			}
			lst_Timeframe.SelectedIndex = 0;
			txt_Client.Text = ClientCode;
			ShowStatus("Подключение установлено");
		}
		
		//================================================================================
		void Connect_Close()
		{
			// процедура при отсоедниении от сервера Транзак
			DTS.t_security.Clear();
			dg_Security.Refresh();
			ShowStatus("Подключение разъединено");
		}
		
		//================================================================================
		void Trading_Start()
		{
			// начало процесса торговли
			if (!bConnected)
			{
				ShowStatus("Нет подключения к серверу");
				return;
			}
			bool bStart = false;
			bool bSecurity = false;
			bool bTimeframe = false;

			string code = edt_Security_Code.Text;
			SecurityID = 0;
			if (code.Length > 0)
			{
				for (int i=0; i<DTS.t_security.Count; i++)
				{
					if (DTS.t_security[i].security_code == code)
					{
						SecurityID = DTS.t_security[i].security_id;
						bSecurity = true;
						break;
					}
				}
			}
			if (!bSecurity)
			{
				ShowStatus("Нет инструмента с заданным кодом (" + code + ")");
				return;
			}
			if (lst_Timeframe.SelectedIndex >= 0)
			{
				TimeframeID = DTS.t_timeframe[lst_Timeframe.SelectedIndex].timeframe_id;
				bTimeframe = true;
			}
			if (!bTimeframe)
			{
				ShowStatus("Нет данных по таймфрейму");
				return;
			}
			PositionLimit = 1;
			edt_Position_Limit.Text = PositionLimit.ToString();
			
			bStart = bSecurity && bTimeframe;
			
			if (bStart)
			{
				btn_Start.Text = "Стоп";
				bStarted = true;
				ts_CurrentCandle = 1;
				CurrentCandle = false;
				
				Enable_Controls(false);
				//ShowStatus("Трэйдинг запущен ("+SecurityID.ToString()+"/"+TimeframeID.ToString()+")");
				ShowStatus("Трэйдинг запущен");

				DTS.t_candle.Clear();
				Transaq_History(100, true);
			}
		}
		
		//================================================================================
		void Trading_Stop()
		{
			// окончание процесса торговли
			Enable_Controls(true);

			btn_Start.Text = "Старт";
			bStarted = false;
			ts_CurrentCandle = 0;
			
			ShowStatus("Трэйдинг остановлен");
		}
		
		//================================================================================
		void Send_Order(string buysell, int sec_id, int qty)
		{
			// отправка заявки на сервер Транзак
			string cmd = "<command id=\"neworder\"> ";
			cmd = cmd + "<secid>" + sec_id.ToString() + "</secid> ";
			cmd = cmd + "<client>" + ClientCode + "</client> ";
			cmd = cmd + "<quantity>" + qty.ToString() + "</quantity> ";
			cmd = cmd + "<buysell>" + buysell + "</buysell> ";
			cmd = cmd + "<bymarket/> ";
			cmd = cmd + "</command>";
			string res = "order command not send";
			
			ShowStatus("Send order: " + cmd);
			//res = oConn.SendCommand(cmd);
			WriteLog("Send_Order. cmd = " + cmd + ", result = " + res);
			txt_Info.Text = "Send_Order. cmd = " + cmd + ", result = " + res;
		}
		//================================================================================
		void SetMarkers()
		{
			// отрисовка маркеров на графике
			// маркеры покупок привязаны к невидимому графику для отображения ниже свечей
			Graph.SetMarkers(Strategy.nBuy, AppDir+"image/up.bmp", "Long", "Buy");
			Graph.SetMarkers(Strategy.nSell, AppDir+"image/down.bmp", "", "");
			Graph.SetMarkers(Strategy.nShort, AppDir+"image/down.bmp", "Short", "");
			Graph.SetMarkers(Strategy.nCover, AppDir+"image/up.bmp", "", "Buy");
		}
		//================================================================================
		void Handle_Strategy()
		{
			// обработка стратегии
			Color clrNo = Color.FromArgb(212,208,200);
			Color clrOpen = Color.FromArgb(150,250,150);
			Color clrClose = Color.FromArgb(250,150,150);

			if (LastDT <= Strategy.LastDT) return;
			
			Strategy.Handle(DTS.t_candle);
			SetMarkers();

			txt_LongEntry.Text = "";
			txt_LongExit.Text = "";
			txt_ShortEntry.Text = "";
			txt_ShortExit.Text = "";
			lbl_Long.BackColor = clrNo;
			lbl_Short.BackColor = clrNo;
			
			// время срабатывания сигналов
			DateTime zdt = DateTime.Now.AddDays(-10);
			if (Strategy.BuyDT > zdt) txt_LongEntry.Text = Strategy.BuyDT.ToString("HH:mm");
			if (Strategy.SellDT > zdt) txt_LongExit.Text = Strategy.SellDT.ToString("HH:mm");
			if (Strategy.ShortDT > zdt) txt_ShortEntry.Text = Strategy.ShortDT.ToString("HH:mm");
			if (Strategy.CoverDT > zdt) txt_ShortExit.Text = Strategy.CoverDT.ToString("HH:mm");
			
			if (!Strategy.PrevLongPos && Strategy.LongPos && !LongPos)
			{
				// open long position
				LongPos = true;
				WriteLog("Buy - Open long");
				txt_LongEntry.BackColor = clrOpen; 
				
				Send_Order("B", SecurityID, PositionLimit);
			}
			if (Strategy.PrevLongPos && !Strategy.LongPos && LongPos)
			{
				// close long position
				LongPos = false;
				WriteLog("Sell - Close long");
				txt_LongEntry.BackColor = clrNo; 
				txt_LongExit.BackColor = clrClose; 
				
				Send_Order("S", SecurityID, PositionLimit);
			}
			if (!Strategy.PrevShortPos && Strategy.ShortPos && !ShortPos)
			{
				// open short position
				ShortPos = true;
				WriteLog("Short - Open short");
				txt_ShortEntry.BackColor = clrOpen; 
				
				Send_Order("S", SecurityID, PositionLimit);
			}
			if (Strategy.PrevShortPos && !Strategy.ShortPos && ShortPos)
			{
				// close short position
				ShortPos = false;
				WriteLog("Cover - Close short");
				txt_ShortEntry.BackColor = clrNo; 
				txt_ShortExit.BackColor = clrClose; 
				
				Send_Order("B", SecurityID, PositionLimit);
			}
			if (Strategy.LongPos)
			{
				lbl_Long.BackColor = clrOpen;
				txt_LongExit.BackColor = clrNo; 
				txt_LongExit.Text = "";
			}
			if (Strategy.ShortPos) 
			{
				lbl_Short.BackColor = clrOpen;
				txt_ShortExit.BackColor = clrNo; 
				txt_ShortExit.Text = "";
			}
		}
		//================================================================================
		void Enable_Controls(bool bEnable)
		{
			// установка состояния элементов управления
			edt_Security_Code.Enabled = bEnable;
			btn_Security_Select.Enabled = bEnable;
			txt_Selected_Code.Enabled = bEnable;
			lst_Timeframe.Enabled = bEnable;
		}
		//================================================================================
		void btn_Connect_Click(object sender, EventArgs e)
		{
			// нажата кнопка подключения
			if (bConnected || bConnecting)
			{
				Transaq_Disconnect();
			}
			else
			{
				if (!bConnecting) Transaq_Connect();
			}
			
		}
		//================================================================================
		void dg_Security_SelectionChanged(object sender, EventArgs e)
		{
			if (dg_Security.SelectedRows.Count > 0)
			{

			}
		}
		//================================================================================
		void btn_Security_Select_Click(object sender, EventArgs e)
		{
			// нажата кнопка выбора интсрумента из списка
			if (dg_Security.SelectedRows.Count > 0)
			{
				DataGridViewRow row = dg_Security.SelectedRows[0];
				string code = row.Cells[2].Value.ToString();

				txt_Selected_Code.Text = code;
				edt_Security_Code.Text = code;
			}
		}
		//================================================================================
		void lst_Timeframe_SelectedIndexChanged(object sender, EventArgs e)
		{
			TimeframeID = DTS.t_timeframe[lst_Timeframe.SelectedIndex].timeframe_id;
		}
		//================================================================================
		void btn_Start_Click(object sender, EventArgs e)
		{
			// нажата кнопка начала торговли
			if (bStarted)
			{
				Trading_Stop();
			}
			else
			{
				Trading_Start();
			}
		}
		//================================================================================
		void btn_Buy_Click(object sender, EventArgs e)
		{
			if (bStarted)
			{
				Send_Order("B", SecurityID, PositionLimit);
			}
			else
			{
				ShowStatus("Трэйдинг не включен, нажмите кнопку <Старт>");
			}
		}
		//================================================================================
		void btn_Sell_Click(object sender, EventArgs e)
		{
			if (bStarted)
			{
				Send_Order("S", SecurityID, PositionLimit);
			}
			else
			{
				ShowStatus("Трэйдинг не включен, нажмите кнопку <Старт>");
			}
		}
		//================================================================================
		void btn_Test_Click(object sender, EventArgs e)
		{
			ShowStatus("Нажата кнопка Тест.");
		}
		
		//================================================================================

		
	}
}
