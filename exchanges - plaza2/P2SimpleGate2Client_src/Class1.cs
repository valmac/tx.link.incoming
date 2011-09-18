using System;
using System.IO;
using System.Text;
using System.Runtime.InteropServices;
using P2ClientGate;
using System.Threading;

namespace P2SimpleGate2Client
{
	/// <summary>
	/// Summary description for Class1.
	/// </summary>
	public class Class1
	{
		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		static void Main(string[] args)
		{
			//
			// TODO: Add code to start application here
			//
			Client cl = new Client();
			cl.Start(args);
			Thread procMsgThread = new Thread(new ThreadStart(cl.ProcessConsole));
			procMsgThread.Start();
			cl.Run();
			procMsgThread.Join();
		}
	}

	class Client
	{
		private bool m_stop = false;
		private bool m_opened = false;
		CP2Connection m_conn;
		CP2DataStream m_stream;
		CP2BLMessageFactory m_msgs;
		StreamWriter m_logFile;
		String m_destAddr = "";

		public void Start(string[] args)
		{
			// Объект "соединение" и параметры соединения с приложением P2MQRouter
			m_conn = new CP2ConnectionClass();
			m_conn.Host = "localhost";
			m_conn.Port = 4001;
			m_conn.AppName = "P2SimpleGate2Client";
			IP2ConnectionEvent_ConnectionStatusChangedEventHandler connStatusHandler = new IP2ConnectionEvent_ConnectionStatusChangedEventHandler(ConnectionStatusChanged);
			m_conn.ConnectionStatusChanged += connStatusHandler;

			// создаем фабрику сообщений
			m_msgs = new CP2BLMessageFactory();
			m_msgs.Init("P2ClientGate.ini","");

			// создаем объект "входящий поток репликации"
			m_stream = new CP2DataStreamClass();
			m_stream.DBConnString = "P2DBSQLite.dll;;P2SimpleReplClient.db";
			m_stream.type = TRequestType.RT_COMBINED_DYNAMIC;
			m_stream.StreamName = "GATE2_REPL";
			
			// регистрируем интерфейсы обратного вызова для получения данных
			IP2DataStreamEvents_StreamStateChangedEventHandler StateHandler = new IP2DataStreamEvents_StreamStateChangedEventHandler(StreamStateChanged);
			m_stream.StreamStateChanged += StateHandler;
			IP2DataStreamEvents_StreamDataInsertedEventHandler InsHandler = new IP2DataStreamEvents_StreamDataInsertedEventHandler(StreamDataInserted);
			m_stream.StreamDataInserted += InsHandler;
			IP2DataStreamEvents_StreamDataUpdatedEventHandler UpdHandler = new IP2DataStreamEvents_StreamDataUpdatedEventHandler(StreamDataUpdated);
			m_stream.StreamDataUpdated += UpdHandler;
			IP2DataStreamEvents_StreamDataDeletedEventHandler DelHandler = new IP2DataStreamEvents_StreamDataDeletedEventHandler(StreamDataDeleted);
			m_stream.StreamDataDeleted += DelHandler;
			
			for( int i = 0; i < args.Length; ++i )
			{
				if( i == 0 )
				{
					m_stream.DBConnString = args[i];
					Console.WriteLine("Using DB connection string: {0}", m_stream.DBConnString);
				}
				if( i == 1 )
				{
					m_destAddr = args[i];
				}
			}
		}

		// обработка ввода пользователя 
		public void ProcessConsole()
		{
			System.Int32 connected = System.Convert.ToInt32(TConnectionStatus.CS_CONNECTION_CONNECTED);
			while( !m_stop )
			{
				Console.WriteLine("0 - EXIT");
				Console.WriteLine("1 - send info");
				Console.Write("Enter command:");
				string cmd = Console.ReadLine();
				if( cmd == "0" )
				{
					m_stop = true;
				}
				else if( cmd == "1" )
				{
					ProcessInfo();
				}
			}
		}

		// добавление сообщения - запроса на задержку исполнения
		void ProcessInfo()
		{
			if (m_destAddr == "")
			{
				m_destAddr = m_conn.ResolveService("GATE2_SVC");
				if (m_destAddr == "")
				{
					Console.WriteLine("Connection to server not completed - no dest address available");
					return;
				}
			}
			Console.WriteLine("Add info params");
			try
			{
				// создаем и заполняем сообщение
				CP2BLMessage msg = m_msgs.CreateMessageByName("Info");
				msg.DestAddr = m_destAddr;
				msg.set_Field("P2_Category", "p2spotgate2");
				msg.set_Field("P2_Type", 1);
				msg.set_Field("Login", m_conn.NodeName);
				Console.Write("Quantity of users:");
				UInt32 qty = System.Convert.ToUInt32(Console.ReadLine());
				msg.set_Field("UserQty", qty);

				// отправляем сообщение и принимаем ответ
				msg = msg.Send(m_conn, 15000);
				
				if (System.Convert.ToString(msg.get_Field("P2_Category")) == "p2spotgate2" && System.Convert.ToUInt32(msg.get_Field("P2_Type")) == 2)
				{
					String strRes;
					UInt32 res = System.Convert.ToUInt32(msg.get_Field("res"));
					switch( res )
					{
						case 0:
							strRes = "OK";
							break;
						default:
							strRes = "Error code " + res;
							break;
					}
					Console.WriteLine("Result " + strRes);
				}
				else
				{
					Console.WriteLine("Unknown message received");
				}
			}
			catch (System.Runtime.InteropServices.COMException e)
			{
				Console.WriteLine("Couldn't send message {0} {1:X}", e.Message,  e.ErrorCode);
			}
		}

		// ГЛАВНЫЙ ЦИКЛ
		public void Run()
		{
			
			while( !m_stop )
			{
				try
				{
					// создаем соединение с роутером
					m_conn.Connect();
					try
					{
						while( !m_stop )
						{
							try
							{
								if( !m_opened )
								{
									if( m_stream.State == TDataStreamState.DS_STATE_ERROR )
									{
										m_stream.Close();
									}
									// открываем поток репликации
									m_stream.Open(m_conn);
									m_opened = true;
								}
							}
							catch (System.Runtime.InteropServices.COMException e)
							{
								LogWriteLine("Exception {0} {1:X}", e.Message,  e.ErrorCode);
							}
							uint cookie;
							// обрабатываем пришедшее сообщение. Обработка идет в интерфейсах обратного вызова
							m_conn.ProcessMessage(out cookie, 0);
							Thread.Sleep(100);
						}
					}
					catch (System.Runtime.InteropServices.COMException e)
					{
						LogWriteLine("Exception {0} {1:X}", e.Message,  e.ErrorCode);
					}
					if( m_opened )
					{
						try
						{
							m_stream.Close();
						}
						catch (System.Runtime.InteropServices.COMException e)
						{
							LogWriteLine("Exception {0} {1:X}", e.Message,  e.ErrorCode);
						}
						m_opened = false;
					}
					m_conn.Disconnect();
				}
				catch (System.Runtime.InteropServices.COMException e)
				{
					LogWriteLine("Exception {0} {1:X}", e.Message,  e.ErrorCode);
				}
				catch (System.Exception e)
				{
					LogWriteLine("System Exception {0} {1}", e.Message, e.Source);
				}
			}
		}

		// Обработка состояния соединения 
		void ConnectionStatusChanged(CP2Connection conn, TConnectionStatus newStatus)
		{
			String state = "MQ connection state ";
			if( ( newStatus & TConnectionStatus.CS_CONNECTION_BUSY ) != 0 )
			{
				state += "BUSY";
			}
			else if( ( newStatus & TConnectionStatus.CS_CONNECTION_CONNECTED ) != 0 )
			{
				state += "CONNECTED";
			}
			else if( ( newStatus & TConnectionStatus.CS_CONNECTION_DISCONNECTED ) != 0 )
			{
				state += "DISCONNECTED";
			}
			else if( ( newStatus & TConnectionStatus.CS_CONNECTION_INVALID ) != 0 )
			{
				state += "INVALID";
			}

			state += " router state ";
			if( ( newStatus & TConnectionStatus.CS_ROUTER_CONNECTED ) != 0 )
			{
				// Когда соединились - запрашиваем адрес сервера-обработчика
				state += "CONNECTED";
				m_destAddr = conn.ResolveService("GATE2_SVC");
			}
			else if( ( newStatus & TConnectionStatus.CS_ROUTER_DISCONNECTED ) != 0 )
			{
				state += "DISCONNECTED";
			}
			else if( ( newStatus & TConnectionStatus.CS_ROUTER_LOGINFAILED ) != 0 )
			{
				state += "LOGINFAILED";
			}
			else if( ( newStatus & TConnectionStatus.CS_ROUTER_NOCONNECT ) != 0 )
			{
				state += "NOCONNECT";
			}
			else if( ( newStatus & TConnectionStatus.CS_ROUTER_RECONNECTING ) != 0 )
			{
				state += "RECONNECTING";
			}
			LogWriteLine(state);
		}

		// Обработка состояния потока репликации
		void StreamStateChanged(CP2DataStream stream, TDataStreamState newState)
		{
			String state = "Stream state ";
			switch( newState )
			{
				case TDataStreamState.DS_STATE_CLOSE:
					state += "CLOSE";
					m_opened = false;
					break;
				case TDataStreamState.DS_STATE_CLOSE_COMPLETE:
					state += "CLOSE_COMPLETE";
					break;
				case TDataStreamState.DS_STATE_ERROR:
					state += "ERROR";
					m_opened = false;
					break;
				case TDataStreamState.DS_STATE_LOCAL_SNAPSHOT:
					state += "LOCAL_SNAPSHOT";
					break;
				case TDataStreamState.DS_STATE_ONLINE:
					state += "ONLINE";
					break;
				case TDataStreamState.DS_STATE_REMOTE_SNAPSHOT:
					state += "REMOTE_SNAPSHOT";
					break;
				case TDataStreamState.DS_STATE_REOPEN:
					state += "REOPEN";
					break;
			}
			LogWriteLine(state);
		}

		//вставка записи
		void StreamDataInserted(CP2DataStream stream, String tableName, CP2Record rec)
		{
			try
			{
				LogWriteLine("Insert " + tableName);
				uint count = rec.Count;
				for( uint i = 0; i < count; ++i )
				{
					if( i != count - 1)
					{
						LogWrite(rec.GetValAsStringByIndex(i) + ";");
					} 
					else
					{
						LogWriteLine(rec.GetValAsStringByIndex(i));
					}
				}
			}
			catch (System.Exception e)
			{
				LogWriteLine("!!!" + e.Message + "!!!" + e.Source);			
			}
		}

		//апдейт записи
		void StreamDataUpdated(CP2DataStream stream, String tableName, Int64 Id, CP2Record rec)
		{
			LogWriteLine("Update " + tableName + " " + Id);
			try
			{
				uint count = rec.Count;
				for( uint i = 0; i < count; ++i )
				{
					if( i != count - 1)
					{
						LogWrite(rec.GetValAsStringByIndex(i) + ";");
					} 
					else
					{
						LogWriteLine(rec.GetValAsStringByIndex(i));
					}
				}
			}
			catch (System.Exception e)
			{
				LogWriteLine("!!!" + e.Message + "!!!" + e.Source);			
			}
		}

		//удаление записи
		void StreamDataDeleted(CP2DataStream stream, String tableName, Int64 Id, CP2Record rec)
		{
			LogWriteLine("Delete " + tableName + " " + Id);
		}


		void LogWriteLine(string s, params object[] arg )
		{
			if( m_logFile == null )
			{
				m_logFile = new StreamWriter("P2SimpleGate2Client.log", false, System.Text.Encoding.Unicode);
			}
			m_logFile.WriteLine(s, arg);
			m_logFile.Flush();
		}

		void LogWrite(string s, params object[] arg )
		{
			if( m_logFile == null )
			{
				m_logFile = new StreamWriter("P2SimpleGate2Client.log", false, System.Text.Encoding.Unicode);
			}
			m_logFile.Write(s, arg);
			m_logFile.Flush();
		}

	}
}
