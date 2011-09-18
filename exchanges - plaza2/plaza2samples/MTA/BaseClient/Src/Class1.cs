using System;
using System.IO;
using System.Text;
using System.Runtime.InteropServices;
using P2ClientGateMTA;
using System.Threading;
using System.Diagnostics;


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
        /// 
        [MTAThread]
		static void Main(string[] args)
		{
			//
			// TODO: Add code to start application here
			//
             try
            {
			    Client cl = new Client();
			    cl.Start(args);
			    Thread procMsgThread = new Thread(new ThreadStart(cl.ProcessConsole));                                               
			    procMsgThread.Start();
			    cl.Run();
			    procMsgThread.Join();
            }
            catch (Exception e)
            {
                int hRes = Marshal.GetHRForException(e);
                Console.WriteLine("Exception {0}", e.Message);
                if (hRes == -2147205116) // P2ERR_INI_FILE_NOT_FOUND
                {
                    Console.WriteLine("Can't find one or both of ini file: P2ClientGate.ini, forts_scheme.ini");
                }
                return;
            }
		}
	}

	class Client
	{
		private bool m_stop = false;

		CP2Connection m_conn;
                

		CP2DataStream m_streamAggregates;
		CP2DataStream m_streamTrades;
		CP2DataStream m_streamCommons;
        CP2DataStream m_streamMiscInfo;

		StreamWriter m_logFile;

		/*
		 * Идентификаторы потоков
		 */
		string streamAggregatesID = "FORTS_FUTAGGR20_REPL";
		string streamTradesID = "FORTS_FUTTRADE_REPL";
		string streamCommonsID = "FORTS_FUTCOMMON_REPL";
        string streamMiscInfoID = "FORTS_MISCINFO_REPL";

		
		// Строки соединения с локальной БД для потоков				
        // базы на SQLite
        string streamAggregatesDBConn = "P2DBSQLite3.dll;sqlite3.ini;forts_aggregate.db";
        string streamMiscInfoDBConn = "P2DBSQLite3.dll;sqlite3.ini;forts_miscinfo.db";        
        string streamTradesDBConn  = "P2DBSQLite3.dll;sqlite3.ini;forts_trades.db";            
        string streamCommonsDBConn = "P2DBODBC.dll;p2dbodbc.ini;DRIVER={SQL Server};SERVER=.;DATABASE=REPL_DB;UID=sa;PWD=654321";
        
		public void Start(string[] args)
		{
           
                // Объект "соединение" и параметры соединения с приложением P2MQRouter
                m_conn = new CP2ConnectionClass();                
                m_conn.Host = "localhost";
                m_conn.Port = 4001;
                m_conn.AppName = "p2clientgate_forts";                
                                
                IP2ConnectionEvent_ConnectionStatusChangedEventHandler connStatusHandler = new IP2ConnectionEvent_ConnectionStatusChangedEventHandler(ConnectionStatusChanged);
                m_conn.ConnectionStatusChanged += connStatusHandler;

                // создаем объект "входящий поток репликации"
                m_streamAggregates = new CP2DataStreamClass();
                m_streamAggregates.DBConnString = streamAggregatesDBConn;
                m_streamAggregates.type = TRequestType.RT_COMBINED_DYNAMIC;
                m_streamAggregates.StreamName = streamAggregatesID;
                m_streamAggregates.TableSet = new CP2TableSetClass();
                m_streamAggregates.TableSet.InitFromIni("orders_aggr.ini", "");

                m_streamTrades = new CP2DataStreamClass();
                m_streamTrades.DBConnString = streamTradesDBConn;
                m_streamTrades.type = TRequestType.RT_COMBINED_DYNAMIC;
                m_streamTrades.StreamName = streamTradesID;
                m_streamTrades.TableSet = new CP2TableSetClass();
                m_streamTrades.TableSet.InitFromIni("fut_trades.ini", "");

                m_streamMiscInfo = new CP2DataStreamClass();
                m_streamMiscInfo.DBConnString = streamMiscInfoDBConn;
                m_streamMiscInfo.type = TRequestType.RT_COMBINED_DYNAMIC;
                m_streamMiscInfo.StreamName = streamMiscInfoID;
                m_streamMiscInfo.TableSet = new CP2TableSetClass();
                m_streamMiscInfo.TableSet.InitFromIni("misc_info.ini", "");


                m_streamCommons = new CP2DataStreamClass();
                m_streamCommons.DBConnString = streamCommonsDBConn;
                m_streamCommons.type = TRequestType.RT_COMBINED_DYNAMIC;
                m_streamCommons.StreamName = streamCommonsID;
                m_streamCommons.TableSet = new CP2TableSetClass();
                m_streamCommons.TableSet.InitFromIni2("forts_scheme.ini", "FutCommon");

                // регистрируем интерфейсы обратного вызова для получения данных
                IP2DataStreamEvents_StreamStateChangedEventHandler StateHandler = new IP2DataStreamEvents_StreamStateChangedEventHandler(StreamStateChanged);
                m_streamAggregates.StreamStateChanged += StateHandler;
                m_streamTrades.StreamStateChanged += StateHandler;
                m_streamCommons.StreamStateChanged += StateHandler;
                m_streamMiscInfo.StreamStateChanged += StateHandler;

                IP2DataStreamEvents_StreamDataInsertedEventHandler InsHandler = new IP2DataStreamEvents_StreamDataInsertedEventHandler(StreamDataInserted);
                m_streamAggregates.StreamDataInserted += InsHandler;
                m_streamTrades.StreamDataInserted += InsHandler;
                m_streamCommons.StreamDataInserted += InsHandler;
                m_streamMiscInfo.StreamDataInserted += InsHandler;
                
                IP2DataStreamEvents_StreamDataUpdatedEventHandler UpdHandler = new IP2DataStreamEvents_StreamDataUpdatedEventHandler(StreamDataUpdated);
                m_streamAggregates.StreamDataUpdated += UpdHandler;
                m_streamTrades.StreamDataUpdated += UpdHandler;
                m_streamCommons.StreamDataUpdated += UpdHandler;
                m_streamMiscInfo.StreamDataUpdated += UpdHandler;

                IP2DataStreamEvents_StreamDataDeletedEventHandler DelHandler = new IP2DataStreamEvents_StreamDataDeletedEventHandler(StreamDataDeleted);
                m_streamAggregates.StreamDataDeleted += DelHandler;
                m_streamTrades.StreamDataDeleted += DelHandler;
                m_streamCommons.StreamDataDeleted += DelHandler;
                m_streamMiscInfo.StreamDataDeleted += DelHandler;

                // Добавляем обработчик события смены номера жизни потокам с клиентской схемой
                IP2DataStreamEvents_StreamLifeNumChangedEventHandler LifeNumHandler = new IP2DataStreamEvents_StreamLifeNumChangedEventHandler(StreamLifeNumChanged);
                m_streamAggregates.StreamLifeNumChanged += LifeNumHandler;
                m_streamTrades.StreamLifeNumChanged += LifeNumHandler;
                m_streamCommons.StreamLifeNumChanged += LifeNumHandler;
                m_streamMiscInfo.StreamLifeNumChanged += LifeNumHandler;

                for (int i = 0; i < args.Length; ++i)
                {
                    if (i == 0)
                    {
                        m_streamAggregates.DBConnString = args[i];
                        Console.WriteLine("Using DB connection string: {0}", m_streamAggregates.DBConnString);
                    }
                }            
		}

        // Функция обработки смены номера жизни
        void StreamLifeNumChanged(CP2DataStream stream, int lifeNum)
        {
            if (stream.StreamName == "FORTS_FUTTRADE_REPL")
            {
                m_streamTrades.TableSet.LifeNum = lifeNum;
                m_streamTrades.TableSet.SetLifeNumToIni("fut_trades.ini");
            }
            if (stream.StreamName == "FORTS_FUTCOMMON_REPL")
            {
                m_streamCommons.TableSet.LifeNum = lifeNum;
                m_streamCommons.TableSet.SetLifeNumToIni("forts_scheme.ini");
            }
            if (stream.StreamName == "FORTS_MISCINFO_REPL")
            {
                m_streamMiscInfo.TableSet.LifeNum = lifeNum;
                m_streamMiscInfo.TableSet.SetLifeNumToIni("misc_info.ini");
            }
            if (stream.StreamName == "FORTS_FUTAGGR20_REPL")
            {
                m_streamAggregates.TableSet.LifeNum = lifeNum;
                m_streamAggregates.TableSet.SetLifeNumToIni("orders_aggr.ini");
            }
        }		

		// обработка ввода пользователя 
		public void ProcessConsole()
		{
			System.Int32 connected = System.Convert.ToInt32(TConnectionStatus.CS_CONNECTION_CONNECTED);
			while( !m_stop )
			{
				Console.WriteLine("0 - EXIT");
				Console.Write("Enter command:");
				string cmd = Console.ReadLine();
				if( cmd == "0" )
				{
					m_stop = true;
				}
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

								if( m_streamAggregates.State ==  TDataStreamState.DS_STATE_ERROR ||
									m_streamAggregates.State ==  TDataStreamState.DS_STATE_CLOSE)
								{
									if( m_streamAggregates.State == TDataStreamState.DS_STATE_ERROR )
									{
										m_streamAggregates.Close();
									}
									// открываем поток репликации
									m_streamAggregates.Open(m_conn);                                    
								}

								if( m_streamTrades.State ==  TDataStreamState.DS_STATE_ERROR ||
									m_streamTrades.State ==  TDataStreamState.DS_STATE_CLOSE)
								{
									if( m_streamTrades.State == TDataStreamState.DS_STATE_ERROR )
									{
										m_streamTrades.Close();
									}
									// открываем поток репликации
                                    m_streamTrades.Open(m_conn);                                                                        
								}

								if( m_streamCommons.State ==  TDataStreamState.DS_STATE_ERROR ||
									m_streamCommons.State ==  TDataStreamState.DS_STATE_CLOSE)
								{
									if( m_streamCommons.State == TDataStreamState.DS_STATE_ERROR )
									{
										m_streamCommons.Close();
									}
									// открываем поток репликации
									m_streamCommons.Open(m_conn);
								}

                                if (m_streamMiscInfo.State == TDataStreamState.DS_STATE_ERROR ||
                                    m_streamMiscInfo.State == TDataStreamState.DS_STATE_CLOSE)
                                {
                                    if (m_streamMiscInfo.State == TDataStreamState.DS_STATE_ERROR)
                                    {
                                        m_streamMiscInfo.Close();
                                    }
                                    // открываем поток репликации
                                    m_streamMiscInfo.Open(m_conn);
                                }
							}
							catch (System.Runtime.InteropServices.COMException e)
							{
								LogWriteLine("Exception {0} {1:X}", e.Message,  e.ErrorCode);
							}
							uint cookie;
							// обрабатываем пришедшее сообщение. Обработка идет в интерфейсах обратного вызова
							m_conn.ProcessMessage(out cookie, 100);
							
						}
					}
					catch (System.Runtime.InteropServices.COMException e)
					{
						LogWriteLine("Exception {0} {1:X}", e.Message,  e.ErrorCode);
					}

					if( m_streamAggregates.State !=  TDataStreamState.DS_STATE_CLOSE )
					{
						try
						{
							m_streamAggregates.Close();
						}
						catch (System.Runtime.InteropServices.COMException e)
						{
							LogWriteLine("Exception {0} {1:X}", e.Message,  e.ErrorCode);
						}
					}

					if( m_streamTrades.State !=  TDataStreamState.DS_STATE_CLOSE )
					{
						try
						{
							m_streamTrades.Close();
						}
						catch (System.Runtime.InteropServices.COMException e)
						{
							LogWriteLine("Exception {0} {1:X}", e.Message,  e.ErrorCode);
						}
					}

					if( m_streamCommons.State !=  TDataStreamState.DS_STATE_CLOSE )
					{
						try
						{
							m_streamCommons.Close();
						}
						catch (System.Runtime.InteropServices.COMException e)
						{
							LogWriteLine("Exception {0} {1:X}", e.Message,  e.ErrorCode);
						}
					}

                    if (m_streamMiscInfo.State != TDataStreamState.DS_STATE_CLOSE)
                    {
                        try
                        {
                            m_streamMiscInfo.Close();
                        }
                        catch (System.Runtime.InteropServices.COMException e)
                        {
                            LogWriteLine("Exception {0} {1:X}", e.Message, e.ErrorCode);
                        }
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
			String state = "Stream " + stream.StreamName + " state: ";
			switch( newState )
			{
				case TDataStreamState.DS_STATE_CLOSE:
					state += "CLOSE";
					//m_opened = false;
					break;
				case TDataStreamState.DS_STATE_CLOSE_COMPLETE:
					state += "CLOSE_COMPLETE";
					break;
				case TDataStreamState.DS_STATE_ERROR:
					state += "ERROR";
					//m_opened = false;
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
