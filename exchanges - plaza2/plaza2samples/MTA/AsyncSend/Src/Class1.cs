using System;
using System.IO;
using System.Text;
using System.Runtime.InteropServices;
using P2ClientGateMTA;
using System.Threading;


namespace AsyncSend
{
	[
	ComVisible(true)
	]
	public class CP2AsyncMessageEvents: IP2AsyncMessageEvents
	{
		void IP2AsyncMessageEvents.DeliveryEvent(CP2BLMessage reply, uint errCode)
		{
       		Class1.LogWriteLine("DeliveryEvent errCode:" + errCode);
            Class1.PrintReplyMsg(reply);
        }
	}

    [
    ComVisible(true)
    ]
    public class CP2SendAsync2Event : IP2AsyncSendEvent2
    {
        void IP2AsyncSendEvent2.SendAsync2Reply(CP2BLMessage reply, uint errCode, long eventParam)
        {
            Class1.LogWriteLine("SendAsync2Reply(" + errCode + ", " + eventParam + ")");
            Class1.PrintReplyMsg(reply);
        }
    }
	class Class1
	{
		static StreamWriter m_logFile;
		
        [MTAThread]
		static void Main(string[] args)
		{
            try
            {
                CP2Connection conn = new CP2ConnectionClass();
                conn.AppName = "AsyncOrdSend"; // адрес вашего приложения в коммуникациях РТС.
                conn.Host = "127.0.0.1"; //Адрес 
                conn.Port = 4001; // и порт локального роутера

                conn.Connect();// устанавливаем соединение с локальным роутером
                string srvAddr = conn.ResolveService("FORTS_SRV"); // ищем адрес сервера приема заявок
                LogWriteLine("address is {0}", srvAddr);
                CP2BLMessageFactory msgs = new CP2BLMessageFactory();

                msgs.Init("p2fortsgate_messages.ini", "");

                CP2BLMessage msg = msgs.CreateMessageByName("FutAddOrder");

                msg.DestAddr = srvAddr;
                msg.set_Field("P2_Category", "FORTS_MSG");
                msg.set_Field("P2_Type", 30);
                msg.set_Field("isin", "LKOH-6.11");
                msg.set_Field("price", "19000");
                msg.set_Field("amount", "1");
                msg.set_Field("client_code", "001");
                msg.set_Field("type", 1);
                msg.set_Field("dir", 1);

                msg.SendAsync(conn, 5000, new CP2AsyncMessageEvents());
                msg.SendAsync2(conn, 5000, new CP2SendAsync2Event(), 1234567890);

                while (true)
                {
                    uint cookie;
                    conn.ProcessMessage(out cookie, 100);
                }

            }
            catch (Exception e)
            {
                int hRes = Marshal.GetHRForException(e);
                Console.WriteLine("Exception {0}", e.Message);
                LogWriteLine("Exception {0} {1}", e.Message, hRes);
                if (hRes == -2147205116) // P2ERR_INI_FILE_NOT_FOUND
                {
                    string s = "Can't find one or both of ini file: P2ClientGate.ini, p2fortsgate_messages.ini";
                    Console.WriteLine("{0}", s);
                    LogWriteLine("{0}", s);
                }
                return;
            }

		}
        public static void PrintReplyMsg(CP2BLMessage reply)
        {
			object c = reply.get_Field("P2_Category");
			object t = reply.get_Field("P2_Type");
			

			LogWriteLine("category " + c + "; type " + t); 

			if( ( (string)c == "FORTS_MSG" ) && ( (System.UInt32)t == 101 ) )
			{
				object code = reply.get_Field("code"); // разбираем ответ
				if ((System.Int32)code == 0)
					LogWriteLine("Adding order Ok, Order_id is "+reply.get_Field("order_id")); 
				else
					LogWriteLine("Adding order fail, logic error is "+reply.get_Field("message")); 
			}
			else if( ( (string)c == "FORTS_MSG" ) && ( (System.UInt32)t == 100 ) )
			{
				LogWriteLine("Adding order fail, system level error "+reply.get_Field("err_code")+" "+reply.get_Field("message")); 
			}
			else
			{
				LogWriteLine("Unexpected MQ message recieved; category " + c + "; type " + t); 
			}
		}

        public static void LogWriteLine(string s, params object[] arg )
		{
			if( m_logFile == null )
			{
				m_logFile = new StreamWriter("P2SimpleGate2Client.log", false, System.Text.Encoding.Unicode);
			}
			m_logFile.WriteLine(s, arg);
			m_logFile.Flush();
		}

		public static void LogWrite(string s, params object[] arg )
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
