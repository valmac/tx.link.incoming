
var conn = WScript.CreateObject("P2ClientGate.P2Connection"); // создаем объект Connection
conn.AppName = "JSOrdSend"; // адрес вашего приложения в коммуникациях РТС.
conn.Host	 = "127.0.0.1"; //Адрес 
conn.Port	 = 4001; // и порт локального роутера

var errorClass = conn.Connect(); // устанавливаем соединение с локальным роутером
var srvAddr = conn.ResolveService("FORTS_SRV"); // ищем адрес сервера приема заявок

WScript.Echo(srvAddr);

// создаем и инициализируем фабрику объектов-сообщений
var msgs = WScript.CreateObject("P2ClientGate.P2BLMessageFactory");
msgs.Init(".\\Scheme\\p2fortsgate_messages.ini","");

WScript.Echo("Msg Factory inited"); 

// создаем и заполняем сообщение
var msg = msgs.CreateMessageByName("FutAddOrder");

WScript.Echo("Msg created");

msg.DestAddr = srvAddr; // адрес сервера

msg.Field("P2_Category")    = "FORTS_MSG"; // служебные поля. 
msg.Field("P2_Type")        = 30; //служебные поля. 



msg.Field("isin")           = "LKOH-6.11"; 
msg.Field("price")     	    = "19000"; 
msg.Field("amount")         = "1"; 
msg.Field("client_code")    = "000";
msg.Field("type")    	    = 1;
msg.Field("dir")    	    = 1;

WScript.Echo("BeforeSend");


msg = msg.Send(conn, 5000); // посылаем сообщение с ожиданием ответа в течение 5 000 миллисекунд

var c = msg.Field("P2_Category");
var t = msg.Field("P2_Type");

WScript.Echo("category " + c + "; type " + t); 

if( ( c == "FORTS_MSG" ) && ( t == 101 ) )
{
    var code = msg.Field("code"); // разбираем ответ
    if (code == 0)
	    WScript.Echo("Adding order Ok, Order_id="+msg.Field("order_id")); 
    else
	    WScript.Echo("Adding order fail, logic error="+msg.Field("message")); 
}
else if( ( c == "FORTS_MSG" ) && ( t == 100 ) )
{
    WScript.Echo("Adding order fail, system level error "+msg.Field("err_code")+" "+msg.Field("message")); 
}
else
{
    WScript.Echo("Unexpected MQ message recieved; category " + c + "; type " + t); 
}



