using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using FTFHelper;
using NDde.Server;
using TestAtConsole;

/// <summary>
/// Мой ДДЕ-сервер
/// </summary>
public class MyDDEServer : DdeServer
{
    private Dictionary<string, Trade> _trades;
    private bool _isParserInited;
    private object[] _headers;
    private bool _isAdviseStarted;

    /// <summary>
    /// Конструктор
    /// </summary>
    /// <param name="service"></param>
    public MyDDEServer(string service)
        : base(service)
    {
        _trades = new Dictionary<string, Trade>();
        _isParserInited = false;
        _isAdviseStarted = false;
    }

    /// <summary>
    /// Запущен ли адвайз (апдейт клиентов ДДЕ-сервера)
    /// </summary>
    public bool IsAdviseStarted
    {
        get{return _isAdviseStarted;}
        set
        {
            Console.WriteLine("IsAdviseStarted={0}", value);
            _isAdviseStarted = value;
        }
    }

    /// <summary>
    /// Событие сервера - получение данных 
    /// </summary>
    /// <param name="conversation"></param>
    /// <param name="item"></param>
    /// <param name="data"></param>
    /// <param name="format"></param>
    /// <returns></returns>
    protected override PokeResult OnPoke(DdeConversation conversation, string item, byte[] data, int format)
    {
        
        Console.WriteLine("OnPoke:".PadRight(16)
                + " Service=" + conversation.Service
                + " Topic=" + conversation.Topic
                );

        var tiks=ParseData(data);
        for (int i = 0; i < tiks.Length;i++ )
        {
            Console.WriteLine(tiks[i]);
        }
        Console.WriteLine();
        return PokeResult.Processed;
    }

    /// <summary>
    /// Событие сервера - дисконнект
    /// </summary>
    /// <param name="conversation"></param>
    protected override void OnDisconnect(DdeConversation conversation)
    {
        _isParserInited = false;
        Console.WriteLine("OnDisconnect:".PadRight(16)
                    + " Service=" + conversation.Service
                    + " Topic=" + conversation.Topic
                    + " Parser not inited.");
    }

    /// <summary>
    /// Событие сервера - после подключения (клиента?)
    /// </summary>
    /// <param name="conversation"></param>
    protected override void OnAfterConnect(DdeConversation conversation)
    {
        Console.WriteLine("OnAfterConnect:".PadRight(16)
                    + " Service=" + conversation.Service
                    + " Topic=" + conversation.Topic

                    );
    }

    /// <summary>
    /// Событие сервера - перед подключением (клиента?)
    /// </summary>
    /// <param name="topic"></param>
    /// <returns></returns>
    protected override bool OnBeforeConnect(string topic)
    {
        Console.WriteLine("OnBeforeConnect:".PadRight(16)
                    + " Service=" + base.Service
                    + " Topic=" + topic);

        return true;
    }

    /// <summary>
    /// Разбор пакета данных на объекты
    /// </summary>
    /// <param name="data"></param>
    /// <returns></returns>
    private Trade[] ParseData(byte[] data)
    {
        List<Trade> ret = new List<Trade>();
        var result = XLTable.Cells(data);
        
        for (int row = 0; row < result.Length; row++)
        {
            if (!_isParserInited)
            {
                _headers = result[row];
                _isParserInited = true;
                continue;
            }
            if (!IsEqual(result[row],_headers))
            {
                Trade trd = Trade.CreateInstance(result[row], _headers);
                ret.Add(trd);
                ProcessTrade(trd);
            }
        }
        return ret.ToArray();
    }

    /// <summary>
    /// проверка идентичности двух массивов
    /// </summary>
    /// <param name="row"></param>
    /// <param name="headers"></param>
    /// <returns></returns>
    private bool IsEqual(object[] row, object[] headers)
    {
        if(row.Length!=headers.Length) return false;
        for(int i=0; i<row.Length;i++)
        {
            if(!row[i].ToString().Equals(headers[i].ToString()))
                return false;
        }
        return true;
    }

    /// <summary>
    /// Обработать тик
    /// </summary>
    /// <param name="trade"></param>
    private void ProcessTrade(Trade trade)
    {
        if(!_trades.ContainsKey(trade.SecID))
            _trades.Add(trade.SecID,trade);

        _trades[trade.SecID] = trade;

        if (!_isAdviseStarted) return;

        Console.WriteLine("Advise {0}  ID:{1}", trade.SecID, trade.ID);
        Advise(trade.SecID, "ID");
        Thread.Sleep(1);

        Console.WriteLine("Advise {0}  DateTime:{1}", trade.SecID,trade.DateTime);
        Advise(trade.SecID, "DateTime");
        Thread.Sleep(1);

        Console.WriteLine("Advise {0}  Price:{1}", trade.SecID,trade.Price);
        Advise(trade.SecID, "Price");
        Thread.Sleep(1);

        Console.WriteLine("Advise {0}  Size:{1}", trade.SecID,trade.Price);
        Advise(trade.SecID, "Size");
        Thread.Sleep(1);
    }

    /// <summary>
    /// Событие сервера - при запросе данных
    /// </summary>
    /// <param name="conversation"></param>
    /// <param name="item"></param>
    /// <param name="format"></param>
    /// <returns></returns>
    protected override RequestResult OnRequest(DdeConversation conversation, string item, int format)
    {
        Console.WriteLine("OnRequest>T={0} I={1} F={2}",
            conversation.Topic,item,format);
        if (format == 1)
            return new RequestResult(Encoding.ASCII.GetBytes(GetValue(conversation.Topic, item) + "\0"));
        return RequestResult.NotProcessed;

    }

    /// <summary>
    /// Событие сервера - при апдейте клиента
    /// </summary>
    /// <param name="topic"></param>
    /// <param name="item"></param>
    /// <param name="format"></param>
    /// <returns></returns>
    protected override byte[] OnAdvise(string topic, string item, int format)
    {
        Console.WriteLine("OnAdvise> T={0} I={1} F={2}",topic, item,format);
        if (format == 1)
        {
            string ret = GetValue(topic, item);
            Console.WriteLine("OnAdvise send:'{0}'", ret);
            return Encoding.ASCII.GetBytes(ret + "\0");
        }
        return null;
    }

    /// <summary>
    /// Событие сервера - начало апдейта клиента
    /// </summary>
    /// <param name="conversation"></param>
    /// <param name="item"></param>
    /// <param name="format"></param>
    /// <returns></returns>
    protected override bool OnStartAdvise(DdeConversation conversation, string item, int format)
    {
        Console.WriteLine("OnStartAdvise:".PadRight(16)
            + " Service='" + conversation.Service + "'"
            + " Topic='" + conversation.Topic + "'"
            + " Handle=" + conversation.Handle.ToString()
            + " Item='" + item + "'"
            + " Format=" + format.ToString());

        // Initiate the advisory loop only if the format is CF_TEXT.
        return format == 1;
    }

    /// <summary>
    /// Получить данные по топику и итему
    /// </summary>
    /// <param name="topic"></param>
    /// <param name="item"></param>
    /// <returns></returns>
    private string GetValue(string topic, string item)
    {
        if (!_trades.ContainsKey(topic))
            return string.Format("NoTopic({0})",topic);
        switch(item.ToUpper())
        {
            case "ID":
                return _trades[topic].ID;
            case "DATETIME":
                return _trades[topic].DateTime.ToString();
            case "PRICE":
                return _trades[topic].Price.ToString();
            case "SIZE":
                return _trades[topic].Size.ToString();
            default:
                return string.Format("NoItem({0})",item);
        }
    }
}