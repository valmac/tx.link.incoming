// P2AddOrderConsole.cpp : Defines the entry point for the console application.
//
#include "stdafx.h"
//////////////////////////////////////////////////////////////////////////
#define STRICT
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0400
#endif
#define _ATL_APARTMENT_THREADED
#include <atlbase.h>
CComModule _Module;
#include <atlcom.h>
#include <comdef.h>
//////////////////////////////////////////////////////////////////////////
#include <io.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <share.h>
#include <conio.h>
//////////////////////////////////////////////////////////////////////////
#include <string>
using namespace std;
//////////////////////////////////////////////////////////////////////////
#include <process.h>
//////////////////////////////////////////////////////////////////////////
#import "libid:{B0940DCA-AB77-46DA-852B-6058F2A995DB}" no_namespace, named_guids
//////////////////////////////////////////////////////////////////////////
FILE* g_fLog = NULL;
//////////////////////////////////////////////////////////////////////////
void showerror( _com_error &e ) 
{
	fprintf(g_fLog, "COM error occured\n" );
	fprintf(g_fLog, "\tError:        %08lx\n", e.Error() );
	fprintf(g_fLog, "\tErrorMessage: %s\n", e.ErrorMessage() );
	fprintf(g_fLog, "\tSource:       %s\n", static_cast<LPCTSTR>(e.Source()) );
	fprintf(g_fLog, "\tDescription:  %s\n", static_cast<LPCTSTR>(e.Description()) );
}
//////////////////////////////////////////////////////////////////////////
class StartCOM {
public:
	StartCOM() 
	{ 
		CoInitializeEx(NULL, COINIT_APARTMENTTHREADED); 
	}
	~StartCOM() { CoUninitialize(); }
};
//////////////////////////////////////////////////////////////////////////
class CLog
{
public:
	CLog()
	{
		fs = _sopen("Log\\P2AddOrderConsole.log", _O_WRONLY | O_CREAT | _O_NOINHERIT | _O_SEQUENTIAL,
			_SH_DENYWR, _S_IREAD | _S_IWRITE);
		g_fLog = fdopen(fs, "a");
		setvbuf(g_fLog, 0, _IONBF, 0);
		SetConsoleOutputCP(1251);
	}
	~CLog()
	{
		fclose(g_fLog); _close(fs);
	}
private:
	int fs;
};
//////////////////////////////////////////////////////////////////////////
class CConnEvent: 
	public IDispEventImpl<0, CConnEvent, &IID_IP2ConnectionEvent, &LIBID_P2ClientGate, 1>
{
public:
	CConnEvent(wchar_t* appName): m_conn("P2ClientGate.P2Connection")
	{
		DispEventAdvise(m_conn);

		m_conn->AppName = appName;		
		m_conn->Port = 4001;
		m_conn->Connect();
	}
	~CConnEvent()
	{
		DispEventUnadvise(m_conn);
	}
	// IP2ConnectionEvent
	void __stdcall ConnectionStatusChanged( IDispatch * conn, TConnectionStatus newStatus )
	{
		const char* csStr;
		switch(newStatus & 0x0000FFFF) 
		{
		case CS_CONNECTION_DISCONNECTED:
			csStr = "CONNECTION_DISCONNECTED";
			break;
		case CS_CONNECTION_CONNECTED:
			csStr = "CONNECTION_CONNECTED";
			break;
		case CS_CONNECTION_INVALID:
			csStr = "CONNECTION_INVALID";
			break;
		case CS_CONNECTION_BUSY:
			csStr = "CONNECTION_BUSY";
			break;
		case CS_ROUTER_DISCONNECTED:
			csStr = "ROUTER_DISCONNECTED";
			break;
		default:
			csStr = "";
		}
		const char* rsStr;
		switch(newStatus & 0xFFFF0000)
		{
		case CS_ROUTER_RECONNECTING:
			rsStr = "ROUTER_RECONNECTING";
			break;
		case CS_ROUTER_CONNECTED:
			rsStr = "ROUTER_CONNECTED";
			break;
		case CS_ROUTER_LOGINFAILED:
			rsStr = "ROUTER_LOGINFAILED";
			break;
		case CS_ROUTER_NOCONNECT:
			rsStr = "ROUTER_NOCONNECT";
			break;
		default:
			rsStr = "";
		}
		fprintf(g_fLog,"ConnectionStatusChanged(%p, '%s-%s')\n", conn, rsStr, csStr);
	}
	//////////////////////////////////////////////////////////////////////////
	BEGIN_SINK_MAP(CConnEvent)
		SINK_ENTRY_EX(0, IID_IP2ConnectionEvent, 1, ConnectionStatusChanged)
	END_SINK_MAP()
	IP2Connection* GetConn(){ return m_conn; }
private:
	IP2ConnectionPtr m_conn;
};
//////////////////////////////////////////////////////////////////////////
class CDSEvents: 
	public IDispEventImpl<0, CDSEvents, &IID_IP2DataStreamEvents, &LIBID_P2ClientGate, 1>
{
public:
	CDSEvents(IP2Connection* conn, wchar_t* name): m_dataStream(L"P2ClientGate.P2DataStream")
	{	
		DispEventAdvise(m_dataStream);
		m_dataStream->DBConnString = (wstring(L"P2DBSqLiteD.dll;;Log\\") + name + L".db").c_str();
		m_dataStream->type = RT_COMBINED_DYNAMIC;
		m_dataStream->StreamName = (wstring(L"FORTS_") + name + L"_REPL").c_str();
		m_dataStream->Open(conn);
	}
	~CDSEvents()
	{
		DispEventUnadvise(m_dataStream);
	}
	void __stdcall PrintRec(IDispatch * r)
	{
		IP2RecordPtr rec;
		HRESULT res = r->QueryInterface(__uuidof(IP2Record), reinterpret_cast<void**>(&rec));
		if( FAILED(res) ) 
		{
			fprintf(g_fLog, "Couldn't query IP2Record interface of record in notification");
			return;
		}
		USES_CONVERSION;
		unsigned long count = rec->Count;
		string fields;
		for(unsigned long i = 0; i < count; ++i ) 
		{
			_bstr_t fld = rec->GetValAsStringByIndex(i);
			fprintf(g_fLog,"|%s", W2A(fld));
		}
		fprintf(g_fLog,"\n");
	}

	// IP2DataStreamEvents
	void __stdcall StreamStateChanged( IDispatch * stream, TDataStreamState newState )
	{
		const char* str;
		switch(newState) {
			case DS_STATE_CLOSE:
				str = "CLOSED";
				break;
			case DS_STATE_LOCAL_SNAPSHOT:
				str = "LOCAL_SNAPSHOT";
				break;
			case DS_STATE_REMOTE_SNAPSHOT:
				str = "REMOTE_SNAPSHOT";
				break;
			case DS_STATE_ONLINE:
				str = "ONLINE";
				break;
			case DS_STATE_CLOSE_COMPLETE:
				str = "CLOSE_COMPLETE";
				break;
			case DS_STATE_REOPEN:
				str = "REOPEN";
				break;
			case DS_STATE_ERROR:
				str = "ERROR";
				break;
			default:
				str = "";
		}
		fprintf(g_fLog,"StreamStateChanged(%p, %s)\n", stream, str);
	}
	void __stdcall StreamDataInserted ( IDispatch * stream, BSTR tableName, IDispatch * rec )
	{
		USES_CONVERSION;
		fprintf(g_fLog,"StreamDataInserted(%p, %s, %p)", stream, W2A(tableName), rec);
		PrintRec(rec);
	}
	void __stdcall StreamDataUpdated ( IDispatch * stream, BSTR tableName, __int64 Id, IDispatch * rec )
	{
		USES_CONVERSION;
		fprintf(g_fLog,"StreamDataUpdated(%p, %s, %I64d, %p)", stream, W2A(tableName), Id, rec);
		PrintRec(rec);
	}
	void __stdcall StreamDataDeleted ( IDispatch * stream, BSTR tableName, __int64 Id, IDispatch * rec )
	{
		USES_CONVERSION;
		fprintf(g_fLog,"StreamDataDeleted(%p, %s, %I64d, %p)", stream, W2A(tableName), Id, rec);
        PrintRec(rec);
	}
	void __stdcall StreamDatumDeleted ( IDispatch * stream, BSTR tableName, __int64 rev )
	{
		USES_CONVERSION;
		fprintf(g_fLog,"StreamDatumDeleted(%p, %s, %I64d)", stream, W2A(tableName), rev);
	}
	void __stdcall StreamDBWillBeDeleted ( IDispatch * stream )
	{
		fprintf(g_fLog,"StreamDBWillBeDeleted(%p)\n", stream);
	}
	void __stdcall StreamLifeNumChanged ( IDispatch * stream, long LifeNum )
	{
		fprintf(g_fLog,"StreamLifeNumChanged(%p, %d)\n", stream, LifeNum);
	}
	void __stdcall StreamDataBegin ( IDispatch * stream )
	{
		fprintf(g_fLog,"StreamDataBegin(%p)\n", stream);
	}
	void __stdcall StreamDataEnd ( IDispatch * stream )
	{
		fprintf(g_fLog,"StreamDataEnd(%p)\n", stream);
	}
	//////////////////////////////////////////////////////////////////////////
	BEGIN_SINK_MAP(CDSEvents)
		SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 1, StreamStateChanged)
		SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 2, StreamDataInserted)
		SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 3, StreamDataUpdated)
		SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 4, StreamDataDeleted)
		SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 5, StreamDatumDeleted)
		SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 6, StreamDBWillBeDeleted)
		SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 7, StreamLifeNumChanged)
		SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 8, StreamDataBegin)
		SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 9, StreamDataEnd)
	END_SINK_MAP()
private:
	IP2DataStreamPtr m_dataStream;
};
//////////////////////////////////////////////////////////////////////////
void PrintMsg(IP2BLMessage* reply, unsigned long errCode)
{
	USES_CONVERSION;
	if( errCode == 0 ) 
	{
		string c = W2A(static_cast<_bstr_t>(reply->GetField("P2_Category")).GetBSTR());
		unsigned long t = reply->GetField("P2_Type");

		if( ( c == "FORTS_MSG" ) && ( t == 101 ) )
		{
			unsigned long code = reply->GetField("code");
			if( code == 0)
			{
				unsigned long orderId = reply->GetField("order_id");
				printf("Adding order Ok, Order_id is %d\n", orderId); 
			}
			else
			{
				string message = W2A(static_cast<_bstr_t>(reply->GetField("message")).GetBSTR());
				printf("Adding order fail, logic error is %s\n", message.c_str()); 
			}
		}
		else if( ( c == "FORTS_MSG" ) && ( t == 100 ) )
		{
			unsigned long errCode = reply->GetField("err_code");
			string message = W2A(static_cast<_bstr_t>(reply->GetField("message")).GetBSTR());
			printf("Adding order fail, system level error 0x%x\n%s\n", errCode, message.c_str()); 
		}
		else
		{
			printf("Unexpected MQ message recieved; category %s type %d\n", c.c_str(), t); 
		}
	}
	else
	{
		printf("Delivery errorCode:0x%x \n", errCode);
	}
	return;
}
//////////////////////////////////////////////////////////////////////////
class CAsyncMessageEvent : 
	public IDispatchImpl<IP2AsyncMessageEvents>,
	public CComObjectRoot
{
public:
	STDMETHOD(DeliveryEvent) ( IP2BLMessage * reply, unsigned long errCode )
	{
		printf("DeliveryEvent(%p, %d)\n", reply, errCode);
		PrintMsg(reply, errCode);
		return S_OK;
	}
	BEGIN_COM_MAP(CAsyncMessageEvent)
		COM_INTERFACE_ENTRY(IDispatch)
		COM_INTERFACE_ENTRY(IP2AsyncMessageEvents)
	END_COM_MAP()
};
//////////////////////////////////////////////////////////////////////////
class CAsyncSendEvent2 : 
	public IDispatchImpl<IP2AsyncSendEvent2>,
	public CComObjectRoot
{
public:
	STDMETHOD(SendAsync2Reply)(IP2BLMessage* reply, unsigned long errCode, __int64 eventParam)
	{
		printf("SendAsync2Reply(%p, %d, %I64d)\n", reply, errCode, eventParam);
		PrintMsg(reply, errCode);
		return S_OK;
	}
	BEGIN_COM_MAP(CAsyncSendEvent2)
		COM_INTERFACE_ENTRY(IDispatch)
		COM_INTERFACE_ENTRY(IP2AsyncSendEvent2)
	END_COM_MAP()
};
//////////////////////////////////////////////////////////////////////////
BOOL g_exit = FALSE;
BOOL WINAPI CtrlHandler( DWORD CtrlType )
{
	g_exit = TRUE;
	return TRUE;
}
//////////////////////////////////////////////////////////////////////////
void ThreadProc(void* name)
{
	StartCOM CoInst;
	try
	{
		CConnEvent conn(reinterpret_cast<wchar_t*>(name));
		while( g_exit == FALSE )				
		{
			ULONG cookie;
			conn.GetConn()->ProcessMessage(&cookie, 5000);
		}
	}
	catch (_com_error e)
	{
		showerror(e);
	}
}
//////////////////////////////////////////////////////////////////////////
int _tmain(int argc, _TCHAR* argv[])
{
	CLog log;
	StartCOM CoInst;
	SetConsoleCtrlHandler(CtrlHandler, TRUE);
	try 
	{
		CConnEvent conn(L"P2FORTS_REPL");
		CDSEvents dsFUTNFO(conn.GetConn(), L"FUTINFO");
/*		CDSEvents dsOPTINFO(conn.GetConn(), L"OPTINFO");
		CDSEvents dsPOS(conn.GetConn(), L"POS");
		CDSEvents dsPART(conn.GetConn(), L"PART");
		CDSEvents dsFUTAGGR20(conn.GetConn(), L"FUTAGGR20");
		CDSEvents dsOPTAGGR(conn.GetConn(), L"OPTAGGR");
		CDSEvents dsOPTCOMMON(conn.GetConn(), L"OPTCOMMON");
		CDSEvents dsFUTCOMMON(conn.GetConn(), L"FUTCOMMON");
		CDSEvents dsFUTTRADE(conn.GetConn(), L"FUTTRADE");*/
		
/*		_beginthread(ThreadProc, 0, L"P2MSGSener1");
		_beginthread(ThreadProc, 0, L"P2MSGSener2");
		_beginthread(ThreadProc, 0, L"P2MSGSener3");
		_beginthread(ThreadProc, 0, L"P2MSGSener4");
		_beginthread(ThreadProc, 0, L"P2MSGSener5");*/

		_bstr_t srvAddr = conn.GetConn()->ResolveService("FORTS_SRV");

		printf("Press any key to send message\n");
		IP2BLMessageFactoryPtr msgs("P2ClientGate.P2BLMessageFactory");
		msgs->Init(L"p2fortsgate_messages.ini", L"");

		while( g_exit == FALSE )				
		{
			ULONG cookie;
			conn.GetConn()->ProcessMessage(&cookie, 100);
			if( _kbhit() ) 
			{
				if( _kbhit() )
				{
					int ch = _getch();
					IP2BLMessagePtr msg = msgs->CreateMessageByName(L"FutAddOrder");
					msg->DestAddr = srvAddr;
					msg->PutField("P2_Category", "FORTS_MSG");
					msg->PutField("P2_Type", 30);
					msg->PutField("isin", "LKOH-6.11");
					msg->PutField("price", "19000");
					msg->PutField("amount", "1");
					msg->PutField("client_code", "000");
					msg->PutField("type", 1);
					msg->PutField("dir", 1);
					if( ch == '1' ) 
					{
						printf("Send async AddOrder message\n");
						msg->SendAsync(conn.GetConn(), 10000, new CComObject<CAsyncMessageEvent>);
					}
					else
					{
						hyper eventParam = GetTickCount();
						eventParam <<= 32;
						eventParam +=GetTickCount();
						printf("SendAsync2 AddOrder message %I64d\n", eventParam);
						msg->SendAsync2(conn.GetConn(), 1000, new CComObject<CAsyncSendEvent2>, eventParam);
					}
				}
			}
		}
	}
	catch (_com_error e)
	{
		showerror(e);
	}
	return 0;
}