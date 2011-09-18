// SmartCOMWrapper.cpp : implementation file
//

#include "stdafx.h"
#include "SmartCOMWrapper.h"
#include <WinNT.h>
#include <WTypes.h>

#include <iostream>

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


LPCTSTR CSmartCOMWrapper::szFmtDate = L"Y-%m-%d";
LPCTSTR CSmartCOMWrapper::szFmtTime = L"%H:%M:%S";
LPCTSTR CSmartCOMWrapper::szFmtDateTime = L"%Y-%m-%d %H:%M:%S";

using namespace std;


namespace tx { namespace smartcom {

CSmartCOMWrapper::CSmartCOMWrapper() : 
								m_bReady(false), 
								m_hMtxSmartCOM(NULL), 
								m_pLogFile(NULL), 
								m_bIdle(false), 
								m_bOnLine(false), 
								m_bMessageBoxOnComException(FALSE), 
								m_bLogIsConnected(TRUE), 
								m_bLogconnect(TRUE), 
								m_bLogConnected(TRUE), 
								m_bLogdisconnect(TRUE), 
								m_bLogDisconnected(TRUE), 
								m_bLogGetSymbols(TRUE), 
								m_bLogAddSymbol(TRUE), 
								m_bLogListenQuotes(TRUE), 
								m_bLogCancelQuotes(TRUE), 
								m_bLogUpdateQuote(TRUE), 
								m_bLogListenBidAsks(TRUE), 
								m_bLogCancelBidAsks(TRUE), 
								m_bLogUpdateBidAsk(TRUE), 
								m_bLogListenTicks(TRUE), 
								m_bLogCancelTicks(TRUE), 
								m_bLogAddTick(TRUE), 
								m_bLogGetBars(TRUE), 
								m_bLogAddBar(TRUE), 
								m_bLogAddTickHistory(TRUE), 
								m_bLogGetPortfolioList(TRUE), 
								m_bLogAddPortfolio(TRUE), 
								m_bLogListenPortfolio(TRUE), 
								m_bLogCancelPortfolio(TRUE), 
								m_bLogSetPortfolio(TRUE), 
								m_bLogUpdatePosition(TRUE), 
								m_bLogAddTrade(TRUE), 
								m_bLogPlaceOrder(TRUE), 
								m_bLogOrderSucceeded(TRUE), 
								m_bLogOrderFailed(TRUE), 
								m_bLogCancelOrder(TRUE), 
								m_bLogOrderCancelSucceeded(TRUE), 
								m_bLogOrderCancelFailed(TRUE), 
								m_bLogMoveOrder(TRUE), 
								m_bLogOrderMoveSucceeded(TRUE), 
								m_bLogOrderMoveFailed(TRUE), 
								m_bLogUpdateOrder(TRUE), 
								m_bLogCancelAllOrders(TRUE),
								m_bLogGetTrades(TRUE),
								m_bLogGetMyOrders(TRUE), 
								m_bLogSetMyOrder(TRUE), 
								m_bLogGetMyTrades(TRUE), 
								m_bLogSetMyTrade(TRUE), 
								m_bLogGetMyOpenPos(TRUE), 
								m_bLogSetMyOpenPos(TRUE), 
								m_bLogGetMyClosePos(TRUE), 
								m_bLogSetMyClosePos(TRUE), 
								m_bLogCheckSubscription(TRUE), 
								m_bLogSetSubscriptionCheckResult(TRUE)
{
	
	// EnableAutomation();
}

CSmartCOMWrapper::~CSmartCOMWrapper()
{
	Terminate();
	if(m_pLogFile)
	{
		m_pLogFile->Close();
		delete m_pLogFile;
	}
}

bool CSmartCOMWrapper::Init()
{
	if(!Ready() && m_hMtxSmartCOM == NULL && (m_hMtxSmartCOM = ::CreateMutex(NULL, FALSE, _T("SmartCom2Wrapper"))) != NULL)
	{
		DWORD dwError = ::GetLastError();
		if(dwError != ERROR_ALREADY_EXISTS && dwError != ERROR_ACCESS_DENIED)
		{
			if(SUCCEEDED(m_IStServerPtr.GetActiveObject(CLSID_StServer)) || SUCCEEDED(m_IStServerPtr.CreateInstance(CLSID_StServer)))
			{
				if(::AfxConnectionAdvise(m_IStServerPtr, DIID__IStClient, GetIDispatch(FALSE), FALSE, &m_dwCookie))
				{
					if(m_pLogFile || (m_pLogFile = new CStdioFile(CTime::GetCurrentTime().Format(_T("Log_%Y-%m-%d_%H%M%S.txt")), CFile::modeCreate|CFile::modeWrite|CFile::shareDenyWrite)) != NULL)
					{
						m_bReady = true;
					}
					else
					{
						cout << MSG_LOG_CREATE_FAIL << NL;
						/// ::AfxMessageBox(_T("Can't to create Log File"), MB_ICONSTOP);
					}
					if(!Ready())
					{
						::AfxConnectionUnadvise(m_IStServerPtr, DIID__IStClient, GetIDispatch(FALSE), FALSE, m_dwCookie);
					}
				}
				else
				{
					::AfxMessageBox(_T("Can't to establish a connection between _IStClient and CSmartCOMWrapper"), MB_ICONSTOP);
				}
				if(!Ready())
				{
					m_IStServerPtr.Release();
				}
			}
			else
			{
				::AfxMessageBox(_T("Can't to create SmartCOM2 object"), MB_ICONSTOP);
			}
		}
		if(Ready())
			disconnect();
		else
		{
			::CloseHandle(m_hMtxSmartCOM);
			m_hMtxSmartCOM = NULL;
		}
	}
	return Ready();
}

void CSmartCOMWrapper::Terminate()
{
	HANDLE hMtxSmartCOM = m_hMtxSmartCOM;
	m_hMtxSmartCOM = NULL;
	if(Ready() && hMtxSmartCOM != NULL)
	{
		disconnect();
		m_bReady = false;
		::AfxConnectionUnadvise(m_IStServerPtr, DIID__IStClient, GetIDispatch(FALSE), FALSE, m_dwCookie);
		m_IStServerPtr.Release();
		::CloseHandle(hMtxSmartCOM);
	}
}

bool CSmartCOMWrapper::WriteLog(time_t TimeStamp, LPCTSTR message)
{
	bool bResult = false;
	if(m_pLogFile)
	{
		CString record;
		record.Format(_T("%s\t%s\n"), CTime(TimeStamp).Format(szFmtDateTime), message);
		TRY
		{
			m_pLogFile->WriteString(record);
			bResult = true;
		}
		CATCH(CFileException, pEx)
		{
			pEx->Delete();
		}
		END_CATCH
	}
	return bResult;
}

BEGIN_INTERFACE_MAP(CSmartCOMWrapper, CCmdTarget)
	INTERFACE_PART(CSmartCOMWrapper, DIID__IStClient, Dispatch)
END_INTERFACE_MAP()

BEGIN_DISPATCH_MAP(CSmartCOMWrapper, CCmdTarget)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "Connected",					0x01, Connected,					VT_UNKNOWN, VTS_NONE)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "Disconnected",				0x02, Disconnected,					VT_UNKNOWN, VTS_BSTR)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "UpdateQuote",				0x03, UpdateQuote,					VT_UNKNOWN, VTS_BSTR VTS_DATE VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_I4)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "UpdateBidAsk",				0x04, UpdateBidAsk,					VT_UNKNOWN, VTS_BSTR VTS_I4 VTS_I4 VTS_R8 VTS_R8 VTS_R8 VTS_R8)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "AddTick",					0x05, AddTick,						VT_UNKNOWN, VTS_BSTR VTS_DATE VTS_R8 VTS_R8 VTS_BSTR VTS_I4)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "AddBar",					0x06, AddBar,						VT_UNKNOWN, VTS_I4 VTS_I4 VTS_BSTR VTS_I4 VTS_DATE VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_R8)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "SetPortfolio",				0x07, SetPortfolio,					VT_UNKNOWN, VTS_BSTR VTS_R8 VTS_R8 VTS_R8 VTS_R8)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "AddTrade",					0x08, AddTrade,						VT_UNKNOWN, VTS_BSTR VTS_BSTR VTS_BSTR VTS_R8 VTS_R8 VTS_DATE VTS_BSTR)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "UpdateOrder",				0x09, UpdateOrder,					VT_UNKNOWN, VTS_BSTR VTS_BSTR VTS_I4 VTS_I4 VTS_I4 VTS_I4 VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_DATE VTS_BSTR VTS_BSTR VTS_I4)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "UpdatePosition",			0x0a, UpdatePosition,				VT_UNKNOWN, VTS_BSTR VTS_BSTR VTS_R8 VTS_R8 VTS_R8)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "AddTickHistory",			0x0f, AddTickHistory,				VT_UNKNOWN, VTS_I4 VTS_I4 VTS_BSTR VTS_DATE VTS_R8 VTS_R8 VTS_BSTR VTS_I4)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "AddSymbol",					0x14, AddSymbol,					VT_UNKNOWN,	VTS_I4 VTS_I4 VTS_BSTR VTS_BSTR VTS_BSTR VTS_BSTR VTS_I4 VTS_I4 VTS_R8 VTS_R8 VTS_BSTR VTS_BSTR VTS_DATE VTS_R8)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "OrderSucceeded",			0x1e, OrderSucceeded,				VT_UNKNOWN, VTS_I4 VTS_BSTR)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "OrderFailed",				0x1f, OrderFailed,					VT_UNKNOWN, VTS_I4 VTS_BSTR VTS_BSTR)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "AddPortfolio",				0x20, AddPortfolio,					VT_UNKNOWN, VTS_I4 VTS_I4 VTS_BSTR VTS_BSTR)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "SetSubscriptionCheckResult",0x21, SetSubscriptionCheckResult,	VT_UNKNOWN, VTS_I4)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "SetMyTrade",				0x22, SetMyTrade,					VT_UNKNOWN, VTS_I4 VTS_I4 VTS_BSTR VTS_BSTR VTS_DATE VTS_R8 VTS_R8 VTS_BSTR VTS_I4 VTS_BSTR)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "SetMyOrder",				0x23, SetMyOrder,					VT_UNKNOWN, VTS_I4 VTS_I4 VTS_BSTR VTS_BSTR VTS_I4 VTS_I4 VTS_I4 VTS_I4 VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_DATE VTS_BSTR VTS_BSTR)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "SetMyOpenPos",				0x24, SetMyOpenPos,					VT_UNKNOWN, VTS_I4 VTS_I4 VTS_BSTR VTS_BSTR VTS_R8 VTS_R8 VTS_R8)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "SetMyClosePos",				0x25, SetMyClosePos,				VT_UNKNOWN, VTS_I4 VTS_I4 VTS_BSTR VTS_BSTR VTS_R8 VTS_R8 VTS_R8 VTS_DATE VTS_BSTR VTS_BSTR)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "OrderCancelSucceeded",		0x26, OrderCancelSucceeded,			VT_UNKNOWN, VTS_BSTR)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "OrderCancelFailed",			0x27, OrderCancelFailed,			VT_UNKNOWN, VTS_BSTR)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "OrderMoveSucceeded",		0x28, OrderMoveSucceeded,			VT_UNKNOWN, VTS_BSTR)
	DISP_FUNCTION_ID(CSmartCOMWrapper, "OrderMoveFailed",			0x29, OrderMoveFailed,				VT_UNKNOWN, VTS_BSTR)
END_DISPATCH_MAP()

bool CSmartCOMWrapper::ListenQuotes(LPCTSTR symbol)
{
	bool bResult = false;
	if(OnLine() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogListenQuotes)
		{
			szFuncCall.Format(_T("ListenQuotes(symbol=%s)"), symbol);
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->ListenQuotes(symbol)) ? true : false;
			if(m_bLogListenQuotes)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::CancelQuotes(LPCTSTR symbol)
{
	bool bResult = false;
	if(Ready() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogCancelQuotes)
		{
			szFuncCall.Format(_T("CancelQuotes(symbol=%s)"), symbol);
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->CancelQuotes(symbol)) ? true : false;
			if(m_bLogCancelQuotes)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::ListenBidAsks(LPCTSTR symbol)
{
	bool bResult = false;
	if(OnLine() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogListenBidAsks)
		{
			szFuncCall.Format(_T("ListenBidAsks(symbol=%s)"), symbol);
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->ListenBidAsks(symbol)) ? true : false;
			if(m_bLogListenBidAsks)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::CancelBidAsks(LPCTSTR symbol)
{
	bool bResult = false;
	if(Ready() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogCancelBidAsks)
		{
			szFuncCall.Format(_T("CancelBidAsks(symbol=%s)"), symbol);
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->CancelBidAsks(symbol)) ? true : false;
			if(m_bLogCancelBidAsks)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::ListenTicks(LPCTSTR symbol)
{
	bool bResult = false;
	if(OnLine() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogListenTicks)
		{
			szFuncCall.Format(_T("ListenTicks(symbol=%s)"), symbol);
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->ListenTicks(symbol)) ? true : false;
			if(m_bLogListenTicks)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::CancelTicks(LPCTSTR symbol)
{
	bool bResult = false;
	if(Ready() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogCancelTicks)
		{
			szFuncCall.Format(_T("CancelTicks(symbol=%s)"), symbol);
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->CancelTicks(symbol)) ? true : false;
			if(m_bLogCancelTicks)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::GetBars(LPCTSTR symbol, StBarInterval interval, long count, time_t since)
{
	bool bResult = false;
	if(OnLine() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		SYSTEMTIME SinceTime;
		DATE VarSince;
		if(!time_t2SystemTime(since, SinceTime))
		{
			::GetLocalTime(&SinceTime);
		}
		::SystemTimeToVariantTime(&SinceTime, &VarSince);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogGetBars)
		{
			szFuncCall.Format(_T("GetBars(symbol=%s, interval=%s, since=%s, count=%d)"), symbol, BarInterval(interval), CTime(since).Format(szFmtDateTime), static_cast<int>(count));
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->GetBars(symbol, interval, VarSince, count)) ? true : false;
			if(m_bLogGetBars)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::ListenPortfolio(LPCTSTR portfolio)
{
	bool bResult = false;
	if(OnLine() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogListenPortfolio)
		{
			szFuncCall.Format(_T("ListenPortfolio(portfolio=%s)"), portfolio);
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->ListenPortfolio(portfolio)) ? true : false;
			if(m_bLogListenPortfolio)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::CancelPortfolio(LPCTSTR portfolio)
{
	bool bResult = false;
	if(Ready() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogCancelPortfolio)
		{
			szFuncCall.Format(_T("CancelPortfolio(portfolio=%s)"), portfolio);
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->CancelPortfolio(portfolio)) ? true : false;
			if(m_bLogCancelPortfolio)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::PlaceOrder(LPCTSTR portfolio, LPCTSTR symbol, StOrder_Action action, StOrder_Type type, StOrder_Validity validity, double price, double amount, double stop, long cookie)
{
	bool bResult = false;
	if(OnLine() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogPlaceOrder)
		{
			szFuncCall.Format(_T("PlaceOrder(portfolio=%s, symbol=%s, action=%s, type=%s, validity=%s, price=%g, amount=%g, stop=%g, cookie=%d)"), portfolio, symbol, OrderAction(action), OrderType(type), OrderValidity(validity), price, amount, stop, static_cast<int>(cookie));
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->PlaceOrder(portfolio, symbol, action, type, validity, price, amount, stop, cookie)) ? true : false;
			if(m_bLogPlaceOrder)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::CancelOrder(LPCTSTR portfolio, LPCTSTR symbol, LPCTSTR orderid)
{
	bool bResult = false;
	if(OnLine() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogCancelOrder)
		{
			szFuncCall.Format(_T("CancelOrder(portfolio=%s, symbol=%s, orderid=%s)"), portfolio, symbol, orderid);
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->CancelOrder(portfolio, symbol, orderid)) ? true : false;
			if(m_bLogCancelOrder)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::MoveOrder(LPCTSTR portfolio, LPCTSTR orderid, double targetPrice)
{
	bool bResult = false;
	if(OnLine() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogMoveOrder)
		{
			szFuncCall.Format(_T("MoveOrder(portfolio=%s, orderid=%s, targetPrice=%g)"), portfolio, orderid, targetPrice);
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->MoveOrder(portfolio, orderid, targetPrice)) ? true : false;
			if(m_bLogMoveOrder)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::GetSymbols()
{
	bool bResult = false;
	if(OnLine() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogGetSymbols)
		{
			szFuncCall = _T("GetSymbols()");
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->GetSymbols()) ? true : false;
			if(m_bLogGetSymbols)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::IsConnected()
{
	bool bResult = false;
	if(Ready() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogIsConnected)
		{
			szFuncCall = _T("IsConnected()");
		}
		try
		{
			bResult = m_IStServerPtr->IsConnected() ? true : false;
			if(m_bLogIsConnected)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::connect(LPCTSTR ip, short port, LPCTSTR login, LPCTSTR password)
{
	bool bResult = false;
	if(Ready() && !Idle() && !OnLine())
	{
		m_bIdle = true;
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogconnect)
		{
			szFuncCall.Format(_T("connect(ip=%s, port=%d, login=%s)"), ip, static_cast<int>(port), login);
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->connect(ip, port, login, password)) ? true : false;
			if(m_bLogconnect)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			m_bIdle = false;
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::disconnect()
{
	bool bResult = false;
	if(Ready() && !Idle() && OnLine())
	{
		m_bIdle = true;
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogdisconnect)
		{
			szFuncCall = _T("disconnect()");
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->disconnect()) ? true : false;
			if(m_bLogdisconnect)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			m_bIdle = false;
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::GetPortfolioList()
{
	bool bResult = false;
	if(OnLine() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogGetPortfolioList)
		{
			szFuncCall = _T("GetPortfolioList()");
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->GetPrortfolioList()) ? true : false;
			if(m_bLogGetPortfolioList)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::CancelAllOrders()
{
	bool bResult = false;
	if(OnLine() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogCancelAllOrders)
		{
			szFuncCall = _T("CancelAllOrders()");
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->CancelAllOrders()) ? true : false;
			if(m_bLogCancelAllOrders)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::CheckSubscription(LPCTSTR subscriptionId)
{
	bool bResult = false;
	if(OnLine() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogCheckSubscription)
		{
			szFuncCall.Format(_T("CheckSubscription(subscriptionId=%s)"), subscriptionId);
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->CheckSubscribtion(subscriptionId)) ? true : false;
			if(m_bLogCheckSubscription)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::GetMyOrders(BOOL onlyActive, LPCTSTR portfolio)
{
	bool bResult = false;
	if(OnLine() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogGetMyOrders)
		{
			szFuncCall.Format(_T("GetMyOrders(onlyActive=%s, portfolio=%s)"), onlyActive ? _T("TRUE") : _T("FALSE"), portfolio);
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->GetMyOrders(onlyActive, portfolio)) ? true : false;
			if(m_bLogGetMyOrders)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::GetMyTrades(LPCTSTR portfolio)
{
	bool bResult = false;
	if(OnLine() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogGetMyTrades)
		{
			szFuncCall.Format(_T("GetMyTrades(portfolio=%s)"), portfolio);
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->GetMyTrades(portfolio)) ? true : false;
			if(m_bLogGetMyTrades)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::GetMyOpenPos(LPCTSTR portfolio)
{
	bool bResult = false;
	if(OnLine() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogGetMyOpenPos)
		{
			szFuncCall.Format(_T("GetMyOpenPos(portfolio=%s)"), portfolio);
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->GetMyOpenPos(portfolio)) ? true : false;
			if(m_bLogGetMyOpenPos)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::GetMyClosePos(LPCTSTR portfolio)
{
	bool bResult = false;
	if(OnLine() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogGetMyClosePos)
		{
			szFuncCall.Format(_T("GetMyClosePos(portfolio=%s)"), portfolio);
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->GetMyClosePos(portfolio)) ? true : false;
			if(m_bLogGetMyClosePos)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

bool CSmartCOMWrapper::GetTrades(LPCTSTR symbol, long count, time_t since)
{
	bool bResult = false;
	if(OnLine() && !Idle())
	{
		time_t TimeStamp = time(NULL);
		SYSTEMTIME SinceTime;
		DATE VarSince;
		if(!time_t2SystemTime(since, SinceTime))
		{
			::GetLocalTime(&SinceTime);
		}
		::SystemTimeToVariantTime(&SinceTime, &VarSince);
		CString szFuncCall;
		if(m_bMessageBoxOnComException || m_bLogGetTrades)
		{
			szFuncCall.Format(_T("GetTrades(symbol=%s, since=%s, count=%d)"), symbol, CTime(since).Format(szFmtDateTime), static_cast<int>(count));
		}
		try
		{
			bResult = SUCCEEDED(m_IStServerPtr->GetTrades(symbol, VarSince, count)) ? true : false;
			if(m_bLogGetTrades)
			{
				WriteLog(TimeStamp, szFuncCall);
			}
		}
		catch(_com_error&)
		{
			if(m_bMessageBoxOnComException)
			{
				::AfxMessageBox(szFuncCall, MB_ICONSTOP|MB_OK);
			}
		}
	}
	return bResult;
}

HRESULT CSmartCOMWrapper::Connected()
{
	m_bIdle = false, m_bOnLine = true;
	if(m_bLogConnected)
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall = _T("Connected()");
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::Disconnected(LPCTSTR reason)
{
	m_bOnLine = m_bIdle = false;
	if(m_bLogDisconnected)
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		szFuncCall.Format(_T("Disconnected(reason=%s)"), reason);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::UpdateQuote(LPCTSTR symbol, DATE datetime, double open, double high, double low, double close, double last, double volume, double size, double bid, double ask, double bidsize, double asksize, double open_int, double go_buy, double go_sell, double go_base, double go_base_backed, double high_limit, double low_limit, long trading_status)
{
	time_t TimeStamp = time(NULL), AnsiDateTime;
	UDATE ud;
	if(SUCCEEDED(::VarUdateFromDate(datetime, 0, &ud)))
	{
		AnsiDateTime = SystemTime2time_t(ud.st);
	}
	else
	{
		AnsiDateTime = static_cast<time_t>(-1);
	}
	if(m_bLogUpdateQuote)
	{
		CString szFuncCall;
		szFuncCall.Format(_T("UpdateQuote(symbol=%s, datetime=%s, open=%g, high=%g, low=%g, close=%g, last=%g, volume=%g, size=%g, bid=%g, ask=%g, bidsize=%g, asksize=%g, open_int=%g, go_buy=%g, go_sell=%g, go_base=%g, go_base_backed=%g, high_limit=%g, low_limit=%g, trading_status=%d)"),
			symbol, CTime(AnsiDateTime).Format(szFmtTime),
			open, high, low, close, last, volume, size, bid, ask, bidsize, asksize, open_int,
			go_buy, go_sell, go_base, go_base_backed, high_limit, low_limit, static_cast<int>(trading_status)
		);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::UpdateBidAsk(LPCTSTR symbol, long row, long nrows, double bid, double bidsize, double ask, double asksize)
{
	if(m_bLogUpdateBidAsk)
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		szFuncCall.Format(_T("UpdateBidAsk(symbol=%s, row=%d, nrows=%d, bid=%g, bidsize=%g, ask=%g, asksize=%g)"), symbol, static_cast<int>(row), static_cast<int>(nrows), bid, bidsize, ask, asksize);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::AddTick(LPCTSTR symbol, DATE datetime, double price, double volume, LPCTSTR tradeno, StOrder_Action action)
{
	time_t TimeStamp = time(NULL), AnsiDateTime;
	UDATE ud;
	if(SUCCEEDED(::VarUdateFromDate(datetime, 0, &ud)))
	{
		AnsiDateTime = SystemTime2time_t(ud.st);
	}
	else
	{
		AnsiDateTime = static_cast<time_t>(-1);
	}
	if(m_bLogAddTick)
	{
		CString szFuncCall;
		szFuncCall.Format(_T("AddTick(symbol=%s, datetime=%s, price=%g, volume=%g, tradeno=%s, action=%s)"), symbol, CTime(AnsiDateTime).Format(szFmtTime), price, volume, tradeno, OrderAction(action));
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::AddBar(long row, long nrows, LPCTSTR symbol, StBarInterval interval, DATE datetime, double open, double high, double low, double close, double volume)
{
	time_t TimeStamp = time(NULL), AnsiDateTime;
	UDATE ud;
	if(SUCCEEDED(::VarUdateFromDate(datetime, 0, &ud)))
	{
		AnsiDateTime = SystemTime2time_t(ud.st);
	}
	else
	{
		AnsiDateTime = static_cast<time_t>(-1);
	}
	if(m_bLogAddBar)
	{
		CString szFuncCall;
		szFuncCall.Format(_T("AddBar(row=%d, nrows=%d, symbol=%s, interval=%s, datetime=%s, open=%g, high=%g, low=%g, close=%g, volume=%g)"),
			static_cast<int>(row), static_cast<int>(nrows), symbol, BarInterval(interval), CTime(AnsiDateTime).Format(szFmtDateTime), open, high, low, close, volume
		);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::SetPortfolio(LPCTSTR portfolio, double cash, double leverage, double comission, double saldo)
{
	if(m_bLogSetPortfolio)
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		szFuncCall.Format(_T("SetPortfolio(portfolio=%s, cash=%g, leverage=%g, comission=%g, saldo=%g)"), portfolio, cash, leverage, comission, saldo);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::AddTrade(LPCTSTR portfolio, LPCTSTR symbol, LPCTSTR orderid, double price, double amount, DATE datetime, LPCTSTR tradeno)
{
	time_t TimeStamp = time(NULL), AnsiDateTime;
	UDATE ud;
	if(SUCCEEDED(::VarUdateFromDate(datetime, 0, &ud)))
	{
		AnsiDateTime = SystemTime2time_t(ud.st);
	}
	else
	{
		AnsiDateTime = static_cast<time_t>(-1);
	}
	if(m_bLogAddTrade)
	{
		CString szFuncCall;
		szFuncCall.Format(_T("AddTrade(portfolio=%s, symbol=%s, orderid=%s, price=%g, amount=%g, datetime=%s, tradeno=%s)"),
			portfolio, symbol, orderid, price, amount, CTime(AnsiDateTime).Format(szFmtDateTime), tradeno
		);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::UpdateOrder(LPCTSTR portfolio, LPCTSTR symbol, StOrder_State state, StOrder_Action action, StOrder_Type type, StOrder_Validity validity, double price, double amount, double stop, double filled, DATE datetime, LPCTSTR orderid, LPCTSTR orderno, long status_mask)
{
	time_t TimeStamp = time(NULL), AnsiDateTime;
	UDATE ud;
	if(SUCCEEDED(::VarUdateFromDate(datetime, 0, &ud)))
	{
		AnsiDateTime = SystemTime2time_t(ud.st);
	}
	else
	{
		AnsiDateTime = static_cast<time_t>(-1);
	}
	if(m_bLogUpdateOrder)
	{
		CString szFuncCall;
		szFuncCall.Format(_T("UpdateOrder(portfolio=%s, symbol=%s, state=%s, action=%s, type=%s, validity=%s, price=%g, amount=%g, stop=%g, filled=%g, datetime=%s, orderid=%s, orderno=%s, status_mask=%d)"),
			portfolio, symbol, OrderState(state), OrderAction(action), OrderType(type), OrderValidity(validity),
			price, amount, stop, filled, CTime(AnsiDateTime).Format(szFmtDateTime), orderid, orderno, static_cast<int>(status_mask)
		);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::UpdatePosition(LPCTSTR portfolio, LPCTSTR symbol, double avprice, double amount, double planned)
{
	if(m_bLogUpdatePosition)
	{
		time_t TimeStamp = time(NULL);
		SYSTEMTIME LclTime;
		::GetLocalTime(&LclTime);
		CString szFuncCall;
		szFuncCall.Format(_T("UpdatePosition(portfolio=%s, symbol=%s, avprice=%g, amount=%g, planned=%g)"), portfolio, symbol, avprice, amount, planned);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::AddTickHistory(long row, long nrows, LPCTSTR symbol, DATE datetime, double price, double volume, LPCTSTR tradeno, StOrder_Action action)
{
	time_t TimeStamp = time(NULL), AnsiDateTime;
	UDATE ud;
	if(SUCCEEDED(::VarUdateFromDate(datetime, 0, &ud)))
	{
		AnsiDateTime = SystemTime2time_t(ud.st);
	}
	else
	{
		AnsiDateTime = static_cast<time_t>(-1);
	}
	if(m_bLogAddTickHistory)
	{
		CString szFuncCall;
		szFuncCall.Format(_T("AddTickHistory(row=%d, nrows=%d, symbol=%s, datetime=%s, price=%g, volume=%g, tradeno=%s, action=%s)"),
			static_cast<int>(row), static_cast<int>(nrows), symbol, CTime(AnsiDateTime).Format(szFmtDateTime), price, volume, tradeno, OrderAction(action)
		);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::AddSymbol(long row, long nrows, LPCTSTR symbol, LPCTSTR short_name, LPCTSTR long_name, LPCTSTR type, long decimals, long lot_size, double punkt, double step, LPCTSTR sec_ext_id, LPCTSTR sec_exch_name, DATE expiry_date, double days_before_expiry)
{
	time_t TimeStamp = time(NULL), AnsiDateTime;
	UDATE ud;
	if(SUCCEEDED(::VarUdateFromDate(expiry_date, 0, &ud)))
	{
		AnsiDateTime = SystemTime2time_t(ud.st);
	}
	else
	{
		AnsiDateTime = static_cast<time_t>(-1);
	}
	if(m_bLogAddSymbol)
	{
		CString szFuncCall;
		szFuncCall.Format(_T("AddSymbol(row=%d, nrows=%d, symbol=%s, short_name=%s, long_name=%s, type=%s, decimals=%d, lot_size=%d, punkt=%g, step=%g, sec_ext_id=%s, sec_exch_name=%s, expiry_date=%s, days_before_expiry=%g)"),
			static_cast<int>(row), static_cast<int>(nrows), symbol, short_name, long_name, type,
			static_cast<int>(decimals), static_cast<int>(lot_size), punkt, step, sec_ext_id, sec_exch_name, CTime(AnsiDateTime).Format(szFmtDate), days_before_expiry
		);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::OrderSucceeded(long cookie, LPCTSTR orderid)
{
	if(m_bLogOrderSucceeded)
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		szFuncCall.Format(_T("OrderSucceeded(cookie=%d, orderid=%s)"), static_cast<int>(cookie), orderid);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::OrderFailed(long cookie, LPCTSTR orderid, LPCTSTR reason)
{
	if(m_bLogOrderFailed)
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		szFuncCall.Format(_T("OrderFailed(cookie=%d, orderid=%s, reason=%s)"), static_cast<int>(cookie), orderid, reason);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::AddPortfolio(long row, long nrows, LPCTSTR portfolioName, LPCTSTR portfolioExch)
{
	if(m_bLogAddPortfolio)
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		szFuncCall.Format(_T("AddPortfolio(row=%d, nrows=%d, portfolioName=%s, portfolioExch=%s)"), static_cast<int>(row), static_cast<int>(nrows), portfolioName, portfolioExch);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::SetSubscriptionCheckResult(BOOL result)
{
	if(m_bLogSetSubscriptionCheckResult)
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		szFuncCall.Format(_T("SetSubscribtionCheckResult(result=%s)"), result ? _T("TRUE") : _T("FALSE"));
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::SetMyTrade(long row, long nrows, LPCTSTR portfolio, LPCTSTR symbol, DATE datetime, double price, double volume, LPCTSTR tradeno, StOrder_Action buysell, LPCTSTR orderno)
{
	time_t TimeStamp = time(NULL), AnsiDateTime;
	UDATE ud;
	if(SUCCEEDED(::VarUdateFromDate(datetime, 0, &ud)))
	{
		AnsiDateTime = SystemTime2time_t(ud.st);
	}
	else
	{
		AnsiDateTime = static_cast<time_t>(-1);
	}
	if(m_bLogSetMyTrade)
	{
		CString szFuncCall;
		szFuncCall.Format(_T("SetMyTrade(row=%d, nrows=%d, portfolio=%s, symbol=%s, datetime=%s, price=%g, volume=%g, tradeno=%s, buysell=%s, orderno)"),
			static_cast<int>(row), static_cast<int>(nrows), portfolio, symbol, CTime(AnsiDateTime).Format(szFmtDateTime), price, volume, tradeno, OrderAction(buysell), orderno
		);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::SetMyOrder(long row, long nrows, LPCTSTR portfolio, LPCTSTR symbol, StOrder_State state, StOrder_Action action, StOrder_Type type, StOrder_Validity validity, double price, double amount, double stop, double filled, DATE datetime, LPCTSTR id, LPCTSTR no)
{
	time_t TimeStamp = time(NULL), AnsiDateTime;
	UDATE ud;
	if(SUCCEEDED(::VarUdateFromDate(datetime, 0, &ud)))
	{
		AnsiDateTime = SystemTime2time_t(ud.st);
	}
	else
	{
		AnsiDateTime = static_cast<time_t>(-1);
	}
	if(m_bLogSetMyOrder)
	{
		CString szFuncCall;
		szFuncCall.Format(_T("SetMyOrder(row=%d, nrows=%d, portfolio=%s, symbol=%s, state=%s, action=%s, type=%s, validity=%s, price=%g, amount=%g, stop=%g, filled=%g, datetime=%s, id=%s, no=%s)"),
			static_cast<int>(row), static_cast<int>(nrows), portfolio, symbol, OrderState(state), OrderAction(action), OrderType(type), OrderValidity(validity),
			price, amount, stop, filled, CTime(AnsiDateTime).Format(szFmtDateTime), id, no
		);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::SetMyOpenPos(long row, long nrows, LPCTSTR portfolio, LPCTSTR symbol, double avprice, double amount, double planned)
{
	if(m_bLogSetMyOpenPos)
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		szFuncCall.Format(_T("SetMyOpenPos(row=%d, nrows=%d, portfolio=%s, symbol=%s, avprice=%g, amount=%g, planned=%g)"), static_cast<int>(row), static_cast<int>(nrows), portfolio, symbol, avprice, amount, planned);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::SetMyClosePos(long row, long nrows, LPCTSTR portfolio, LPCTSTR symbol, double amount, double price_buy, double price_sell, DATE postime, LPCTSTR order_open, LPCTSTR order_close)
{
	time_t TimeStamp = time(NULL), AnsiDateTime;
	UDATE ud;
	if(SUCCEEDED(::VarUdateFromDate(postime, 0, &ud)))
	{
		AnsiDateTime = SystemTime2time_t(ud.st);
	}
	else
	{
		AnsiDateTime = static_cast<time_t>(-1);
	}
	if(m_bLogSetMyClosePos)
	{
		CString szFuncCall;
		szFuncCall.Format(_T("SetMyClosePos(row=%d, nrows=%d, portfolio=%s, symbol=%s, amount=%g, price_buy=%g, price_sell=%g, postime=%s, order_open=%s, order_close=%s)"),
			static_cast<int>(row), static_cast<int>(nrows), portfolio, symbol, amount, price_buy, price_sell, CTime(AnsiDateTime).Format(szFmtDateTime), order_open, order_close
		);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::OrderCancelSucceeded(LPCTSTR orderid)
{
	if(m_bLogOrderCancelSucceeded)
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		szFuncCall.Format(_T("OrderCancelSucceeded(orderid=%s)"), orderid);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::OrderCancelFailed(LPCTSTR orderid)
{
	if(m_bLogOrderCancelFailed)
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		szFuncCall.Format(_T("OrderCancelFailed(orderid=%s)"), orderid);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::OrderMoveSucceeded(LPCTSTR orderid)
{
	if(m_bLogOrderMoveSucceeded)
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		szFuncCall.Format(_T("OrderMoveSucceeded(orderid=%s)"), orderid);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

HRESULT CSmartCOMWrapper::OrderMoveFailed(LPCTSTR orderid)
{
	if(m_bLogOrderMoveFailed)
	{
		time_t TimeStamp = time(NULL);
		CString szFuncCall;
		szFuncCall.Format(_T("OrderMoveFailed(orderid=%s)"), orderid);
		WriteLog(TimeStamp, szFuncCall);
	}
	return S_OK;
}

LPCTSTR CSmartCOMWrapper::OrderState(StOrder_State state)
{
	LPCTSTR szState;
	switch(state)
	{
	case StOrder_State_ContragentReject:
		szState = _T("ContragentReject");
		break;
	case StOrder_State_Submited:
		szState = _T("Submited");
		break;
	case StOrder_State_Pending:
		szState = _T("Pending");
		break;
	case StOrder_State_Open:
		szState = _T("Open");
		break;
	case StOrder_State_Expired:
		szState = _T("Expired");
		break;
	case StOrder_State_Cancel:
		szState = _T("Cancel");
		break;
	case StOrder_State_Filled:
		szState = _T("Filled");
		break;
	case StOrder_State_Partial:
		szState = _T("Partial");
		break;
	case StOrder_State_ContragentCancel:
		szState = _T("ContragentCancel");
		break;
	case StOrder_State_SystemReject:
		szState = _T("SystemReject");
		break;
	case StOrder_State_SystemCancel:
		szState = _T("SystemCancel");
		break;
	default:
		szState = NULL;
		break;
	}
	return szState;
}

LPCTSTR CSmartCOMWrapper::OrderAction(StOrder_Action action)
{
	LPCTSTR szAction;
	static const  [
	static const 
	switch(action)
	{
	case StOrder_Action_Buy:
		szAction = _T("Buy");
		break;
	case StOrder_Action_Sell:
		szAction = _T("Sell");
		break;
	case StOrder_Action_Short:
		szAction = _T("Short");
		break;
	case StOrder_Action_Cover:
		szAction = _T("Cover");
		break;
	default:
		szAction = NULL;
		break;
	}
	return szAction;
}

LPCTSTR CSmartCOMWrapper::OrderType(StOrder_Type type)
{
	LPCTSTR szType;
	switch(type)
	{
	case StOrder_Type_Market:
		szType = _T("Market");
		break;
	case StOrder_Type_Limit:
		szType = _T("Limit");
		break;
	case StOrder_Type_Stop:
		szType = _T("Stop");
		break;
	case StOrder_Type_StopLimit:
		szType = _T("StopLimit");
		break;
	default:
		szType = NULL;
		break;
	}
	return szType;
}

LPCTSTR CSmartCOMWrapper::OrderValidity(StOrder_Validity validity)
{
	LPCTSTR szValidity;
	switch(validity)
	{
	case StOrder_Validity_Day:
		szValidity = _T("Day");
		break;
	case StOrder_Validity_Gtc:
		szValidity = _T("Gtc");
		break;
	default:
		szValidity = NULL;
		break;
	}
	return szValidity;
}

LPCTSTR CSmartCOMWrapper::BarInterval(StBarInterval interval)
{
	LPCTSTR szInterval;
	switch(interval)
	{
	case StBarInterval_Tick:
		szInterval = _T("Tick");
		break;
	case StBarInterval_1Min:
		szInterval = _T("1Min");
		break;
	case StBarInterval_5Min:
		szInterval = _T("5Min");
		break;
	case StBarInterval_10Min:
		szInterval = _T("10Min");
		break;
	case StBarInterval_15Min:
		szInterval = _T("15Min");
		break;
	case StBarInterval_30Min:
		szInterval = _T("30Min");
		break;
	case StBarInterval_60Min:
		szInterval = _T("60Min");
		break;
	case StBarInterval_2Hour:
		szInterval = _T("2Hour");
		break;
	case StBarInterval_4Hour:
		szInterval = _T("4Hour");
		break;
	case StBarInterval_Day:
		szInterval = _T("Day");
		break;
	case StBarInterval_Week:
		szInterval = _T("Week");
		break;
	case StBarInterval_Month:
		szInterval = _T("Month");
		break;
	case StBarInterval_Quarter:
		szInterval = _T("Quarter");
		break;
	case StBarInterval_Year:
		szInterval = _T("Year");
		break;
	default:
		szInterval = NULL;
		break;
	}
	return szInterval;
}

time_t CSmartCOMWrapper::SystemTime2time_t(SYSTEMTIME& SysTime)
{
	struct tm Tm;
	Tm.tm_sec	= SysTime.wSecond;
	Tm.tm_min	= SysTime.wMinute;
	Tm.tm_hour	= SysTime.wHour;
	Tm.tm_mday	= SysTime.wDay;
	Tm.tm_mon	= SysTime.wMonth - 1;
	Tm.tm_year	= SysTime.wYear - 1900;
	Tm.tm_isdst	= 0;
	return mktime(&Tm);
}

bool CSmartCOMWrapper::time_t2SystemTime(time_t DateTime, SYSTEMTIME& SysTime)
{
	struct tm Tm;
	if(::localtime_s(&Tm, &DateTime) == 0)
	{
		SysTime.wYear = static_cast<WORD>(Tm.tm_year + 1900);
		SysTime.wMonth = static_cast<WORD>(Tm.tm_mon + 1);
		SysTime.wDay = static_cast<WORD>(Tm.tm_mday);
		SysTime.wDayOfWeek = static_cast<WORD>(Tm.tm_wday);
		SysTime.wHour = static_cast<WORD>(Tm.tm_hour);
		SysTime.wMinute = static_cast<WORD>(Tm.tm_min);
		SysTime.wSecond = static_cast<WORD>(Tm.tm_sec);
		SysTime.wMilliseconds = 0;
		return true;
	}
	return false;
}


}		// smartcom
}		// tx