/*
 *  Copyright (C) 2010 Alexander Chernykh 
 *
 *  This file is part of MyQuik: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  MyQuik is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 *  MyQuik.cpp : main source file for MyQuik.exe
 */

#include "stdafx.h"
#include "resource.h"
#include "MyQuik.h"
#include "TradeDlg2.h"
#include <color.h>
#include <ctrl.h>


#ifdef _DEBUG
#define new DEBUG_NEW
#endif

#pragma comment(linker, "/SECTION:.shr,RWS")
#pragma data_seg(".shr")
HWND g_hMainWnd = NULL;
#pragma data_seg()

CMyQuikApp _Module;

int WINAPI _tWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpstrCmdLine, int nCmdShow)
{
#if 1
	// Защита от одновременного запуска более одного приложения:
	bool alreadyRunning;
	HANDLE hMutexOneInstance;
	hMutexOneInstance = ::CreateMutex(NULL, TRUE, MYQUIKDDECLIENT TEXT("-F0EF5051-3266-42ba-9FFD-34314987F3A5"));
    alreadyRunning = (GetLastError() == ERROR_ALREADY_EXISTS);
    if (hMutexOneInstance != NULL) 
    {
        ::ReleaseMutex(hMutexOneInstance);
    }
    if (alreadyRunning)
	{
#if 1
		HWND hOther = g_hMainWnd;
        if (hOther != NULL)
		{
			::PostMessage(hOther, UM_RESTORE, 0, 0);
		}
#endif
		return FALSE;
	}
#endif
#ifdef _DEBUG
	_CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF|_CRTDBG_LEAK_CHECK_DF);
#endif

	HRESULT hRes = ::CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
	ATLASSERT(SUCCEEDED(hRes));

	// this resolves ATL window thunking problem when Microsoft Layer for Unicode (MSLU) is used
	::DefWindowProc(NULL, 0, 0, 0L);

	AtlInitCommonControls(ICC_BAR_CLASSES);	// add flags to support other controls

	hRes = theApp.Init(NULL, hInstance);
	ATLASSERT(SUCCEEDED(hRes));

	theApp.InitInstance(GetCommandLineW());

	int nRet = 0;
	// BLOCK: Run application
	{
		CTradeDlg2 dlgMain;

		theApp.m_hWnd = dlgMain;

		nRet = dlgMain.DoModal();
	}

	theApp.ExitInstance();

	theApp.Term();
	::CoUninitialize();

	return nRet;
}


//
// class Application
//

Application::Application()
{
	m_hWnd = NULL;
}

int Application::SaveWindowPosition(const LPCREATESTRUCT lpCreateStruct, LPCTSTR name, int flags)
{
	int status = S_OK;
	using namespace my::lib;

	BOOL changed = FALSE;
	if (! (flags & my::lib::FWP_FORCE))
	{
		CREATESTRUCT cs;
		memset(&cs, 0, sizeof(cs));
		LoadWindowPosition(&cs, name, flags);				
		if (flags & FWP_X)
			changed = (cs.x != lpCreateStruct->x);
		if (flags & FWP_Y)
			changed = (cs.y != lpCreateStruct->y);
		if (flags & FWP_CX)
			changed |= (cs.cx != lpCreateStruct->cx);
		if (flags & FWP_CY)
			changed |= (cs.cy != lpCreateStruct->cy);
		if (flags & FWP_STYLE)
			changed |= (cs.style != lpCreateStruct->style);
	}
	else
		changed = TRUE;
	if (changed)		
	{// Сохраняем в реестре только после изменения положения или размеров окна:
		CString path;
		path.Format(TEXT("Workspace\\%s"), name); 	
		CRegKey key;
		key.Create(GetRegKey(), path);

		if ((lpCreateStruct->style & WS_MAXIMIZE) == FALSE)
		{
			if (flags & FWP_X)
				key.SetDWORDValue(TEXT("x"), lpCreateStruct->x);
			if (flags & FWP_Y)
				key.SetDWORDValue(TEXT("y"), lpCreateStruct->y);
			if (flags & FWP_CX)
				key.SetDWORDValue(TEXT("cx"), lpCreateStruct->cx);
			if (flags & FWP_CY)
				key.SetDWORDValue(TEXT("cy"), lpCreateStruct->cy);
		}
		if (flags & FWP_STYLE)
			key.SetDWORDValue(TEXT("style"), lpCreateStruct->style);
	}
	return status;
}

int Application::LoadWindowPosition(LPCREATESTRUCT lpCreateStruct, LPCTSTR name, int flags)
{
	int result;	

	CString path;
	path.Format(TEXT("Workspace\\%s"), name); 	
	CRegKey key;
	result = key.Open(GetRegKey(), path);

	if (result != ERROR_SUCCESS)
		return result;

	using namespace my::lib;
	ULONG val;
	if (flags & FWP_X) 
	{
		result = key.QueryDWORDValue(TEXT("x"), val);
		if (result == ERROR_SUCCESS)
			lpCreateStruct->x = (int)val;
		else
			return result;
	} 
	if (flags & FWP_Y) 
	{
		result = key.QueryDWORDValue(TEXT("y"), val);
		if (result == ERROR_SUCCESS)
			lpCreateStruct->y = (int)val;
		else
			return result;
	} 
	if (flags & FWP_CX)
	{
		result = key.QueryDWORDValue(TEXT("cx"), val);
		if (result == ERROR_SUCCESS)
			lpCreateStruct->cx = (int)val;
		else
			return result;
	} 
	if (flags & FWP_CY)
	{
		result = key.QueryDWORDValue(TEXT("cy"), val);
		if (result == ERROR_SUCCESS)
			lpCreateStruct->cy = (int)val;
		else
			return result;
	} 
	if (flags & FWP_STYLE)
	{
		result = key.QueryDWORDValue(TEXT("style"), val);
		if (result == ERROR_SUCCESS)
			lpCreateStruct->style = (int)val;
		else
			return result;
	} 
	if (flags & FWP_DIALOG)
	{
		const int cxborder = ::GetSystemMetrics(SM_CXFIXEDFRAME);
		const int cyborder = ::GetSystemMetrics(SM_CYFIXEDFRAME);
		lpCreateStruct->cx += 2*cxborder;
		lpCreateStruct->cy += 2*cyborder;
	}
	return result;
}
#if 0
int Application::SaveWindowPosition(HWND hWnd, LPCTSTR name, int flags)
{
	CRect rect;
	::GetWindowRect(hWnd, &rect);
	CREATESTRUCT cs;
	cs.x = rect.left; cs.y = rect.top;
	cs.cx = rect.Width(); cs.cy = rect.Height();
	cs.style = ::GetWindowLong(hWnd, GWL_STYLE);
	return SaveWindowPosition(&cs, name, flags);
}
#else
int Application::SaveWindowPosition(HWND hWnd, LPCTSTR name, int flags)
{
	int status = S_OK;

	CREATESTRUCT createStruct;
	WINDOWINFO wi;
	wi.cbSize = sizeof(wi);
	::GetWindowInfo(hWnd, &wi);
	//createStruct.lpszClass;
	createStruct.x = wi.rcWindow.left;// + wi.cxWindowBorders; 
	createStruct.y = wi.rcWindow.top;// + wi.cyWindowBorders;
	if (flags & my::lib::FWP_DIALOG)
	{
		createStruct.cx = wi.rcWindow.right - wi.rcWindow.left - 2*wi.cxWindowBorders; 
		createStruct.cy = wi.rcWindow.bottom - wi.rcWindow.top - 2*wi.cyWindowBorders;
	}
	else
	{
		createStruct.cx = wi.rcWindow.right - wi.rcWindow.left;// - 2*wi.cxWindowBorders; 
		createStruct.cy = wi.rcWindow.bottom - wi.rcWindow.top;// - 2*wi.cyWindowBorders;
	}
	createStruct.style = wi.dwStyle;
	TSTRING_STD2(text, size);
	::GetWindowText(hWnd, text, size);
	createStruct.lpszName = text;

	status = SaveWindowPosition(&createStruct, name, flags);

	return status;
}
#endif
int Application::SaveDlgWindowPosition(HWND hWnd, LPCTSTR name)
{
	using namespace my::lib;
	return SaveWindowPosition(hWnd, name, FWP_DIALOG|FWP_GENERIC);
}

int Application::LoadDlgWindowPosition(LPCREATESTRUCT lpCreateStruct, LPCTSTR name)
{
	using namespace my::lib;
	return Application::LoadWindowPosition(lpCreateStruct, name, FWP_GENERIC|FWP_DIALOG);
}

int Application::ClearWindowPosition(LPCTSTR name, int flags)
{
	CString path;
	path.Format(TEXT("Workspace\\%s"), name); 	
	CRegKey key;
	LONG result = key.Open(GetRegKey(), path);
	if (result == ERROR_SUCCESS)
	{
		using namespace my::lib;

		if (flags & FWP_X)
			key.DeleteValue(TEXT("x"));
		if (flags & FWP_Y)
			key.DeleteValue(TEXT("y"));
		if (flags & FWP_CX)
			key.DeleteValue(TEXT("cx"));
		if (flags & FWP_CY)
			key.DeleteValue(TEXT("cy"));
		if (flags & FWP_STYLE)
			key.DeleteValue(TEXT("style"));
	}
	return result;
}

int Application::LoadVersion()
{
	CRegKey key;
	key.Attach(GetRegKey());
	DWORD val;
	LONG result = key.QueryDWORDValue(TEXT("current_version"), val);
	if (result == ERROR_SUCCESS)
		m_product.version.reg = (int)val;
	else
		m_product.version.reg = VERSION_0_3_1;
	key.Detach();
#if 1
	m_product.version.cur = 
		(LOBYTE(HIWORD(m_product.info.versionMS)) << 24) | 
		(LOBYTE(LOWORD(m_product.info.versionMS)) << 16) | 
		(LOBYTE(HIWORD(m_product.info.versionLS)) << 8) | 
		(LOBYTE(LOWORD(m_product.info.versionLS)));
#endif
	return S_OK;
}

int Application::SaveVersion()
{
	LONG result = 0;
	if (m_product.version.reg != m_product.version.cur)
	{
		CRegKey key;
		key.Attach(GetRegKey());
		LONG result = key.SetDWORDValue(TEXT("current_version"), m_product.version.cur);
		key.Detach();
	}
	return result;
}

int Application::LoadProductInfo()
{
	int result = -1;	
	DWORD dwHandle;
	TSTRING_STD(name);
	if (::GetModuleFileName(NULL, name, SIZEOF_ARRAY(name)))
	{
		DWORD dwSize = ::GetFileVersionInfoSize(name, &dwHandle);
		if (dwSize)
		{
			my::Buffer buffer;
			if (S_OK == buffer.Allocate(dwSize))
			{				
				if (TRUE == ::GetFileVersionInfo(name, dwHandle, buffer.GetSize(), buffer.GetData()))
				{
					LPCVOID pBlock = buffer.GetDataIt();
					UINT len;
					VS_FIXEDFILEINFO * pFixedInfo;
					if (TRUE == ::VerQueryValue(pBlock, TEXT("\\"), (LPVOID*)&pFixedInfo, &len))
					{
						m_product.info.versionMS = pFixedInfo->dwProductVersionMS;
						m_product.info.versionLS = pFixedInfo->dwProductVersionLS;
					}
					struct LANGANDCODEPAGE {
						WORD wLanguage;
						WORD wCodePage;
					} *lpTranslate;
					UINT cbTranslate;
					if (TRUE == ::VerQueryValue(pBlock, TEXT("\\VarFileInfo\\Translation"), (LPVOID*)&lpTranslate, &cbTranslate))
					{
						TSTRING_STD(SubBlock);
						LPVOID lpBuffer;
						
						_stprintf_s(SubBlock, SIZEOF_ARRAY(SubBlock), 
							TEXT("\\StringFileInfo\\%04x%04x\\ProductVersion"),
							lpTranslate->wLanguage,
							lpTranslate->wCodePage);

						if (TRUE == ::VerQueryValue(pBlock, SubBlock, &lpBuffer, &len))
							m_product.info.name = (LPTSTR)lpBuffer;

					}
					result = 0;
				}
			}
		}
		SYSTEMTIME & time = m_product.info.time;
		memset(&time, 0, sizeof(time));
		my::lib::File file;
		if (S_OK == file.Open(name))
			file.GetTime(time);
	}
	return result;
}

DWORD Application::GetCurrentVersion() const 
{
	return m_product.version.cur; 
}

DWORD Application::GetVersionReg() const 
{
	return m_product.version.reg; 
}

LPCTSTR Application::GetProductName() const
{
	return m_product.info.name; 
}

int Application::GetProductBuild() const
{
	return LOWORD(m_product.info.versionLS);
}

int Application::GetProductTime(SYSTEMTIME & time) const
{
	time = m_product.info.time;
	return 0;
}

CRegKey & Application::GetRegKey()
{
	LONG result;
	if (m_reg.key.m_hKey == NULL)
		result = m_reg.key.Create(HKEY_CURRENT_USER, m_reg.path);
	return m_reg.key;
}

HICON Application::LoadIcon(LPCTSTR name)
{
	return ::LoadIcon(theApp.GetModuleInstance(), name);
}

HICON Application::LoadIcon(UINT id)
{
	return LoadIcon(MAKEINTRESOURCE(id));
}

LPCTSTR Application::GetAppName() const 
{
	return m_name; 
}

LPCTSTR Application::GetAppPath() const 
{
	return m_path; 
}

int Application::ProcessCmdLine(LPTSTR lpstrCmdLine)
{
	int nArgs;
	LPTSTR * szArglist = ::CommandLineToArgvW(lpstrCmdLine, &nArgs);
	if (szArglist != NULL)
	{
		// Полное название приложения:
		m_path.Format(TEXT("%s"), szArglist[0]);
		// Короткое название:
		TSTRING_STD(strAppName);
		SAFE_TSTRCPY(strAppName, m_path);

		int pos = m_path.ReverseFind(TEXT('\\'));
		if (pos >= 0)
			m_path.Delete(pos, m_path.GetLength() - pos);

		LPCTSTR pStr = StrRChr(strAppName, NULL, TEXT('\\'));
		if (pStr != NULL)
		{
			LPTSTR pEnd = StrRChr(++pStr, NULL, TEXT('.'));
			if (pEnd != NULL)
				*pEnd = TEXT('\0');
			SAFE_TSTRCPY(strAppName, pStr);
		}
		m_name.Format(TEXT("%s"), strAppName);
#if defined _DEBUG && 0
		CString text;
		text.Format(TEXT("szArglist[0]: %s\r\nAppName: %s\r\nAppPath: %s"), szArglist[0], GetAppName(), GetAppPath());
		my::MessageBox(NULL, text);
#endif
		::LocalFree(szArglist);
	}
	return S_OK;
}

int Application::SetRegistryKey(LPCTSTR company, LPCTSTR application)
{
	LONG result = ERROR_SUCCESS;
	m_reg.path.Format(TEXT("Software\\%s\\%s"), company, application);

	result = m_reg.key.Create(HKEY_CURRENT_USER, m_reg.path);

	return result;
}

LONG Application::DeleteKey(LPCTSTR path, LPCTSTR name, BOOL recursive)
{
	LONG result;
	HKEY hKey;
	CString regPath;
	LPCTSTR appName = theApp.GetAppName();
	if (path != NULL)
		regPath.Format(TEXT("Software\\")COMPANY_NAME TEXT("\\%s\\%s"), appName, path);
	else
		regPath.Format(TEXT("Software\\")COMPANY_NAME TEXT("\\%s"), appName);
	result = ::RegOpenKeyEx(HKEY_CURRENT_USER, regPath, 0, KEY_ALL_ACCESS, &hKey);
	if (result == ERROR_SUCCESS)
	{
		CRegKey key(hKey);
		if (recursive)
			result = key.RecurseDeleteKey(name);
		else
			result = key.DeleteValue(name);
	}
	return result;
}

int Application::CreateSubkey(CRegKey & subKey, LPCTSTR path)
{
	LONG result;
	result = subKey.Create(GetRegKey(), path);
	return result;
}

int Application::OpenSubkey(CRegKey & subKey, LPCTSTR path)
{
	LONG result;
	result = subKey.Open(GetRegKey(), path);
	return result;
}


//
// CMyQuikApp
//

CMyQuikApp::CMyQuikApp()
{
	m_settingsAreModified = FALSE;
	m_settingsFlags = 0;
	m_shortcuts.current.val32 = m_shortcuts.previos.val32 = 0;
	// Place all significant initialization in InitInstance
}

HWND CMyQuikApp::GetMainWnd()
{
	return g_hMainWnd;
}

void CMyQuikApp::SetMainWnd(HWND hWnd)
{
	g_hMainWnd = hWnd;
}

// CMyQuikApp initialization

BOOL CMyQuikApp::InitInstance(LPTSTR lpstrCmdLine)
{
	::GetSystemTime(&m_stime);

	// InitCommonControlsEx() is required on Windows XP if an application
	// manifest specifies use of ComCtl32.dll version 6 or later to enable
	// visual styles.  Otherwise, any window creation will fail.
	INITCOMMONCONTROLSEX InitCtrls;
	InitCtrls.dwSize = sizeof(InitCtrls);
	// Set this to include all the common control classes you want to use
	// in your application.
	InitCtrls.dwICC = ICC_WIN95_CLASSES;//|ICC_STANDARD_CLASSES|ICC_USEREX_CLASSES;
	InitCommonControlsEx(&InitCtrls);

#if USE_GDIPLUS
	// Инициализируем GDI+:
	Gdiplus::GdiplusStartupInput gdiSI;
	Gdiplus::GdiplusStartupOutput gdiSO;
	//ULONG_PTR gdiHookToken;
	Gdiplus::GdiplusStartup(&m_gdiToken,&gdiSI,&gdiSO);
#endif

	::InitializeCriticalSection(&m_criticalSection);

	ProcessCmdLine(lpstrCmdLine);
	SetRegistryKey(COMPANY_NAME, GetAppName());

	LoadProductInfo();

	LoadVersion();
	LoadSettings();

	// Загружаем список свойств инструментов:
	LoadTableProperties(&m_listOfTableProperties);

	// Создаем список изображений:
	UINT ids[] = {
		IDI_SENT, IDI_CANCEL, IDI_BUY, IDI_SELL, IDI_ACTIVE_BUY, IDI_ACTIVE_SELL, IDI_CANCEL_BUY, IDI_CANCEL_SELL, 
		IDI_STOP_BUY, IDI_STOP_SELL, IDI_ACTIVE_STOP_BUY, IDI_ACTIVE_STOP_SELL, 
		IDI_CANCEL_STOP, IDI_CANCEL_STOP_BUY, IDI_CANCEL_STOP_SELL,
		IDI_BUY_GRAY, IDI_SELL_GRAY, IDI_STOP_BUY_GRAY, IDI_STOP_SELL_GRAY,
	};
	m_iconList.Create(16, 16, ILC_COLOR24|ILC_MASK, I_ICON_LAST-1, 0);
	for (int i = 0; i < SIZEOF_ARRAY(ids); i++)
	{
		UINT id = ids[i];
		HICON hIcon = theApp.LoadIcon(id);
		m_iconList.AddIcon(hIcon);
	}

	EnablelHooks();

	return TRUE;
}

int CMyQuikApp::ExitInstance()
{
	DisablelHooks();

#if USE_GDIPLUS
	Gdiplus::GdiplusShutdown(m_gdiToken);
#endif

	SaveVersion();
	SaveSettings();

	::DeleteCriticalSection(&m_criticalSection);

	return S_OK;
}

void CMyQuikApp::EnterCriticalSection()
{
	::EnterCriticalSection(&m_criticalSection); 
}

void CMyQuikApp::LeaveCriticalSection()
{
	::LeaveCriticalSection(&m_criticalSection); 
}

int CMyQuikApp::UpdateListOfTableProperties(LPCTSTR name, const QuoteTable::Properties * pProperties, int flags)
{
	return m_listOfTableProperties.UpdateProperties(name, pProperties, flags);
}

void CMyQuikApp::SaveTableProperties()
{
	::SaveTableProperties(&m_listOfTableProperties);
}

const QuoteTable::Properties * CMyQuikApp::FindTableProperty(LPCTSTR name, int flags)
{
	return m_listOfTableProperties.GetItem(name, flags);
}

const QuoteTable::Properties * CMyQuikApp::FindTableProperty(LPCTSTR name, LPCTSTR seccode, LPCTSTR classcode, LPCTSTR clientcode, LPCTSTR account)
{
	return m_listOfTableProperties.GetItem(name, seccode, classcode, clientcode, account);
}

const QuoteTable::Properties * CMyQuikApp::FindTableProperty(const Transaction * pTa)
{
	return m_listOfTableProperties.GetItem(
		pTa->strName[0] ? pTa->strName : NULL, 
		pTa->strSecCode[0] ? pTa->strSecCode : NULL, 
		pTa->strClassCode[0] ? pTa->strClassCode : NULL, 
		pTa->strClientCode[0] ? pTa->strClientCode : NULL, 
		pTa->strAccount[0] ? pTa->strAccount : NULL);
}

const QuoteTable::Properties * CMyQuikApp::GetCurrentTableProperty()
{
#if 0
	return m_listOfTableProperties.GetCurrentItem();
#else
	const QuoteTable * pTable = GetCurrentTable();
	if (pTable != NULL)
		return pTable->GetProperties();
	else
		return NULL;
#endif
}

QuoteTable * CMyQuikApp::FindTable(const Transaction * pTa)
{
	return m_tables.FindTable(
		pTa->strName[0] ? pTa->strName : NULL, 
		pTa->strSecCode[0] ? pTa->strSecCode : NULL, 
		pTa->strClassCode[0] ? pTa->strClassCode : NULL, 
		pTa->strClientCode[0] ? pTa->strClientCode : NULL, 
		pTa->strAccount[0] ? pTa->strAccount : NULL);
}

int CMyQuikApp::DoTransaction(Transaction * pTransaction, const QuoteTable::Properties * pProperties)
{
	int status = S_OK;
	status = ::SendMessage(GetMainWnd(), UM_TRANSACTION, (WPARAM)pTransaction, (LPARAM)pProperties);
	return status;
}

void CMyQuikApp::LogMessage(LPCTSTR msg, int code)
{
	::SendMessage(GetMainWnd(), UM_LOG, (WPARAM)msg, (LPARAM)code);
}

int CMyQuikApp::CreateClient(HWND hWnd)
{
	return m_client.Create(hWnd);
}

int CMyQuikApp::CloseClient()
{
	return m_client.Close();
}

int CMyQuikApp::ClearCacheServer()
{
	return m_client.ClearCacheServer();
}

int CMyQuikApp::GetServerStatus() const
{
	return m_client.GetServerStatus();
}

void CMyQuikApp::SetCurrentToolName(LPCTSTR name)
{
	if (name != NULL)
		SAFE_TSTRCPY(theApp.m_settings.trading.instrument.name, name);
	else
		memset(theApp.m_settings.trading.instrument.name, 0, sizeof(theApp.m_settings.trading.instrument.name));
}

LPCTSTR CMyQuikApp::GetCurrentToolName() const
{
	return theApp.m_settings.trading.instrument.name;
}

BOOL CMyQuikApp::IsSameToolName(LPCTSTR name) const
{
	return (0 == lstrcmp(name, GetCurrentToolName()));
}

BOOL CMyQuikApp::IsAnotherToolName(LPCTSTR name) const
{
	return (! IsSameToolName(name));
}

int CMyQuikApp::GetCurrentTrade(Trade & trade, double price) 
{ 
	int result = m_trader.GetCurrentTrade(trade); 
	if (result == S_OK)
		UpdateTrade(trade, price);
	return result;
}

int CMyQuikApp::GetTrade(LPCTSTR name, Trade & trade, double price, double * pLastProfit) 
{ 
	trade.price2 = price;
	int result = m_trader.GetTrade(name, trade); 
	if (result == S_OK)
		UpdateTrade(trade, price);
	return result;
}

int CMyQuikApp::GetTrade(LPCTSTR name, Trade & trade)
{ 
	int result = m_trader.GetTrade(name, trade); 
	return result;
}

void CMyQuikApp::UpdateTrade(Trade & trade, double price)
{
#if defined _DEBUG && 0
	trade.operation = Transaction::OPERATION_BUY;
	trade.quantity = 5;
	trade.price = 123456789;
#endif
	if (trade.quantity > 0)
	{
		double k = trade.price / trade.price2;
		double volume = k * price * trade.quantity;		
		if (trade.operation == Transaction::OPERATION_BUY)
			trade.profit = volume - trade.volume;
		else
			trade.profit = trade.volume - volume;
		trade.valid = TRUE;

		trade.last.profit = trade.profit;
		trade.last.valid = trade.valid;
	}
#if 0
	trade.valid = FALSE;
#endif
#if defined _DEBUG && 0
	trade.last.valid = TRUE;
#endif
}

int CMyQuikApp::RemoveInstrument(LPCTSTR name)
{
	int status;
	RemoveTableProperties(name);
	status = m_listOfTableProperties.Remove(name);

	HWND hWnd = theApp.GetMainWnd();
	::SendMessage(hWnd, UM_UPDATE_INSTRUMENTS, (WPARAM)-1, (LPARAM)name);

	return status;
}

int CMyQuikApp::RemoveInstrumentsAll()
{
	int status;

	ListOfTableProperties::iterator it;
	for (it = m_listOfTableProperties.begin(); it != m_listOfTableProperties.end(); it++)
	{
		RemoveTableProperties((*it).name);
	} // for (it)
	status = m_listOfTableProperties.Clear();

	HWND hWnd = theApp.GetMainWnd();
	::SendMessage(hWnd, UM_UPDATE_INSTRUMENTS, (WPARAM)-1, (LPARAM)NULL);

	return status;
}

LPCSTR strsProperties[] = {
	"name",
	"account",
	"clientcode",
	"classcode",
	"seccode",
	"price_nrdigits",
	"price_step",
	"trade_quantity",
	"trade_spread",
	"stop_value_relative",
	"stop_value_absolute",
	"stop_is_relative",
	"stop_slippage",
	"profit_value_relative",
	"profit_value_absolute",
	"profit_is_relative",
	"profit_slippage",
};

int CMyQuikApp::DoExportInstruments(LPCTSTR fileName)
{
	int status;
	my::lib::File file;

	status = file.New(fileName);
	if (status == S_OK)
	{
		ListOfTableProperties::const_iterator it;
		for (it = m_listOfTableProperties.begin(); it != m_listOfTableProperties.end(); it++)
		{
			const QuoteTable::Properties & properties = *(it);
			CHAR str[MAX_PATH_EX];
			int len;
			TSTRING3(tstr, SIZEOF_ARRAY(str), size);			
			USES_CONVERSION;

			len = sprintf_s(str, size, "[%s]\r\n", T2A(properties.name));

			file.Write(str, len);
			len = sprintf_s(str, size, "seccode=%s\r\n", T2A(properties.seccode));
			file.Write(str, len);
			len = sprintf_s(str, size, "classcode=%s\r\n", T2A(properties.classcode));
			file.Write(str, len);
			len = sprintf_s(str, size, "clientcode=%s\r\n", T2A(properties.clientcode));
			file.Write(str, len);
			len = sprintf_s(str, size, "account=%s\r\n", T2A(properties.account));
			file.Write(str, len);

			len = sprintf_s(str, size, "price_nrdigits=%d\r\n", properties.price.nd);
			file.Write(str, len);			
			FormatString(tstr, size, properties.price.step, properties.price.nd);
			len = sprintf_s(str, size, "price_step=%s\r\n", T2A(tstr));
			file.Write(str, len);
			len = sprintf_s(str, size, "trade_quantity=%.0f\r\n", properties.trading.quantity);
			file.Write(str, len);			
			FormatString(tstr, size, properties.trading.spread, properties.price.nd);
			len = sprintf_s(str, size, "trade_spread=%s\r\n", T2A(tstr));
			file.Write(str, len);

			const QuoteTable::Properties::Trading::Autos::Item & item1 = properties.trading.autos.stop;
			FormatString(tstr, size, item1.relative, 2);
			len = sprintf_s(str, size, "stop_value_relative=%s\r\n", T2A(tstr));
			file.Write(str, len);
			FormatString(tstr, size, item1.absolute, properties.price.nd);
			len = sprintf_s(str, size, "stop_value_absolute=%s\r\n", T2A(tstr));
			file.Write(str, len);
			len = sprintf_s(str, size, "stop_is_relative=%d\r\n", item1.isRelative);
			file.Write(str, len);			
			FormatString(tstr, size, item1.slippage, properties.price.nd);
			len = sprintf_s(str, size, "stop_slippage=%s\r\n", T2A(tstr));
			file.Write(str, len);

			const QuoteTable::Properties::Trading::Autos::Item & item2 = properties.trading.autos.profit;
			FormatString(tstr, size, item2.relative, 2);
			len = sprintf_s(str, size, "profit_value_relative=%s\r\n", T2A(tstr));
			file.Write(str, len);
			FormatString(tstr, size, item2.absolute, properties.price.nd);
			len = sprintf_s(str, size, "profit_value_absolute=%s\r\n", T2A(tstr));
			file.Write(str, len);
			len = sprintf_s(str, size, "profit_is_relative=%d\r\n", item2.isRelative);
			file.Write(str, len);			
			FormatString(tstr, size, item2.slippage, properties.price.nd);
			len = sprintf_s(str, size, "profit_slippage=%s\r\n", T2A(tstr));
			file.Write(str, len);

			len = sprintf_s(str, size, "\r\n");
			file.Write(str, len);
		} // for (it)		
	}

	return status;
}

int CMyQuikApp::DoImportInstruments(LPCTSTR fileName)
{
	int status;
	my::lib::File file;
	status = file.Open(fileName);
	if (status == S_OK)
	{
		ListOfTableProperties * pList = &m_listOfTableProperties;
		pList->Clear();

		QuoteTable::Properties properties;
		BOOL creatingItem = FALSE;
		char data[1000];
		char buf[100];
		char str[MAX_PATH_EX];
		int it = 0;
		char ch, prev;
		ch = prev = 0;
		USES_CONVERSION;
		for (;;)
		{
			int ready = 0;
			for (;;)
			{				
				UINT nrBytes = file.Read(buf, 1);
				if (nrBytes > 0)
				{
					prev = ch;
					ch = buf[0];
					if (ch == '\n')
					{
						if (prev == '\r')
						{
							it--;
						}
						ready = 1;
						break;
					}
					else
					{
						if (it < sizeof(data) - 1)
							data[it++] = ch;
						else
							break;
					}
				}
				else
					break;
			} // for (;;)
			if (it > 0)
			{// Обработка строк:
				int i, n = 0;
				data[it] = 0;
				char * pBegin = data;
				char * pStr;
				char * pEnd;
				pStr = pBegin;
				pEnd = my::str::SkipGaps(pStr, it - (pStr - pBegin));
				if (pEnd != NULL)
				{
					char * end;
					pStr = pEnd;
					if (*pStr == '[')
					{
						++pStr;
						if (creatingItem)
						{
							pList->Add(&properties);
						}
						else
						{
							creatingItem = TRUE;
						}
						memset(&properties, 0, sizeof(properties));
						pEnd = strchr(pStr, ']');
						if (pEnd != NULL)
						{// Название инструмента:
							n = (int)(pEnd - pStr);
							memcpy(str, pStr, n);
							str[n] = 0;
							SAFE_TSTRCPY(properties.name, A2T(str));							
						}
						else
						{
							creatingItem = FALSE;
						}
					} // if (*pStr == '[')
					else
					{// Анализируем параметры инструмента:
						for (i = 0; i < SIZEOF_ARRAY(strsProperties); i++)
						{
							LPCSTR strProp = strsProperties[i];
							n = strlen(strProp);
							if (0 == memcmp(strProp, pStr, n))
							{
								pStr += n;
								pEnd = my::str::SkipGaps(pStr, it - (pStr - pBegin));
								if (pEnd != NULL)
								{
									pStr = pEnd;
									if (*pStr == '=')
									{
										pStr += 1;
										pEnd = my::str::SkipGaps(pStr, it - (pStr - pBegin));
										if (pEnd != NULL)
										{
											pStr = pEnd;
											n = it - static_cast<int>((pStr - pBegin));
											switch (i)
											{
											case I_PROPERTY_ACCOUNT:
												SAFE_TSTRCPY(properties.account, A2T(pStr));
												break;
											case I_PROPERTY_CLIENTCODE:
												SAFE_TSTRCPY(properties.clientcode, A2T(pStr));
												break;
											case I_PROPERTY_CLASSCODE:
												SAFE_TSTRCPY(properties.classcode, A2T(pStr));
												break;
											case I_PROPERTY_SECCODE:
												SAFE_TSTRCPY(properties.seccode, A2T(pStr));
												break;
											case I_PROPERTY_PRICE_NRDIGITS:
												properties.price.nd = strtol(pStr, &end, 10); pStr = end;
												break;
											case I_PROPERTY_PRICE_STEP:
												properties.price.step = strtod(pStr, &end); pStr = end;
												break;
											case I_PROPERTY_TRADE_QUANTITY:
												properties.trading.quantity = strtod(pStr, &end); pStr = end;
												break;
											case I_PROPERTY_TRADE_SPRED:
												properties.trading.spread = strtod(pStr, &end); pStr = end;
												break;
											case I_PROPERTY_STOP_VALUE_RELATIVE:
												properties.trading.autos.stop.relative = strtod(pStr, &end); pStr = end;
												break;
											case I_PROPERTY_STOP_VALUE_ABSOLUTE:
												properties.trading.autos.stop.absolute = strtod(pStr, &end); pStr = end;
												break;
											case I_PROPERTY_STOP_IS_RELATIVE:
												properties.trading.autos.stop.isRelative = strtol(pStr, &end, 10); pStr = end;
												break;
											case I_PROPERTY_PROFIT_VALUE_RELATIVE:
												properties.trading.autos.profit.relative = strtod(pStr, &end); pStr = end;
												break;
											case I_PROPERTY_PROFIT_VALUE_ABSOLUTE:
												properties.trading.autos.profit.absolute = strtod(pStr, &end); pStr = end;
												break;
											case I_PROPERTY_PROFIT_IS_RELATIVE:
												properties.trading.autos.profit.isRelative = strtol(pStr, &end, 10); pStr = end;
												break;
											} // switch (i)
										}
									}
								}
								break;
							}
						} // for (i)
					}
				}
				it = 0;
				ready = 0;
			} // if (it > 0)
			else
				if (! ready)
					break;
		} // for (;;)
		if (creatingItem)
			pList->Add(&properties);
	}
	return status;
}

Action::Type CMyQuikApp::CheckActionAndExecute(int vkey, UINT msg, LPARAM lParam, HWND hWnd)
{
	int flags = 0x0;
	int previos = 0;
	if (msg == WM_KEYUP)
		flags = Action::F_UP;
	else if (msg == WM_KEYDOWN)
		previos = (DWORD(lParam) >> 30) & 0x1;

	Action::Type action = CheckActionByKey(vkey, flags, GetCurrentLayout(), msg, lParam);
	if (action != ACTION_TYPE_UNKNOWN)
	{
		if (previos == 0) // Игнорируем удерживание клавиш!
		{
			HWND hMainWnd = theApp.GetMainWnd();
			Action objAction(action, flags);
			int result = ::SendMessage(hMainWnd, UM_ACTION, (WPARAM)&objAction, (LPARAM)hWnd);
			if (result == S_OK)
			{
#if defined _DEBUG && 1
				USES_CONVERSION;
				CString msg;
				msg.Format(TEXT("Action: %s%s\r\n"), GetActionName(action), ACTION_FLAG_END(flags) ? TEXT(" - end") : TEXT(""));
				TRACE(T2A(msg));
#endif
			}
			else
				action = ACTION_TYPE_UNKNOWN;
		} // if (previos == 0)
	}
	return action;
}

LRESULT CALLBACK CBTProc(int nCode, WPARAM wParam, LPARAM lParam) 
{
	HHOOK hHook = theApp.m_hooks[CMyQuikApp::I_HOOK_CBT].hHook;

	if (nCode < 0)  // do not process message 
		return ::CallNextHookEx(hHook, nCode, wParam, lParam); 

	BOOL translated = FALSE;
	if (nCode == HCBT_SYSCOMMAND)
	{
		ResetShortcutsLL();
	}

	return translated ? 1 : CallNextHookEx(hHook, nCode, wParam, lParam); 
}

LRESULT CALLBACK KeyboardProc(int nCode, WPARAM wParam, LPARAM lParam) 
{
	HHOOK hHook = theApp.m_hooks[CMyQuikApp::I_HOOK_KEYBOARD].hHook;

	if (nCode < 0)  // do not process message 
		return ::CallNextHookEx(hHook, nCode, wParam, lParam); 

	MSG msg;
	msg.message = (lParam & 0x80000000) ? WM_KEYUP : WM_KEYDOWN;
	msg.wParam = wParam;
	msg.lParam = lParam;
	BOOL translated = theApp.PreTranslateMessage(&msg);
#if defined _DEBUG && 0
	if (translated)
		TRACE("KeyboardProc: translated\r\n");
#endif
	return translated ? 1 : CallNextHookEx(hHook, nCode, wParam, lParam); 
}

static HWND hWndPrev = NULL;

LRESULT CALLBACK MouseProc(int nCode, WPARAM wParam, LPARAM lParam) 
{
	HHOOK hHook = theApp.m_hooks[CMyQuikApp::I_HOOK_MOUSE].hHook;

	if (nCode < 0)  // do not process message 
		return ::CallNextHookEx(hHook, nCode, wParam, lParam); 

	MSG msg;
	msg.message = (UINT)wParam;
	msg.wParam = wParam;
	msg.lParam = lParam;
	BOOL translated = theApp.PreTranslateMessage(&msg);

	return translated ? 1 : CallNextHookEx(hHook, nCode, wParam, lParam); 
}

int CMyQuikApp::EnablelHooks(BOOL global)
{
	int status = S_OK;
#if 1
	m_shortcuts.current.val32 = m_shortcuts.previos.val32 = 0;
#endif
	Hook * pHook;
	int threadId = ::GetCurrentThreadId();
	if (global)
		threadId = 0;
#if 1
	pHook = &m_hooks[I_HOOK_CBT];
	pHook->type = WH_CBT;
	pHook->proc = CBTProc;
	pHook->hHook = ::SetWindowsHookEx(pHook->type, pHook->proc, (HINSTANCE) NULL, threadId); 
#endif
	pHook = &m_hooks[I_HOOK_KEYBOARD];
	pHook->type = WH_KEYBOARD;
	pHook->proc = KeyboardProc;
	pHook->hHook = ::SetWindowsHookEx(pHook->type, pHook->proc, (HINSTANCE) NULL, threadId); 
	pHook = &m_hooks[I_HOOK_MOUSE];
	pHook->type = WH_MOUSE;
	pHook->proc = MouseProc;
	pHook->hHook = ::SetWindowsHookEx(pHook->type, pHook->proc, (HINSTANCE) NULL, threadId); 
	return status;
}

int CMyQuikApp::DisablelHooks(BOOL cbt, BOOL keyboard, BOOL mouse)
{
	int status = S_OK;
	BOOL disable[I_HOOK_LAST] = {cbt, keyboard, mouse};
	for (int i = I_HOOK_KEYBOARD; i <= I_HOOK_MOUSE; i++)
	{
		HHOOK & hHook = m_hooks[i].hHook;
		if (hHook && disable[i])
		{
			BOOL succes = ::UnhookWindowsHookEx(hHook);
			hHook = NULL;
		}
	}
#if 1
	m_shortcuts.current.val32 = m_shortcuts.previos.val32 = 0;
#endif
	return status;
}

BOOL CMyQuikApp::PreTranslateMessage(MSG* pMsg)
{	
	BOOL translated = FALSE;
	UINT msg = pMsg->message;
	LPARAM lParam = pMsg->lParam;
	int vkey = 0; 
	BOOL checkAction = FALSE;

	if (msg == WM_KEYDOWN || msg == WM_KEYUP)
	{		
		vkey = static_cast<int>(pMsg->wParam);
#if defined _DEBUG && 0
		WORD repeat = LOWORD(pMsg->lParam);
		TRACE("WM_KEYDOWN: vkey=%d, repeat=%d, prev=%d\r\n", vkey, repeat, prev);
#endif		
		if (vkey == VK_APPS && msg == WM_KEYUP)
		{// Вызов всплывающего меню при нажатии специальной клавиши:
			::SendMessage(::GetActiveWindow(), UM_MENU, (WPARAM)F_SHOW_MENU, 0);
		}
		else 
			checkAction = TRUE;
	}	
#if 1
	if ((msg >= WM_MOUSEFIRST && msg <= WM_MOUSELAST) && 
		(msg != WM_MOUSEMOVE && msg != WM_MOUSEHWHEEL) &&
		(msg != WM_LBUTTONDBLCLK && msg != WM_RBUTTONDBLCLK && msg != WM_MBUTTONDBLCLK && msg != WM_XBUTTONDBLCLK))
#else
	else if (msg == WM_LBUTTONDOWN || msg == WM_RBUTTONDOWN || msg == WM_MBUTTONDOWN || msg == WM_XBUTTONDOWN)// || msg == WM_MOUSEHWHEEL)
#endif
	{
		checkAction = TRUE;
	}
	if (checkAction)
	{
		HWND hWnd;
#if 0
		hWnd = GetActiveWindow();
#else
		CPoint point;
		::GetCursorPos(&point);
		hWnd = ::WindowFromPoint(point);
#endif
		Action::Type action = this->CheckActionAndExecute(vkey, msg, lParam, hWnd);
		if (action != ACTION_TYPE_UNKNOWN)
		{
			DWORD shortcut = this->GetShortcutKey(action);
#if 0
			if (! (shortcut == MAKESHORTCUT(0, 0, SCK_LBUTTON) || shortcut == MAKESHORTCUT(0, 0, SCK_RBUTTON) || 
				shortcut == MAKESHORTCUT(0, 0, SCK_MBUTTON)))
#else
			if (! (shortcut & MAKESHORTCUT(0, 0, SCK_LBUTTON|SCK_RBUTTON|SCK_MBUTTON|SCK_XBUTTON1|SCK_XBUTTON2)))
#endif
				translated = TRUE;
		}
	}
	return translated;
}

static LPCTSTR strSettings = TEXT("Settings");
static LPCTSTR strQuikAutoCheckConnection = TEXT("quik_auto_check_connection");
static LPCTSTR strQuikPath = TEXT("path_quik");
static LPCTSTR strTrans2QuikPath = TEXT("path_trans2quik");
static LPCTSTR strHistoryPath = TEXT("path_history");
static LPCTSTR strConfirmTransaction = TEXT("confirm_transaction");
static LPCTSTR strConfirmCancel = TEXT("confirm_cancel");
static LPCTSTR strUseSpread = TEXT("use_spread");
static LPCTSTR strAutoPrice = TEXT("auto_price");
static LPCTSTR strAutoStopOrder = TEXT("auto_stoporder");
static LPCTSTR strBestIsCurrent = TEXT("best_is_current");
static LPCTSTR strStopRelativeValue = TEXT("stop_relative_value");
static LPCTSTR strStopAbsoluteValue = TEXT("stop_value_absolute");
static LPCTSTR strStopIsRelative = TEXT("stop_is_relative");
static LPCTSTR strPrintTransaction = TEXT("print_transaction");
static LPCTSTR strSaveOnDisk = TEXT("save_on_disk");
static LPCTSTR strName = TEXT("name");
static LPCTSTR strCurrentInstrumentName = TEXT("current_instrument");
static LPCTSTR strKeepHistory = TEXT("keep_history");
static LPCTSTR strHistoryKeepCancel = TEXT("history_keep_cancel");
static LPCTSTR strHistoryModify = TEXT("history_enable_modify");
static LPCTSTR strHistoryConfirmRemove = TEXT("history_confirm_remove");

static LPCTSTR names[] = {
	TEXT("foreground"),
	TEXT("price_current"), TEXT("price_min"), TEXT("price_max"), 
	TEXT("percent"),
	TEXT("indicator_price"), TEXT("indicator_demand_supply"),
	TEXT("trade_basic"), TEXT("trade_profit"), TEXT("trade_last"), 
	TEXT("account_limit"), TEXT("account_profit"),
	TEXT("info_transaction"),
	TEXT("info_instrument_name"), TEXT("info_instrument_sec"),
	TEXT("active_frame"),
};

static void SetParams(Settings::Presentation::GlassItem * pItem, 
	BOOL textEnable, COLORREF textColor, DWORD textEffects, DWORD textVal1, 
	BOOL backgrndEnable, COLORREF backgrndColor, DWORD backgrndOpacity = 100, DWORD backgrndVal1 = 0)
{
	pItem->text.enable = textEnable;
	pItem->text.cf.crTextColor = textColor;
	pItem->text.cf.dwEffects = textEffects;
	pItem->text.val1 = textVal1;
	pItem->backgrnd.enable = backgrndEnable;
	pItem->backgrnd.color = backgrndColor;
	pItem->backgrnd.opacity = backgrndOpacity;
	pItem->backgrnd.val1 = backgrndVal1;
}

int CMyQuikApp::LoadSettings()
{
	Settings & settings = m_settings;

	int status = S_OK;
	BOOL success;

	DEFINE_PATH_EX(path);
	TSTRING_SMALL2(name, sizeofName);
	int n;

	TRACE("Load settings\r\n");

	const DWORD verReg = theApp.GetVersionReg();

	// Сначала определяем домашний каталог:	
	memset(m_settings.common.path.home, 0, sizeof(m_settings.common.path.home));
#if 0
	success = ::SHGetSpecialFolderPath(NULL, path, CSIDL_APPDATA, FALSE);
	if (success)
	{
		::wsprintf(m_settings.common.path.home, TEXT("%s\\%s"), path, theApp.GetAppName());
		::CreateDirectory(m_settings.common.path.home, NULL);
	}
#else
	BOOL validHomePath = FALSE;
	HKEY hKey;
	_stprintf_s(path, SIZEOF_ARRAY(path), TEXT("Software\\%s\\%s\\Settings\\Common"), COMPANY_NAME, theApp.GetAppName());
	if (ERROR_SUCCESS == ::RegOpenKeyEx (HKEY_CURRENT_USER, path, 0, KEY_READ, &hKey))
	{
		DWORD dwType = REG_SZ;
		DWORD dwSize = SIZEOF_ARRAY(m_settings.common.path.home);
		if (ERROR_SUCCESS == ::RegQueryValueEx(hKey, TEXT("path_home"), NULL, &dwType, (LPBYTE)m_settings.common.path.home, &dwSize))
		{
			::CreateDirectory(m_settings.common.path.home, NULL);
			validHomePath = TRUE;
		}
		::RegCloseKey(hKey);
	}
	if (! validHomePath)
	{
		SAFE_TSTRCPY(m_settings.common.path.home, theApp.GetAppPath());
		validHomePath = TRUE;
	}
#endif
	wsprintf(path, strSettings);

	CString str;

	CRegKey key;
	CRegKey & root = GetRegKey();

	wsprintf(path, TEXT("%s\\Common"), strSettings);
	if (ERROR_SUCCESS == key.Open(root, path))
	{
		if (verReg < VERSION_0_4_0)
		{// Копируем старые пользовательские настройки:		
			MergeSettings(key, TEXT("quik_path"), strQuikPath);
			MergeSettings(key, TEXT("trans2quik_path"), strTrans2QuikPath);
			MergeSettings(key, TEXT("history_path"), strHistoryPath);
			SetModifySettings();		
		} // if (verReg < VERSION_0_4_0)
	}
	LPCTSTR defaultQuikPath = TEXT("C:\\Quik5\\info.exe");
	QueryStringValue (key, str, strQuikPath, defaultQuikPath);
	SAFE_TSTRCPY(settings.common.path.quik, str);
	QueryIntValue (key, settings.common.quik.autoCheckConnection, strQuikAutoCheckConnection, TRUE);
#if 1
	wsprintf(name, TEXT("%s"), TRANS2QUIK_DLL_NAME);
#else
	wsprintf(name, TEXT("%s\\%s"), theApp.GetAppPath(), TRANS2QUIK_DLL_NAME);
#endif
	QueryStringValue(key, str, strTrans2QuikPath, name);
	SAFE_TSTRCPY(settings.common.path.trans2quik, str);
	wsprintf(name, TEXT("%s\\%s"), settings.common.path.home, TEXT("history"));
	QueryStringValue(key, str, strHistoryPath, name);
	SAFE_TSTRCPY(settings.common.path.history, str);
#if 1
	success = ::CreateDirectory(settings.common.path.history, NULL);
#endif
	_stprintf_s(name, SIZEOF_ARRAY(name), TEXT("%s\\%s.log"), settings.common.path.home, theApp.GetAppName());
	QueryStringValue(key, str, TEXT("path_log"), name);
	SAFE_TSTRCPY(settings.common.path.log, str);

	key.Close();

	wsprintf(path, TEXT("%s\\Server"), strSettings);
	SAFE_TSTRCPY(settings.server.name, MYQUIKDDESERVER);

	wsprintf(path, TEXT("%s\\Trading"), strSettings);
	key.Open(root, path);
	QueryIntValue(key, settings.trading.confirmTransaction, strConfirmTransaction, TRUE);
	QueryIntValue (key, settings.trading.confirmCancel, strConfirmCancel, TRUE);
	QueryIntValue (key, settings.trading.autoPrice, strAutoPrice, FALSE);
	QueryIntValue (key, settings.trading.useSpread, strUseSpread, FALSE);
#if 1
	QueryIntValue (key, settings.trading.bestIsCurrent, strBestIsCurrent, FALSE);
#else
	settings.trading.bestIsCurrent = FALSE;
#endif
	QueryIntValue (key, settings.trading.autoStop, strAutoStopOrder, FALSE);
	QueryIntValue (key, settings.trading.flags, TEXT("flags"), F_TRADING_PRINT_MESSAGE_ON_TRANSACTION_ERROR);
	QueryStringValue (key, str, strCurrentInstrumentName, TEXT(""));
	SAFE_TSTRCPY(settings.trading.instrument.name, str);
	QueryIntValue (key, settings.trading.history.keep, strKeepHistory, TRUE);
	QueryIntValue (key, settings.trading.history.keepCancel, strHistoryKeepCancel, TRUE);
	QueryIntValue (key, settings.trading.history.modify, strHistoryModify, TRUE);
	QueryIntValue (key, settings.trading.history.confirmRemove, strHistoryConfirmRemove, TRUE);
	key.Close();

	wsprintf(path, TEXT("%s\\Log"), strSettings);
	key.Open(root, path);	
	QueryIntValue (key, settings.log.printTransaction, strPrintTransaction, FALSE);
	QueryIntValue (key, settings.log.saveOnDisk, strSaveOnDisk, TRUE);
	key.Close();

	wsprintf(path, TEXT("%s\\Shortcuts"), strSettings);

	my::ShortcutMap & layout0 = settings.shortcuts.layouts[I_LAYOUT_DEFAULT];
	my::ShortcutMap & layout1 = settings.shortcuts.layouts[I_LAYOUT_CUSTOM];
	InitActionsDefault(layout0); InitActionsDefault(layout1);

	key.Open(root, path, KEY_READ|KEY_WRITE|KEY_QUERY_VALUE);

	if (verReg < VERSION_0_4_0)
	{
		// Удаляем этот раздел:
		DeleteKey(NULL, TEXT("Settings\\Keyboard"), TRUE);
		SetModifySettings();		
	} // if (verReg < VERSION_0_4_0)

	// Поиск комбинации по названию:
	TSTRING_SMALL(valueName);	
	for (int i = 0; ; i++)
	{
		DWORD cbName = SIZEOF_ARRAY(valueName);
		DWORD type;
		DWORD val32;
		DWORD size = sizeof(val32);
		if (ERROR_SUCCESS == ::RegEnumValue(key, i, valueName, &cbName, NULL, &type, (LPBYTE)&val32, &size)) 
		{
			LPCTSTR actionName;
			for (int iAction = 0; (actionName = GetActionName(iAction)); iAction++)
			{
				if (0 == lstrcmp(valueName, actionName))
				{
					layout1[iAction] = val32;
					break;
				}
			} // for (iAction)
		}
		else
			break;
	} // for (i)

	QueryIntValue(key, settings.shortcuts.iCurrentLayout, TEXT("shortcuts_layout"), 0);
	key.Close();

	LPCTSTR defaultFontName = TEXT("Tahoma");
	int defaultFontSize = 9;

	{// Загружаем параметры предстваления окон:
		n = wsprintf(path, TEXT("%s\\Presentation\\View"), strSettings);
		key.Open(root, path);

		BOOL showDeals, showGlass, showHistory, showStop, showLog;
		showDeals = showGlass = TRUE;
		showHistory = showStop = showLog = FALSE;

		if (verReg < VERSION_0_4_0)
		{
			LPCTSTR path = TEXT("Workspace");
			LPCTSTR valName;
			valName = TEXT("show_quotes");
			if (ERROR_SUCCESS == QueryIntValue(root, showGlass, path, valName, showGlass))
				DeleteKey(path, valName);
			valName = TEXT("show_history");
			if (ERROR_SUCCESS == QueryIntValue(root, showHistory, path, valName, showHistory))
				DeleteKey(path, valName);
			valName = TEXT("show_log");
			if (ERROR_SUCCESS == QueryIntValue(root, showLog, path, valName, showLog))
				DeleteKey(path, valName);
			valName = TEXT("show_stop");
			if (ERROR_SUCCESS == QueryIntValue(root, showStop, path, valName, showStop))
				DeleteKey(path, valName);
		} // if (verReg < VERSION_0_4_0)

		for (int i = 0; i < SIZEOF_ARRAY(settings.presentation.view.item); i++)
			settings.presentation.view.item[i].flags = F_SHOW_TITLE;

		if (showDeals)
			settings.presentation.view.item[I_WINDOW_DEALS].flags |= F_SHOW_WINDOW;
		if (showGlass)
			settings.presentation.view.item[I_WINDOW_GLASS].flags |= F_SHOW_WINDOW;
		if (showHistory)
			settings.presentation.view.item[I_WINDOW_HISTORY].flags |= F_SHOW_WINDOW;
		if (showLog)
			settings.presentation.view.item[I_WINDOW_LOG].flags |= F_SHOW_WINDOW;
		if (showStop)
			settings.presentation.view.item[I_WINDOW_STOP].flags |= F_SHOW_WINDOW;

		for (int i = 0; i < SIZEOF_ARRAY(settings.presentation.view.item); i++)
		{
			wsprintf(name, TEXT("wnd%d_flags"), i);
			Settings::Presentation::Window & window = settings.presentation.view.item[i];
			QueryIntValue (key, window.flags, name, window.flags);
			wsprintf(name, TEXT("wnd%d_opacity_active"), i);
			QueryIntValue (key, window.opacity.active, name, 100);
			wsprintf(name, TEXT("wnd%d_opacity_inactive"), i);
			QueryIntValue (key, window.opacity.inactive, name, 90);
		} // for (i)
	}

	{// Загружаем параметры отображения главного окна:
		n = wsprintf(path, TEXT("%s\\Presentation\\Main"), strSettings);
		key.Open(root, path);
		QueryIntValue (key, settings.presentation.main.flags, TEXT("flags"), F_VIEW_ENABLE_FLASHING);
		QueryIntValue (key, settings.presentation.main.show.wnd, TEXT("show_wnd"), 
			F_MAIN_SHOW_LIST|F_MAIN_SHOW_OSD|F_MAIN_SHOW_CONTROLS);
		QueryIntValue (key, settings.presentation.main.show.frame, TEXT("show_frame"), 
			F_MAIN_SHOW_LIST|F_MAIN_SHOW_OSD|F_MAIN_SHOW_CONTROLS);
		QueryIntValue (key, settings.presentation.main.margins, TEXT("margins"), 
			::GetSystemMetrics(SM_CXFRAME) - ::GetSystemMetrics(SM_CXEDGE));
		key.Close();
	}
	{// Загружаем параметры OSD:
		n = wsprintf(path, TEXT("%s\\Presentation\\OSD"), strSettings);
		key.Open(root, path);
		TxtFormat * pFormats[] = {
			&settings.presentation.osd.foreground,
			&settings.presentation.osd.prices.current, &settings.presentation.osd.prices.minimum, &settings.presentation.osd.prices.maximum,
			&settings.presentation.osd.percent,
			&settings.presentation.osd.indicators.price, &settings.presentation.osd.indicators.demandSupply,
			&settings.presentation.osd.trade.basic, &settings.presentation.osd.trade.profit, &settings.presentation.osd.trade.last,
			&settings.presentation.osd.account.limit, &settings.presentation.osd.account.profit,
			&settings.presentation.osd.transaction,
			&settings.presentation.osd.instrument, &settings.presentation.osd.seccode,
			&settings.presentation.osd.frame,
		};
		int fontSizes[] = {
			8,
			12, 8, 8, 
			10,
			8, 8,
			10, 12, 10,
			8, 10,
			8,
			12, 10,
			8,
		};
		DWORD colors[] = {
			RGB(0, 0, 0),
			RGB(0, 0, 0), RGB(0, 0, 0), RGB(0, 0, 0),
			RGB(0, 0, 0),
			RGB(0, 0, 0), RGB(0, 0, 0),
			RGB(0, 0, 0), RGB(0, 0, 0), RGB(0, 0, 0),
			RGB(0, 0, 0), RGB(0, 0, 0),
			RGB(0, 0, 0),
			RGB(0, 0, 0), RGB(0, 0, 0),
			::GetSysColor(COLOR_HIGHLIGHT), //RGB(0x20, 0x20, 0x20), // COLOR_ACTIVEBORDER,
		};		
		DWORD color;
		
		LPCTSTR fontName = defaultFontName;
		BOOL useForegroundFont;
		QueryIntValue (key, useForegroundFont, TEXT("use_foreground_font"), FALSE);
		settings.presentation.osd.useForegroundFont = useForegroundFont;
		for (int i = 0; i < SIZEOF_ARRAY(pFormats); i++)
		{
			TxtFormat * pTxtFmt = pFormats[i];
			BOOL isIndicatorPrice = (pTxtFmt == &settings.presentation.osd.indicators.price);
			BOOL isIndicatorDemandSupply = (pTxtFmt == &settings.presentation.osd.indicators.demandSupply);
			BOOL isIndicator = isIndicatorPrice || isIndicatorDemandSupply;
			BOOL groupDigits = 
#if 0
				!(isIndicator || 
				pTxtFmt == &settings.presentation.osd.frame ||
				pTxtFmt == &settings.presentation.osd.transaction ||
				pTxtFmt == &settings.presentation.osd.instrument || pTxtFmt == &settings.presentation.osd.seccode);
#else
				FALSE;
#endif
			// Название:
			wsprintf(name, TEXT("%s_font_name"), names[i]);
			QueryStringValue (key, str, name, defaultFontName);
			fontName = str;
			if (i > 0)
			{
				TxtFormat * pFmt = pFormats[0];
				if (useForegroundFont)
					fontName = pFmt->cf.szFaceName;
			}			
			SAFE_TSTRCPY(pTxtFmt->cf.szFaceName, fontName);
			wsprintf(name, TEXT("%s_font_size"), names[i]);
			QueryIntValue(key, pTxtFmt->cf.yHeight, name, fontSizes[i]);
			 pTxtFmt->cf.yHeight *= 20;				 
			// Цвет:
			wsprintf(name, TEXT("%s_font_color"), names[i]);
			QueryIntValue (key, pTxtFmt->cf.crTextColor, name, colors[i]);
			// Эффекты:
			wsprintf(name, TEXT("%s_font_effects"), names[i]);
			QueryIntValue (key, pTxtFmt->cf.dwEffects, name, 0); // (i == 0) ? CFE_BOLD : 0
			// Смещение:
			wsprintf(name, TEXT("%s_offset_cx"), names[i]);
			QueryStringValue (key, str, name, TEXT("0"));
			pTxtFmt->offset.cx = StrToInt(str);
			wsprintf(name, TEXT("%s_offset_cy"), names[i]);
			QueryStringValue (key, str, name, TEXT("0"));
			pTxtFmt->offset.cy = StrToInt(str);
			// Положение:
			wsprintf(name, TEXT("%s_vertical"), names[i]);
			QueryIntValue (key, pTxtFmt->placement.vertical, name, isIndicatorPrice ? 1 : 0);

			wsprintf(name, TEXT("%s_group_digits"), names[i]);
			QueryIntValue (key, pTxtFmt->groupDigits, name, groupDigits);
		}
		wsprintf(name, TEXT("background_color"));
#if 0
		color = ::GetSysColor(COLOR_BTNFACE); // RGB(220, 220, 220);  
#else
		color = ::GetSysColor(COLOR_ACTIVECAPTION);
		color = my::lib::MakeColorLighterEx(color, 255, 240, 0); // my::lib::MakeColorLighter(color, 80);//
#endif
		QueryIntValue (key, settings.presentation.osd.backgrnd.color, name, color);
		key.Close();
	}

	{// Загружаем параметры "стакана":
		n = wsprintf(path, TEXT("%s\\Presentation\\Glass"), strSettings);
		key.Open(root, path);

		Settings::Presentation::Glass & glass = settings.presentation.glass;

		Settings::Presentation::GlassItem defaultGlassItems[SIZEOF_ARRAY(glass.items)];
		Settings::GlassItem * pDefaultItem;

		// Формируем значения по умолчанию:
		memset(&defaultGlassItems, 0, sizeof(defaultGlassItems));

		pDefaultItem = &defaultGlassItems[GLASS_ITEM_COMMON];
		SetParams(pDefaultItem, TRUE, DEFAULT_COLOR_TEXT, 0, FALSE, TRUE, DEFAULT_COLOR_FILL);

		pDefaultItem = &defaultGlassItems[GLASS_ITEM_NEUTRAL_ZONE];
		SetParams(pDefaultItem, FALSE, DEFAULT_COLOR_TEXT, 0, FALSE, TRUE, ::GetSysColor(COLOR_BTNFACE), 70, TRUE);
		
		defaultGlassItems[GLASS_ITEM_NEUTRAL_PRICE] = *pDefaultItem;
		pDefaultItem = &defaultGlassItems[GLASS_ITEM_NEUTRAL_PRICE];
		pDefaultItem->backgrnd.enable = FALSE;

		defaultGlassItems[GLASS_ITEM_NEUTRAL_VOLUME] = *pDefaultItem;
		defaultGlassItems[GLASS_ITEM_NEUTRAL_USER] = *pDefaultItem;

		pDefaultItem = &defaultGlassItems[GLASS_ITEM_BUY];
		SetParams(pDefaultItem, TRUE, DEFAULT_COLOR_BUY_TEXT, 0, 0, TRUE, DEFAULT_COLOR_BUY, 100, TRUE);
		pDefaultItem->u.alignment.value = 
			Settings::Presentation::Glass::MakeColumnAlignment(ALIGNMENT_RIGHT, ALIGNMENT_RIGHT, ALIGNMENT_RIGHT);
		pDefaultItem->u.alignment.enable = TRUE;

		pDefaultItem = &defaultGlassItems[GLASS_ITEM_BUY_PRICE];
		SetParams(pDefaultItem, FALSE, DEFAULT_COLOR_BUY_TEXT, 0, 0, FALSE, DEFAULT_COLOR_BUY);

		defaultGlassItems[GLASS_ITEM_BUY_VOLUME] = *pDefaultItem;
		defaultGlassItems[GLASS_ITEM_BUY_USER] = *pDefaultItem;

		pDefaultItem = &defaultGlassItems[GLASS_ITEM_SELL];
		SetParams(pDefaultItem, TRUE, DEFAULT_COLOR_SELL_TEXT, 0, 0, TRUE, DEFAULT_COLOR_SELL, 100, TRUE);
		pDefaultItem->u.alignment.value = 
			Settings::Presentation::Glass::MakeColumnAlignment(ALIGNMENT_RIGHT, ALIGNMENT_RIGHT, ALIGNMENT_RIGHT);
		pDefaultItem->u.alignment.enable = TRUE;

		pDefaultItem = &defaultGlassItems[GLASS_ITEM_SELL_PRICE];
		SetParams(pDefaultItem, FALSE, DEFAULT_COLOR_SELL_TEXT, 0, 0, FALSE, DEFAULT_COLOR_SELL);

		defaultGlassItems[GLASS_ITEM_SELL_VOLUME] = *pDefaultItem;
		defaultGlassItems[GLASS_ITEM_SELL_USER] = *pDefaultItem;

		// Индикация объёма:
		pDefaultItem = &defaultGlassItems[GLASS_ITEM_BUY_VOLUME];
		pDefaultItem->other.enable = TRUE; // включить индикацию объёма;
		pDefaultItem->other.val1 = RGB(128, 128, 255);
		pDefaultItem->u.values.value1 = ALIGNMENT_LEFT;
		pDefaultItem->u.values.value2 = Settings::Presentation::Glass::MakeMargins(0, 0, 0, 0);

		defaultGlassItems[GLASS_ITEM_SELL_VOLUME] = defaultGlassItems[GLASS_ITEM_BUY_VOLUME];

		// Свои заявки и сделки:
		pDefaultItem = &defaultGlassItems[GLASS_ITEM_USER_BIDS_BUY];
		SetParams(pDefaultItem, TRUE, DEFAULT_COLOR_SELL_TEXT, 0, FALSE, TRUE, RGB(0, 255, 128), 15, TRUE);

		pDefaultItem = &defaultGlassItems[GLASS_ITEM_USER_STOPS_BUY];
		SetParams(pDefaultItem, TRUE, RGB(0, 64, 0), 0, FALSE, TRUE, RGB(0, 128, 64), 15, TRUE);

		pDefaultItem = &defaultGlassItems[GLASS_ITEM_USER_DEALS_BUY];
		SetParams(pDefaultItem, TRUE, DEFAULT_COLOR_SELL_TEXT, CFE_BOLD, FALSE, TRUE, RGB(255, 255, 255), 30, TRUE);

		pDefaultItem = &defaultGlassItems[GLASS_ITEM_USER_BIDS_SELL];
		SetParams(pDefaultItem, TRUE, DEFAULT_COLOR_BUY_TEXT, 0, FALSE, TRUE, RGB(128, 0, 128), 15, TRUE);

		pDefaultItem = &defaultGlassItems[GLASS_ITEM_USER_STOPS_SELL];
		SetParams(pDefaultItem, TRUE, RGB(64, 0, 0), 0, FALSE, TRUE, RGB(64, 0, 64), 15, TRUE);

		pDefaultItem = &defaultGlassItems[GLASS_ITEM_USER_DEALS_SELL];
		SetParams(pDefaultItem, TRUE, DEFAULT_COLOR_BUY_TEXT, CFE_BOLD, FALSE, TRUE, RGB(255, 255, 255), 30, TRUE);

		// Различные цены:
		pDefaultItem = &defaultGlassItems[GLASS_ITEM_PRICE_MIN];
		SetParams(pDefaultItem, TRUE, RGB(255, 255, 255), 0, FALSE, TRUE, RGB(0, 128, 255));

		pDefaultItem = &defaultGlassItems[GLASS_ITEM_PRICE_MAX];
		SetParams(pDefaultItem, TRUE, RGB(255, 255, 255), 0, FALSE, TRUE, RGB(255, 77, 74));

		pDefaultItem = &defaultGlassItems[GLASS_ITEM_PRICE_BID];
		SetParams(pDefaultItem, FALSE, DEFAULT_COLOR_TEXT, 0, FALSE, TRUE, RGB(0, 255, 255), 20);

		pDefaultItem = &defaultGlassItems[GLASS_ITEM_PRICE_OFFER];
		SetParams(pDefaultItem, FALSE, DEFAULT_COLOR_TEXT, 0, FALSE, TRUE, RGB(255, 255, 0), 20);

		pDefaultItem = &defaultGlassItems[GLASS_ITEM_PRICE_LAST];
		SetParams(pDefaultItem, TRUE, DEFAULT_COLOR_TEXT, CFE_BOLD, FALSE, TRUE, RGB(255, 255, 255), 50, TRUE);

		for (int i = 0; i < SIZEOF_ARRAY(glass.items); i++)
		{
#if 0
			if (i == GLASS_ITEM_USER)
				continue;
#endif
			Settings::GlassItem & item = glass.items[i];
			const Settings::GlassItem & defaultItem = defaultGlassItems[i];
#if 1
			item.i = i;
#endif
			wsprintf(name, TEXT("_%02d_font_name"), i); QueryStringValue (key, str, name, defaultFontName);
			SAFE_TSTRCPY(item.text.cf.szFaceName, str);
			wsprintf(name, TEXT("_%02d_font_size"), i); QueryIntValue(key, item.text.cf.yHeight, name, defaultFontSize);
			item.text.cf.yHeight *= 20;
			wsprintf(name, TEXT("_%02d_font_color"), i); QueryIntValue (key, item.text.cf.crTextColor, name, defaultItem.text.cf.crTextColor);
			wsprintf(name, TEXT("_%02d_font_effects"), i); QueryIntValue (key, item.text.cf.dwEffects, name, defaultItem.text.cf.dwEffects);

			wsprintf(name, TEXT("_%02d_text_enable"), i); QueryIntValue (key, item.text.enable, name, defaultItem.text.enable);
			wsprintf(name, TEXT("_%02d_text_val1"), i); QueryIntValue (key, item.text.val1, name, defaultItem.text.val1);

			wsprintf(name, TEXT("_%02d_backgrnd_enable"), i); QueryIntValue (key, item.backgrnd.enable, name, defaultItem.backgrnd.enable);
			wsprintf(name, TEXT("_%02d_backgrnd_color"), i); QueryIntValue (key, item.backgrnd.color, name, defaultItem.backgrnd.color);
			wsprintf(name, TEXT("_%02d_backgrnd_opacity"), i); QueryIntValue (key, item.backgrnd.opacity, name, defaultItem.backgrnd.opacity);
			wsprintf(name, TEXT("_%02d_backgrnd_val1"), i); QueryIntValue (key, item.backgrnd.val1, name, defaultItem.backgrnd.val1);

			wsprintf(name, TEXT("_%02d_other_enable"), i); QueryIntValue (key, item.other.enable, name, defaultItem.other.enable);
			wsprintf(name, TEXT("_%02d_other_val1"), i); QueryIntValue (key, item.other.val1, name, defaultItem.other.val1);

			wsprintf(name, TEXT("_%02d_value1"), i); QueryIntValue (key, item.u.values.value1, name, defaultItem.u.values.value1);
			wsprintf(name, TEXT("_%02d_value2"), i); QueryIntValue (key, item.u.values.value2, name, defaultItem.u.values.value2);
		}

		QueryIntValue (key, glass.view.style, TEXT("view_style"), GLASS_STYLE_1);
		QueryIntValue (key, glass.view.flipHorizontal, TEXT("view_flip_horizontal"), 0);
		QueryIntValue (key, glass.view.flags, TEXT("view_flags"), F_GLASS_VIEW_RAREFIED|F_GLASS_VIEW_NEUTRAL_ZONE);

		QueryIntValue (key, glass.user.flags, TEXT("user_flags"), 
			F_USER_SHOW_ACTIVE_BIDS|F_USER_SHOW_ACTIVE_STOPS|F_USER_SHOW_ACTIVE_DEALS|F_USER_SHOW_ICONS|F_USER_SHOW_USER_COLUMN
#if 0
			|F_USER_GROUP_BY_PRICE
#endif
			);
		if (verReg < VERSION_0_4_0)
		{
			LPCTSTR valName;

			BOOL buyUp, flipHorizontal, rarefied, neutralZone;
			buyUp = flipHorizontal = FALSE; rarefied = neutralZone = TRUE;

			valName = TEXT("style");
			if (ERROR_SUCCESS == QueryIntValue(root, glass.view.style, path, valName, glass.view.style))
				DeleteKey(path, valName);
			valName = TEXT("buy_up");
			if (ERROR_SUCCESS == QueryIntValue(root, buyUp, path, valName, buyUp))
			{
				if (! buyUp)
					glass.user.flags &= ~F_GLASS_VIEW_BUY_UP;
				DeleteKey(path, valName);
			}
			valName = TEXT("flip_horizontal");
			if (ERROR_SUCCESS == QueryIntValue(root, glass.view.flipHorizontal, path, valName, glass.view.flipHorizontal))
				DeleteKey(path, valName);
			valName = TEXT("rarefied");
			if (ERROR_SUCCESS == QueryIntValue(root, rarefied, path, valName, rarefied))
			{
				if (! buyUp)
					glass.user.flags &= ~F_GLASS_VIEW_RAREFIED;
				DeleteKey(path, valName);
			}

			BOOL showActiveBids, showActiveStops, showActiveDeals, showUserColumn;
			showActiveBids = showActiveStops = showActiveDeals = showUserColumn = TRUE;

			valName = TEXT("show_active_bids");
			if (ERROR_SUCCESS == QueryIntValue(root, showActiveBids, path, valName, showActiveBids))
			{
				if (! showActiveBids)
					glass.user.flags &= ~F_USER_SHOW_ACTIVE_BIDS;
				DeleteKey(path, valName);
			}
			valName = TEXT("show_active_stops");
			if (ERROR_SUCCESS == QueryIntValue(root, showActiveStops, path, valName, showActiveStops))
			{
				if (! showActiveStops)
					glass.user.flags &= ~F_USER_SHOW_ACTIVE_STOPS;
				DeleteKey(path, valName);
			}
			valName = TEXT("show_active_deals");
			if (ERROR_SUCCESS == QueryIntValue(root, showActiveDeals, path, valName, showActiveDeals))
			{
				if (! showActiveDeals)
					glass.user.flags &= ~F_USER_SHOW_ACTIVE_DEALS;
				DeleteKey(path, valName);
			}
			valName = TEXT("show_active_column");
			if (ERROR_SUCCESS == QueryIntValue(root, showUserColumn, path, valName, showUserColumn))
			{
				if (! showUserColumn)
					glass.user.flags &= ~F_USER_SHOW_USER_COLUMN;
				DeleteKey(path, valName);
			}
		} // if (verReg < VERSION_0_4_0)


		QueryIntValue (key, glass.backgrnd.color, TEXT("backgrnd_color"), ::GetSysColor(COLOR_APPWORKSPACE));
		QueryIntValue (key, glass.columns.width, TEXT("columns_width"), Settings::Presentation::Glass::MakeColumnWidth(40, 30, 30));
		QueryIntValue (key, glass.grid.color, TEXT("grid_color"), DEFAULT_COLOR_GRID);
		QueryIntValue (key, glass.grid.width, TEXT("grid_width"), 1);
		QueryIntValue (key, glass.grid.lines, TEXT("grid_lines"), GRID_LINES_NOTHING);
		QueryIntValue (key, glass.grid.show, TEXT("grid_show"), TRUE);
		QueryIntValue (key, glass.margins.value, TEXT("margins_value"), Settings::Presentation::Glass::MakeMargins(1, 1, 2, 2));
		QueryIntValue (key, glass.margins.enable, TEXT("margins_enable"), TRUE);
		QueryIntValue (key, glass.border.color, TEXT("border_color"), DEFAULT_COLOR_BORDER);
		QueryIntValue (key, glass.border.width, TEXT("border_width"), 2);
		QueryIntValue (key, glass.border.lines, TEXT("border_lines"), BORDER_LINES_ALL);
		QueryIntValue (key, glass.border.show, TEXT("border_show"), TRUE);
		QueryIntValue (key, glass.marker.color, TEXT("marker_color"), DEFAULT_COLOR_MARKER);
		QueryIntValue (key, glass.marker.width, TEXT("marker_width"), 2);
		QueryIntValue (key, glass.marker.show, TEXT("marker_show"), TRUE);
		QueryIntValue (key, glass.marker.follow, TEXT("marker_follow"), MARKER_FOLLOW_PRICE);

		if (verReg < VERSION_0_4_0)
		{// Копируем старые пользовательские настройки:
			DWORD val32;
			MergeSettings(key, TEXT("background_color"), glass.backgrnd.color);
			MergeSettings(key, TEXT("buy_color"), glass.items[GLASS_ITEM_BUY].backgrnd.color);
			MergeSettings(key, TEXT("sell_color"), glass.items[GLASS_ITEM_SELL].backgrnd.color);
			MergeSettings(key, TEXT("grid_color"), glass.grid.color);
			MergeSettingsGridLines(key, glass.grid.lines);
			MergeSettings(key, TEXT("grid_spacing"), val32);
			MergeSettings(key, TEXT("selection_color"), glass.marker.color);
			MergeSettings(key, TEXT("selection_width"), glass.marker.width);
			SetModifySettings();		
		} // if (verReg < VERSION_0_4_0)

		key.Close();
	}

	{// Загружаем параметры списка сделок:
		n = wsprintf(path, TEXT("%s\\Presentation\\List"), strSettings);
		key.Open(root, path);

		if (verReg < VERSION_0_4_0)
			key.DeleteValue(TEXT("columns_order"));

		QueryStringValue (key, str, TEXT("columns_data"), 
			// Формат: index:visible:alignment
			TEXT("0:1:0 1:0:2 2:1:2 3:1:2 4:0:2 5:1:2 6:0:2 7:1:2 8:0:2 9:1:2 10:1:2 11:0:2 12:1:2 13:1:2 14:1:0 15:0:2 16:0:2")); 
		USES_CONVERSION;
		char * strOrder = T2A(str);
		char * pBegin = strOrder;
		char * pStr = pBegin;
		for (int i = 0; i < SIZEOF_ARRAY(settings.presentation.list.columns.item); i++)
		{
			Settings::Presentation::ListOfDeals::Column & item = settings.presentation.list.columns.item[i];
			item.index = -1;
			item.visible = 0;
			item.format = ALIGNMENT_CENTER;
		} // for (i)
		int len = str.GetLength();
		for (int i = 0; i < SIZEOF_ARRAY(settings.presentation.list.columns.item); i++)
		{
			char * pEnd = my::str::SkipGaps(pStr, len - (pStr - pBegin));
			if (pEnd != NULL)
			{
				char * end;
				int index = strtol(pStr, &end, 10); pStr = ++end;
				int visible = strtol(pStr, &end, 10); pStr = ++end;
				int format = strtol(pStr, &end, 10); pStr = end;
				Settings::Presentation::ListOfDeals::Column & item = settings.presentation.list.columns.item[i];
				item.index = (char)index;
				item.visible = (char)visible;
				item.format = (char)format;
			}
			else
				break;
		} // for (i)

		// Цвета:
		for (int i = 0; i < SIZEOF_ARRAY(settings.presentation.list.colors.item); i++)
		{
			Settings::Presentation::ListOfDeals::Color & color = settings.presentation.list.colors.item[i];
			MakeColorDefault(color, i);
			wsprintf(name, TEXT("colors_%02d_text"), i); 
			QueryIntValue (key, color.text, name, color.text);
			wsprintf(name, TEXT("colors_%02d_background"), i); 
			QueryIntValue (key, color.backgrnd, name, color.backgrnd);
		} // for (i)
		QueryIntValue (key, settings.presentation.list.colors.useDifferentColors, TEXT("colors_use_different_colors"), TRUE);
		QueryIntValue (key, settings.presentation.list.colors.useDifferentColorsText, TEXT("colors_use_different_colors_text"), TRUE);

		key.Close();
	}

	if (verReg < VERSION_0_4_0)
	{
		DeleteKey(TEXT("Workspace"), TEXT("DlgQuotes"), TRUE);
	}

	return status;
}

void CMyQuikApp::MergeSettings(CRegKey & key, LPCTSTR from, LPCTSTR to)
{
	TSTRING_STD(buf);
	ULONG n = SIZEOF_ARRAY(buf);
	if (key)
	{
		if (ERROR_SUCCESS == key.QueryStringValue(from, buf, &n))
		{
			key.SetStringValue(to, buf);
			key.DeleteValue(from);
		}
	}
}

void CMyQuikApp::MergeSettings(CRegKey & key, LPCTSTR name, DWORD & dwValue)
{
	DWORD val32;
	if (key)
	{
		if (ERROR_SUCCESS == key.QueryDWORDValue (name, val32))
		{		
			dwValue = val32;
			key.DeleteValue (name);
		}
	}
}

void CMyQuikApp::MergeSettingsGridLines(CRegKey & key, DWORD & dwValue)
{
	if (! key)
		return;
	LPCTSTR name1 = TEXT("grid_outline_cols");
	LPCTSTR name2 = TEXT("grid_outline_rows");
	DWORD val32;
	DWORD dw = 0;
	BOOL update = FALSE;
	if (ERROR_SUCCESS == key.QueryDWORDValue (name1, val32))
	{		
		update = TRUE;
		if (val32)
			dw |= GRID_LINES_VERTICAL;
		key.DeleteValue(name1);
	}
	if (ERROR_SUCCESS == key.QueryDWORDValue (name2, val32))
	{		
		update = TRUE;
		if (val32)
			dw |= GRID_LINES_HORIZONTAL;
		key.DeleteValue(name2);
	}
	if (update)
		dwValue = dw;
}

int CMyQuikApp::SaveSettings()
{
	int status = S_OK;

	if (! m_settingsAreModified)
		return status;

	Settings & settings = m_settings;
	DEFINE_PATH_EX(path);
	TSTRING_SMALL(name);
	int n;

	TRACE("Save settings\r\n");

	CRegKey key;

	wsprintf(path, TEXT("%s\\Common"), strSettings);
	if (ERROR_SUCCESS == CreateSubkey(key, path))
	{		
		key.SetStringValue(strQuikPath, settings.common.path.quik);
		key.SetDWORDValue(strQuikAutoCheckConnection, settings.common.quik.autoCheckConnection);
		key.SetStringValue(strTrans2QuikPath, settings.common.path.trans2quik);
		key.SetStringValue(strHistoryPath, settings.common.path.history);
		key.SetStringValue(TEXT("path_log"), settings.common.path.log);
		key.Close();
	}
	wsprintf(path, TEXT("%s\\Server"), strSettings);
	if (ERROR_SUCCESS ==  CreateSubkey(key, path))
	{
		key.SetStringValue(strName, settings.server.name);
		key.Close();
	}
	wsprintf(path, TEXT("%s\\Trading"), strSettings);
	if (ERROR_SUCCESS ==  CreateSubkey(key, path))
	{
		key.SetDWORDValue(strConfirmTransaction, settings.trading.confirmTransaction);
		key.SetDWORDValue(strConfirmCancel, settings.trading.confirmCancel);
		key.SetDWORDValue(strAutoPrice, settings.trading.autoPrice);
		key.SetDWORDValue(strUseSpread, settings.trading.useSpread);
		key.SetDWORDValue(strBestIsCurrent, settings.trading.bestIsCurrent);
		key.SetDWORDValue(strAutoStopOrder, settings.trading.autoStop);
		key.SetDWORDValue(TEXT("flags"), settings.trading.flags);
		key.SetStringValue(strCurrentInstrumentName, settings.trading.instrument.name);
		key.SetDWORDValue(strKeepHistory, settings.trading.history.keep);
		key.SetDWORDValue(strHistoryKeepCancel, settings.trading.history.keepCancel);
		key.SetDWORDValue(strHistoryModify, settings.trading.history.modify);
		key.SetDWORDValue(strHistoryConfirmRemove, settings.trading.history.confirmRemove);
		key.Close();
	}
	wsprintf(path, TEXT("%s\\Log"), strSettings);
	if (ERROR_SUCCESS ==  CreateSubkey(key, path))
	{
		key.SetDWORDValue(strPrintTransaction, settings.log.printTransaction);
		key.SetDWORDValue(strSaveOnDisk, settings.log.saveOnDisk);
		key.Close();
	}
	wsprintf(path, TEXT("%s\\Shortcuts"), strSettings);
	if (ERROR_SUCCESS == CreateSubkey(key, path))
	{
		my::ShortcutMap & layout = settings.shortcuts.layouts[I_LAYOUT_CUSTOM];
		for (size_t i = 0; i < layout.size(); i++)
		{
			DWORD shortcut = layout[i];
#if 0		
			wsprintf(name, TEXT("action_%02d"), i);
			key.SetDWORDValue(name, shortcut);
#else			
			key.SetDWORDValue(GetActionName(i), shortcut);
#endif						
		} // for (i)
		key.SetDWORDValue(TEXT("shortcuts_layout"), settings.shortcuts.iCurrentLayout);
		key.Close();
	}

	{// Сохраняем параметры предстваления окон:
		n = wsprintf(path, TEXT("%s\\Presentation\\View"), strSettings);
		if (ERROR_SUCCESS ==  CreateSubkey(key, path))
		{
			for (int i = 0; i < SIZEOF_ARRAY(settings.presentation.view.item); i++)
			{
				Settings::Presentation::Window & window = settings.presentation.view.item[i];
				wsprintf(name, TEXT("wnd%d_flags"), i);
				key.SetDWORDValue(name, window.flags);
				wsprintf(name, TEXT("wnd%d_opacity_active"), i);
				key.SetDWORDValue(name, window.opacity.active);
				wsprintf(name, TEXT("wnd%d_opacity_inactive"), i);
				key.SetDWORDValue(name, window.opacity.inactive);
			} // for (i)
		}
	}

	{// Сохраняем параметры отображения главного окна:
		n = wsprintf(path, TEXT("%s\\Presentation\\Main"), strSettings);
		if (ERROR_SUCCESS ==  CreateSubkey(key, path))
		{
			key.SetDWORDValue(TEXT("flags"), settings.presentation.main.flags);
			key.SetDWORDValue(TEXT("show_wnd"), settings.presentation.main.show.wnd);
			key.SetDWORDValue(TEXT("show_frame"), settings.presentation.main.show.frame);
			key.SetDWORDValue(TEXT("margins"), settings.presentation.main.margins);
			key.Close();
		}
	}
	{// Сохраняем параметры OSD:
		n = wsprintf(path, TEXT("%s\\Presentation\\OSD"), strSettings);
		if (ERROR_SUCCESS ==  CreateSubkey(key, path))
		{
			TxtFormat * pFormats[] = {
				&settings.presentation.osd.foreground, 
				&settings.presentation.osd.prices.current, &settings.presentation.osd.prices.minimum, &settings.presentation.osd.prices.maximum,
				&settings.presentation.osd.percent,
				&settings.presentation.osd.indicators.price, &settings.presentation.osd.indicators.demandSupply,
				&settings.presentation.osd.trade.basic, &settings.presentation.osd.trade.profit, &settings.presentation.osd.trade.last,
				&settings.presentation.osd.account.limit, &settings.presentation.osd.account.profit,
				&settings.presentation.osd.transaction,
				&settings.presentation.osd.instrument, &settings.presentation.osd.seccode,
			};
			TSTRING_STD2(str, size);
			for (int i = 0; i < SIZEOF_ARRAY(pFormats); i++)
			{
				TxtFormat * pTxtFmt = pFormats[i];
				// Название:
				if (i == 0 || !settings.presentation.osd.useForegroundFont)
				{
					wsprintf(name, TEXT("%s_font_name"), names[i]);
					key.SetStringValue(name, pTxtFmt->cf.szFaceName);
				}
				wsprintf(name, TEXT("%s_font_size"), names[i]);
				key.SetDWORDValue(name, pTxtFmt->cf.yHeight / 20);
				// Цвет:
				wsprintf(name, TEXT("%s_font_color"), names[i]);
				key.SetDWORDValue(name, pTxtFmt->cf.crTextColor);
				// Эффекты:
				wsprintf(name, TEXT("%s_font_effects"), names[i]);
				key.SetDWORDValue(name, pTxtFmt->cf.dwEffects);
				// Смещение:				
				wsprintf(name, TEXT("%s_offset_cx"), names[i]);
				_stprintf_s(str, size, TEXT("%d"), pTxtFmt->offset.cx);
				key.SetStringValue(name, str);
				wsprintf(name, TEXT("%s_offset_cy"), names[i]);
				_stprintf_s(str, size, TEXT("%d"), pTxtFmt->offset.cy);
				key.SetStringValue(name, str);
				// Положение:
				wsprintf(name, TEXT("%s_vertical"), names[i]);
				key.SetDWORDValue(name, pTxtFmt->placement.vertical);

				wsprintf(name, TEXT("%s_group_digits"), names[i]);
				key.SetDWORDValue(name, pTxtFmt->groupDigits);
			}
			key.SetDWORDValue(TEXT("background_color"), settings.presentation.osd.backgrnd.color);
			key.SetDWORDValue(TEXT("use_foreground_font"), settings.presentation.osd.useForegroundFont);
			key.Close();
		}
	}

	{// Сохраняем параметры "стакана":
		n = wsprintf(path, TEXT("%s\\Presentation\\Glass"), strSettings);
		if (ERROR_SUCCESS ==  CreateSubkey(key, path))
		{
			Settings::Presentation::Glass & glass = settings.presentation.glass;

			for (int i = 0; i < SIZEOF_ARRAY(glass.items); i++)
			{
#if 0
				if (i == GLASS_ITEM_USER)
					continue;
#endif
				Settings::GlassItem & item = glass.items[i];

				wsprintf(name, TEXT("_%02d_font_name"), i); key.SetStringValue(name, item.text.cf.szFaceName);
				wsprintf(name, TEXT("_%02d_font_size"), i); key.SetDWORDValue(name, item.text.cf.yHeight / 20);
				wsprintf(name, TEXT("_%02d_font_color"), i); key.SetDWORDValue(name, item.text.cf.crTextColor);
				wsprintf(name, TEXT("_%02d_font_effects"), i); key.SetDWORDValue(name, item.text.cf.dwEffects);

				wsprintf(name, TEXT("_%02d_text_enable"), i); key.SetDWORDValue(name, item.text.enable);
				wsprintf(name, TEXT("_%02d_text_val1"), i); key.SetDWORDValue(name, item.text.val1);

				wsprintf(name, TEXT("_%02d_backgrnd_enable"), i); key.SetDWORDValue(name, item.backgrnd.enable);
				wsprintf(name, TEXT("_%02d_backgrnd_color"), i); key.SetDWORDValue(name, item.backgrnd.color);
				wsprintf(name, TEXT("_%02d_backgrnd_opacity"), i); key.SetDWORDValue(name, item.backgrnd.opacity);
				wsprintf(name, TEXT("_%02d_backgrnd_val1"), i); key.SetDWORDValue(name, item.backgrnd.val1);

				wsprintf(name, TEXT("_%02d_other_enable"), i); key.SetDWORDValue(name, item.other.enable);
				wsprintf(name, TEXT("_%02d_other_val1"), i); key.SetDWORDValue(name, item.other.val1);

				wsprintf(name, TEXT("_%02d_value1"), i); key.SetDWORDValue(name, item.u.values.value1);
				wsprintf(name, TEXT("_%02d_value2"), i); key.SetDWORDValue(name, item.u.values.value2);
			} // for (i)

			key.SetDWORDValue(TEXT("view_style"), glass.view.style);
			key.SetDWORDValue(TEXT("view_flip_horizontal"), glass.view.flipHorizontal);
			key.SetDWORDValue(TEXT("view_flags"), glass.view.flags);

			key.SetDWORDValue(TEXT("user_flags"), glass.user.flags);

			key.SetDWORDValue(TEXT("backgrnd_color"), glass.backgrnd.color);
			key.SetDWORDValue(TEXT("columns_width"), glass.columns.width);
			key.SetDWORDValue(TEXT("grid_color"), glass.grid.color);
			key.SetDWORDValue(TEXT("grid_width"), glass.grid.width);
			key.SetDWORDValue(TEXT("grid_lines"), glass.grid.lines);
			key.SetDWORDValue(TEXT("grid_show"), glass.grid.show);
			key.SetDWORDValue(TEXT("margins_value"), glass.margins.value);
			key.SetDWORDValue(TEXT("margins_enable"), glass.margins.enable);
			key.SetDWORDValue(TEXT("border_color"), glass.border.color);
			key.SetDWORDValue(TEXT("border_width"), glass.border.width);
			key.SetDWORDValue(TEXT("border_lines"), glass.border.lines);
			key.SetDWORDValue(TEXT("border_show"), glass.border.show);
			key.SetDWORDValue(TEXT("marker_color"), glass.marker.color);
			key.SetDWORDValue(TEXT("marker_width"), glass.marker.width);
			key.SetDWORDValue(TEXT("marker_show"), glass.marker.show);
			key.SetDWORDValue(TEXT("marker_follow"), glass.marker.follow);

			key.Close();
		}
	}

	{// Сохраняем параметры списка сделок:
		n = wsprintf(path, TEXT("%s\\Presentation\\List"), strSettings);
		if (ERROR_SUCCESS ==  CreateSubkey(key, path))
		{
			TSTRING_SMALL(strOrder);
			LPTSTR pStr = strOrder;
			for (int i = 0; i < SIZEOF_ARRAY(settings.presentation.list.columns.item); i++)
			{
				const Settings::Presentation::ListOfDeals::Column & item = settings.presentation.list.columns.item[i];
				int index = item.index;
				if (index >= 0)
				{
					int n = wsprintf(pStr, TEXT("%d:%d:%d "), index, item.visible, item.format);
					pStr += n;
				}
				else
				{
					break;
				}
			} // for (i)
			if (pStr > strOrder)
				--pStr;
			*pStr = TEXT('\0');
			key.SetStringValue(TEXT("columns_data"), strOrder);

			// Цвета:
			for (int i = 0; i < SIZEOF_ARRAY(settings.presentation.list.colors.item); i++)
			{
				Settings::Presentation::ListOfDeals::Color & color = settings.presentation.list.colors.item[i];
				wsprintf(name, TEXT("colors_%02d_text"), i);
				key.SetDWORDValue(name, color.text);
				wsprintf(name, TEXT("colors_%02d_background"), i);
				key.SetDWORDValue(name, color.backgrnd);
			} // for (i)
			key.SetDWORDValue(TEXT("colors_use_different_colors"), settings.presentation.list.colors.useDifferentColors);
			key.SetDWORDValue(TEXT("colors_use_different_colors"), settings.presentation.list.colors.useDifferentColorsText);
			key.Close();
		}
	}
	return status;
}

void CMyQuikApp::SetModifySettings(const Settings * pSettings, int modify, int flags) 
{ 
	if (pSettings != NULL)
		m_settings = *pSettings;
	m_settingsAreModified = modify; 
	m_settingsFlags = flags;
}

BOOL CMyQuikApp::GetModifySettings() const
{
	return m_settingsAreModified;
}

void CMyQuikApp::GetStartTime(SYSTEMTIME & stime)
{
	stime = m_stime;
}

HICON CMyQuikApp::LoadSmallIcon(UINT id)
{
	HINSTANCE hInst = theApp.GetModuleInstance();
	HICON hIcon = (HICON)::LoadImage(hInst, MAKEINTRESOURCE(id), IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);
	return hIcon;
}

HICON CMyQuikApp::LoadIconEx(HINSTANCE hInst, UINT id, int cx, int cy, int flags)
{
	if (cy == -1)
		cy = cx;
	HICON hIcon = (HICON)::LoadImage(hInst, MAKEINTRESOURCE(id), IMAGE_ICON, cx, cy, flags);
	return hIcon;
}

int CMyQuikApp::ClearWindowPosition(LPCTSTR name, int flags)
{
	DeleteKey(TEXT("Workspace"), name, TRUE);
	return S_OK;
}

void CMyQuikApp::FlashWindow(HWND hWnd, UINT count, DWORD timeout)
{
	my::wnd::Flash(hWnd, count, timeout);
}

DWORD MakeShortcutKey(int vkey, int & flags, UINT msg, LPARAM lParam)
{
	DWORD shortcut = INVALID_SHORTCUT;
	DWORD & curVal32 = theApp.m_shortcuts.current.val32;
	DWORD & preVal32 = theApp.m_shortcuts.previos.val32;

	if (vkey == VK_RETURN || vkey == VK_ESCAPE || vkey == VK_TAB || 	
		vkey == VK_SELECT || vkey == VK_EXECUTE || vkey == VK_HELP || 
		vkey == VK_SNAPSHOT || vkey == VK_PRINT || 
		vkey == VK_LWIN || vkey == VK_RWIN || vkey == VK_APPS ||
		vkey == VK_CLEAR || vkey == VK_SEPARATOR || 
		vkey == VK_CAPITAL || vkey == VK_NUMLOCK || vkey == VK_SCROLL)
	{
#if 0
		if (vkey == VK_ESCAPE)
			curVal32 = preVal32 = 0;
#endif
		goto end; // эти кнопки игнорируются;
	}

	int ctrls = 0;
	if (0x8000 & GetAsyncKeyState (VK_CONTROL))
		ctrls |= SCK_CONTROL;
	if (0x8000 & GetAsyncKeyState (VK_SHIFT))
		ctrls |= SCK_SHIFT;
	if (0x8000 & GetAsyncKeyState (VK_ALT))
		ctrls |= SCK_ALT;
#if 1
	int lbtn, rbtn, xbtn1, xbtn2;
	if (::GetSystemMetrics(SM_SWAPBUTTON)) {lbtn = VK_RBUTTON; rbtn = VK_LBUTTON; xbtn1 = VK_XBUTTON2; xbtn2 = VK_XBUTTON1;}
	else {lbtn = VK_LBUTTON; rbtn = VK_RBUTTON; xbtn1 = VK_XBUTTON1; xbtn2 = VK_XBUTTON2;}

	if (Action::F_CHECKMOUSEASYNC)
	{
		if (0x8000 & GetAsyncKeyState (lbtn))
			ctrls |= SCK_LBUTTON;
		if (0x8000 & GetAsyncKeyState (rbtn))
			ctrls |= SCK_RBUTTON;
		if (0x8000 & GetAsyncKeyState (VK_MBUTTON))
			ctrls |= SCK_MBUTTON;
		if (0x8000 & GetAsyncKeyState (xbtn1))
			ctrls |= SCK_XBUTTON1;
		if (0x8000 & GetAsyncKeyState (xbtn2))
			ctrls |= SCK_XBUTTON2;
	}
#endif
	if ((vkey == VK_CONTROL) || (vkey == VK_SHIFT) || (vkey == VK_ALT))
	{
#if 0
		if (vkey == VK_CONTROL)
			ctrls |= SCK_CONTROL;
		if (vkey == VK_SHIFT)
			ctrls |= SCK_SHIFT;
		if (vkey == VK_ALT)
			ctrls |= SCK_ALT;
#endif
		vkey = 0;
	}
#if 0
	shortcut = MAKESHORTCUT(vkey, 0, ctrls);
#else
	int keys[2];
	const int count = SIZEOF_ARRAY(keys);
	int iKey, iKeyZero;
	int nrKeys;
	int i;
#if 0
	preVal32 = curVal32;
#endif
	SHORTCUT2KEYS(curVal32, keys);

	if (vkey)
	{
		iKey = iKeyZero = -1;
		nrKeys = 0;
		for (i = 0; i < count; i++)
		{		
			int key = keys[i];
			if (key)
			{
				++nrKeys;
				if (vkey == key)
					iKey = i;				
			}
		} // for (i)
		if (iKey >= 0)
		{
			if (flags & Action::F_UP)
			{
				keys[iKey] = 0;
				for (i = iKey; i < count - 1; i++)
				{
					keys[i] = keys[i+1]; 
					keys[i+1] = 0;
				} // for (i)
				--nrKeys;
			}
		}
		else
		{
			if (nrKeys < count)
			{
				iKey = nrKeys;
				keys[iKey] = vkey;
			}
		}
		curVal32 = MAKESHORTCUT2(keys, ctrls);
#if defined _DEBUG && 0
		TRACE("myShortcut=0x%x\r\n", val32);
#endif
	}
	else
	{
	}

	iKey = iKeyZero = -1;
	nrKeys = 0;
	for (i = 0; i < count; i++)
	{			
		int key = keys[i];
		if (key)
		{
			++nrKeys;
			if (vkey == key)
			{
				iKey = i;
				break;
			}
		}
	} // for (i)

	if (iKey < 0)
	{
		if (nrKeys < count)
		{
			iKey = nrKeys;
			keys[iKey] = vkey;
		}
	}

	if (! (msg == WM_KEYDOWN || msg == WM_KEYUP))
	{// Обработка сообщений мыши:
		int up = Action::F_UP;
		switch (msg)
		{
		case WM_LBUTTONDOWN:
			ctrls |= SCK_LBUTTON;
			break;
		case WM_RBUTTONDOWN:
			ctrls |= SCK_RBUTTON;
			break;
		case WM_MBUTTONDOWN:
			ctrls |= SCK_MBUTTON;			
			break;
		case WM_XBUTTONDOWN:
			ctrls |= SCK_XBUTTON1;
			break;
#if 1
		case WM_LBUTTONUP:
			ctrls |= SCK_LBUTTON;
			flags |= up;
			break;
		case WM_RBUTTONUP:
			ctrls |= SCK_RBUTTON;
			flags |= up;
			break;
		case WM_MBUTTONUP:
			ctrls |= SCK_MBUTTON;
			flags |= up;
			break;
		case WM_XBUTTONUP:
			ctrls |= SCK_XBUTTON1;
			flags |= up;
			break;
#endif
#if 0
		case WM_LBUTTONDBLCLK:
			ctrls |= SCK_LBUTTONDBL;
			break;
		case WM_RBUTTONDBLCLK:
			ctrls |= SCK_RBUTTONDBL;
			break;
#endif
		default:
			goto end;
		} // switch (msg)
#if 1
		if (flags & Action::F_CHECKMOUSEASYNC)
			if (flags & up)
				goto end;
#endif
	}
#endif
	shortcut = MAKESHORTCUT2(keys, ctrls);
#if 0
	shortcut |= preVal32 & 0xffff0000;
	preVal32 = MAKESHORTCUT(0, 0, ctrls);
#endif
#if defined _DEBUG && 1
	TRACE("shortcut=0x%08x%s\r\n", shortcut, (flags & Action::F_UP) ? " UP" : "");
#endif
end:
	return shortcut;
}

void ResetShortcutsLL()
{
	theApp.m_shortcuts.current.val32 = theApp.m_shortcuts.previos.val32 = 0;
}

//
//
//

void LoadTableProperties(LPCTSTR name, QuoteTable::Properties * pProperties)
{
	QuoteTable::Properties properties;
	properties.Initialize();

	CString str;
	CString path;
	path.Format(TEXT("Instruments\\%s"), name); 

	TSTRING_STD2(buf, size);

	CRegKey key;
	key.Open(theApp.GetRegKey(), path);

	if (ERROR_SUCCESS != QueryStringValue(key, str, TEXT("seccode"), TEXT("")))
		str = TEXT("?");
	SAFE_TSTRCPY(properties.seccode, str);
	QueryStringValue(key, str, TEXT("classcode"), TEXT(""));
	SAFE_TSTRCPY(properties.classcode, str);
	QueryStringValue(key, str, TEXT("clientcode"), TEXT(""));
	SAFE_TSTRCPY(properties.clientcode, str);
	QueryStringValue(key, str, TEXT("account"), TEXT(""));
	SAFE_TSTRCPY(properties.account, str);

	TCHAR * pEnd;	
	int nd = properties.price.nd;
	QueryIntValue(key, properties.price.nd, TEXT("price_nrdigits"), nd);
	double d = 1;
	d = Mul_0_1(d, nd);
	FormatString(buf, size, d, nd);
	QueryStringValue(key, str, TEXT("price_step"), buf);
	properties.price.step = _tcstod(str, &pEnd);
	QueryIntValue(key, properties.trading.quantity, TEXT("trade_quantity"), static_cast<int>(properties.trading.quantity));
	FormatString(buf, size, 0.0, nd);
	QueryStringValue(key, str, TEXT("trade_spread"), buf);
	properties.trading.spread = _tcstod(str, &pEnd);

	FormatString(buf, size, 1.0, nd);
	QueryStringValue(key, str, TEXT("stop_value_relative"), buf);
	properties.trading.autos.stop.relative = _tcstod(str, &pEnd);
	FormatString(buf, size, 0.0, nd);
	QueryStringValue(key, str, TEXT("stop_value_absolute"), buf);
	properties.trading.autos.stop.absolute = _tcstod(str, &pEnd);
	QueryIntValue(key, properties.trading.autos.stop.isRelative, TEXT("stop_is_relative"), properties.trading.autos.stop.isRelative);
	FormatString(buf, size, 0.0, nd);
	QueryStringValue(key, str, TEXT("stop_slippage"), buf);
	properties.trading.autos.stop.slippage = _tcstod(str, &pEnd);

	FormatString(buf, size, 1.0, nd);
	QueryStringValue(key, str, TEXT("profit_value_relative"), buf);
	properties.trading.autos.profit.relative = _tcstod(str, &pEnd);
	FormatString(buf, size, 0.0, nd);
	QueryStringValue(key, str, TEXT("profit_value_absolute"), buf);
	properties.trading.autos.profit.absolute = _tcstod(str, &pEnd);
	QueryIntValue(key, properties.trading.autos.profit.isRelative, TEXT("profit_is_relative"), properties.trading.autos.profit.isRelative);
	FormatString(buf, size, 0.0, nd);
	QueryStringValue(key, str, TEXT("profit_slippage"), buf);
	properties.trading.autos.profit.slippage = _tcstod(str, &pEnd);

	*pProperties = properties;
	int len = min(SIZEOF_ARRAY(pProperties->name) - 1, lstrlen(name));
	memcpy(pProperties->name, name, len*sizeof(TCHAR));
	pProperties->name[len] = TEXT('\0');
}

void SaveTableProperties(LPCTSTR name, const QuoteTable::Properties * pProperties)
{
	QuoteTable::Properties properties;
	properties = *pProperties;

	TSTRING_STD2(str, size);

	CString path;
	path.Format(TEXT("Instruments\\%s"), name); 

	CRegKey key;
	key.Create(theApp.GetRegKey(), path);

	key.SetStringValue(TEXT("seccode"), properties.seccode);
	key.SetStringValue(TEXT("classcode"), properties.classcode);
	key.SetStringValue(TEXT("clientcode"), properties.clientcode);
	key.SetStringValue(TEXT("account"), properties.account);

	key.SetDWORDValue(TEXT("price_nrdigits"), properties.price.nd);
	FormatString(str, size, properties.price.step, properties.price.nd);
	key.SetStringValue(TEXT("price_step"), str);
	key.SetDWORDValue(TEXT("trade_quantity"), static_cast<int>(properties.trading.quantity));
	FormatString(str, size, properties.trading.spread, properties.price.nd);
	key.SetStringValue(TEXT("trade_spread"), str);

	FormatString(str, size, properties.trading.autos.stop.relative, 2);
	key.SetStringValue(TEXT("stop_value_relative"), str);
	FormatString(str, size, properties.trading.autos.stop.absolute, properties.price.nd);
	key.SetStringValue(TEXT("stop_value_absolute"), str);
	key.SetDWORDValue(TEXT("stop_is_relative"), properties.trading.autos.stop.isRelative);
	FormatString(str, size, properties.trading.autos.stop.slippage, properties.price.nd);
	key.SetStringValue(TEXT("stop_slippage"), str);

	FormatString(str, size, properties.trading.autos.profit.relative, 2);
	key.SetStringValue(TEXT("profit_value_relative"), str);
	FormatString(str, size, properties.trading.autos.profit.absolute, properties.price.nd);
	key.SetStringValue(TEXT("profit_value_absolute"), str);
	key.SetDWORDValue(TEXT("profit_is_relative"), properties.trading.autos.profit.isRelative);
	FormatString(str, size, properties.trading.autos.profit.slippage, properties.price.nd);
	key.SetStringValue(TEXT("profit_slippage"), str);
}

void RemoveTableProperties(LPCTSTR name)
{
	theApp.DeleteKey(TEXT("Instruments"), name, TRUE);
}

#define MAX_KEY_LENGTH 255
#define MAX_VALUE_NAME 16383

void LoadTableProperties(ListOfTableProperties * pListOfProperties)
{
	int result;
	HKEY hKey;
	CRegKey key;
	result = key.Open(theApp.GetRegKey(), TEXT("Instruments"));
	hKey = (HKEY)key;
	if (result == ERROR_SUCCESS)
	{
		TCHAR    achKey[MAX_KEY_LENGTH];   // buffer for subkey name
		DWORD    cbName;                   // size of name string 
		TCHAR    achClass[MAX_PATH_EX] = TEXT("");  // buffer for class name 
		DWORD    cchClassName = MAX_PATH_EX;  // size of class string 
		DWORD    cSubKeys=0;               // number of subkeys 
		DWORD    cbMaxSubKey;              // longest subkey size 
		DWORD    cchMaxClass;              // longest class string 
		DWORD    cValues;              // number of values for key 
		DWORD    cchMaxValue;          // longest value name 
		DWORD    cbMaxValueData;       // longest value data 
		DWORD    cbSecurityDescriptor; // size of security descriptor 
		FILETIME ftLastWriteTime;      // last write time 
	 
		DWORD i, retCode; 
	 
		DWORD cchValue = MAX_VALUE_NAME; 
	 
		// Get the class name and the value count. 
		retCode = ::RegQueryInfoKey(
			hKey,                    // key handle 
			achClass,                // buffer for class name 
			&cchClassName,           // size of class string 
			NULL,                    // reserved 
			&cSubKeys,               // number of subkeys 
			&cbMaxSubKey,            // longest subkey size 
			&cchMaxClass,            // longest class string 
			&cValues,                // number of values for this key 
			&cchMaxValue,            // longest value name 
			&cbMaxValueData,         // longest value data 
			&cbSecurityDescriptor,   // security descriptor 
			&ftLastWriteTime);       // last write time 
	 
		// Enumerate the subkeys, until RegEnumKeyEx fails.
	    
		if (cSubKeys)
		{
			for (i=0; i < cSubKeys; i++) 
			{ 
				cbName = MAX_KEY_LENGTH;
				retCode = ::RegEnumKeyEx(hKey, i,
						 achKey, 
						 &cbName, 
						 NULL, 
						 NULL, 
						 NULL, 
						 &ftLastWriteTime); 
				if (retCode == ERROR_SUCCESS) 
				{
					QuoteTable::Properties properties;
					LoadTableProperties(achKey, &properties);
					pListOfProperties->Add(&properties);
				}
			}
		}
	}
}

void SaveTableProperties(const ListOfTableProperties * pListOfProperties)
{
	ListOfTableProperties::const_iterator it;
	for (it = pListOfProperties->begin(); it != pListOfProperties->end(); it++)
	{
		SaveTableProperties(it->name, &(*(it)));
	} // for (it)
}

int FormatString(LPTSTR str, size_t size, double d, int n)
{
	int len;
	TCHAR format[] = TEXT("%0.xf");
	format[3] = (n % 10) + 0x30;
	len = _stprintf_s(str, size, format, d);
	return len;
}

double Mul_0_1(double d, int n)
{
	double val = d;
	for (int i = 0; i < n; i++)
		val = (double)val * 0.1;
	return val;
}

int CalculateLengthAfterPoint(LPCTSTR str, int length)
{
	int i, n = 0;
	for (i = length - 1; i >= 0; i--)
	{
		if (str[i] == TEXT('.') || str[i] == TEXT(','))
		{
			n = length - 1 - i;
			break;
		}
	}
	return n;
}

BOOL IsModernOS(DWORD * pMajor, DWORD * pMinor)
{
	OSVERSIONINFO osvi;
	osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	osvi.dwMajorVersion = 0;
	::GetVersionEx(&osvi);
	BOOL modern = (osvi.dwMajorVersion > 5) ? TRUE : FALSE;
	if (pMajor != NULL)
		*pMajor = osvi.dwMajorVersion;
	if (pMinor != NULL)
		*pMinor = osvi.dwMinorVersion;
	return modern;
}

#include <uxtheme.h>

void GetHighlightParams(HWND hWnd, COLORREF & highlight, COLORREF & inactive, COLORREF & active, double & opacity, int flags)
{
#if 0
	HTHEME hTheme = ::OpenThemeData(hWnd, TEXT("MENU"));
	if (hTheme)
	{
		::GetThemeColor(hTheme, MENU_BARBACKGROUND, MB_ACTIVE, TMT_FILLCOLOR, &highlight);
		::GetThemeColor(hTheme, MENU_BARBACKGROUND, MB_ACTIVE, TMT_FILLCOLOR, &inactive);
	}
#endif
#if 0
	highlight = ::GetSysColor(COLOR_GRADIENTACTIVECAPTION);// COLOR_HIGHLIGHT
	inactive = ::GetSysColor(COLOR_GRADIENTINACTIVECAPTION);
#else
	if (! (flags & F_VALID_COLOR_HIGHLIGHT))
		highlight = ::GetSysColor(COLOR_MENUHILIGHT);
#if 1
	inactive = my::lib::SumColorsAlpha(highlight, ::GetSysColor(COLOR_BTNFACE), 0.7);
#else
	inactive = my::lib::SumColorsAlpha(highlight, RGB(250, 250, 250), 0.7);
#endif
#endif
#if 0
	if (hTheme)
		::CloseThemeData(hTheme);
#endif
	active = (hWnd == ::GetFocus()) ? highlight : inactive;
	opacity = 0.25;
}

DWORD SetDlgCursor(CWindow * pDlg, UINT id, LPCTSTR cursor)
{
	HCURSOR hCursor = ::LoadCursor(NULL, cursor);
	HWND hWnd = GetDlgItem(pDlg->m_hWnd, id);
	return ::SetClassLong(hWnd, GCL_HCURSOR, (LONG) hCursor);
}

DWORD SetDlgCursorHand(CWindow * pDlg, UINT id)
{
	return SetDlgCursor(pDlg, id, IDC_HAND);
}

namespace my {
	int MessageBox(HWND hWnd, LPCTSTR text, UINT uType)
	{
		LPCTSTR caption = theApp.GetAppName();
		int ret;
#if 0
		if (hWnd == NULL)
			ret = ::MessageBox(hWnd, text, caption, uType);
		else
#endif
			ret = my::ctrl::MessageBox(hWnd, text, caption, uType);
		return ret;
	}

	namespace ctrl {
		int GetFocusedItem(const CListViewCtrl & list, int iItem, int flags)
		{
			return list.GetNextItem(iItem, flags);
		}
		int SetFocusedItem(CListViewCtrl & list, int iItem, int flags)
		{
			return (TRUE == list.SetItemState(iItem, flags|LVIS_FOCUSED, flags|LVIS_FOCUSED)) ? S_OK : -1;
		}
	} // namespace ctrl {
} // namespace my

void ModifyMenuItemsText(CMenu & menu)
{
	for (int i = 0; i < menu.GetMenuItemCount(); i++)
	{
		TSTRING_STD(text);
		TSTRING_STD2(string, size);
		UINT flags = MF_BYPOSITION;
		int n = menu.GetMenuString(i, string, size - 1, flags);
		if (n > 0)
		{			
			int pos;
			LPTSTR pStr = StrChr(string, TEXT('\t'));
			if (pStr)
				pos = pStr - string;
			else
				pos = n;
			string[pos] = TEXT('\0');

			UINT id = menu.GetMenuItemID(i);
			Action::Type action;			
			switch (id)
			{
			case ID_BUY: action = ACTION_BUY; break;
			case ID_SELL: action = ACTION_SELL; break;
			case ID_BUY_FIXED: action = ACTION_BUY_FIXED; break;
			case ID_SELL_FIXED: action = ACTION_SELL_FIXED; break;
			case ID_BUY_MARKET: action = ACTION_BUY_MARKET; break;
			case ID_SELL_MARKET: action = ACTION_SELL_MARKET; break;
			case ID_BUY_CURRENT: action = ACTION_BUY_CURRENT; break;
			case ID_SELL_CURRENT: action = ACTION_SELL_CURRENT; break;
			case ID_BUY_TA: action = ACTION_BUY_TA; break;
			case ID_SELL_TA: action = ACTION_SELL_TA; break;
			case ID_CANCEL_ORDER: action = ACTION_CANCEL_ORDER; break;
			case ID_CANCEL_ORDER_GLASS: action = ACTION_CANCEL_ORDER; break;
			case ID_CANCEL_ALL_ORDERS: action = ACTION_CANCEL_ORDERS_ALL; break;
			case ID_CANCEL_ALL_ORDERS_BUY: action = ACTION_CANCEL_ORDERS_BUY; break;
			case ID_CANCEL_ALL_ORDERS_SELL: action = ACTION_CANCEL_ORDERS_SELL; break;
			case ID_CANCEL_ALL_ORDERS_STOP: action = ACTION_CANCEL_ORDERS_ALL_STOP; break;
			case ID_STOP_ORDER: action = ACTION_STOP_ORDER; break;
			case ID_STOP_BID_BUY: action = ACTION_STOP_BID_BUY; break;
			case ID_STOP_BID_SELL: action = ACTION_STOP_BID_SELL; break;
			case ID_STOP_BID_BUY_OFFSET: action = ACTION_STOP_BID_BUY_OFFSET; break;
			case ID_STOP_BID_SELL_OFFSET: action = ACTION_STOP_BID_SELL_OFFSET; break;
			case ID_STOP_BID_BUY_TA: action = ACTION_STOP_BID_BUY_TA; break;
			case ID_STOP_BID_SELL_TA: action = ACTION_STOP_BID_SELL_TA; break;
			case ID_STOP_BID_BUY_OFFSET_TA: action = ACTION_STOP_BID_BUY_OFFSET_TA; break;
			case ID_STOP_BID_SELL_OFFSET_TA: action = ACTION_STOP_BID_SELL_OFFSET_TA; break;
			case ID_TRADE_ZERO: action = ACTION_TRADE_ZERO; break;
			case ID_TRADE_REVERSE: action = ACTION_TRADE_REVERSE; break;
			case ID_USER_TRADE: action = ACTION_SHOW_ACTIVE_BIDS_AND_DEALS; break;
			case ID_INSTRUMENT_NEXT: action = ACTION_INSTRUMENT_NEXT; break;
			case ID_INSTRUMENT_PREVIOS: action = ACTION_INSTRUMENT_PREVIOS; break;
			case ID_PROPERTIES: action = ACTION_EDIT_INSTRUMENT; break;
			case ID_TRANSACTION_INFO: action = ACTION_SHOW_TRANSACTION_INFO; break;
			case ID_SHOW_MAIN: action = ACTION_SHOW_HIDE_WINDOW_DEALS; break;
			case ID_SHOW_GLASS: action = ACTION_SHOW_HIDE_WINDOW_GLASS; break;
			case ID_SHOW_HISTORY: action = ACTION_SHOW_HIDE_WINDOW_HISTORY; break;
			case ID_SHOW_MSGLOG: action = ACTION_SHOW_HIDE_WINDOW_MESSAGES; break;
			case ID_SET_BASE: action = ACTION_CHANGE_BUTTONS; break;
			case ID_SET_EXTENDED: action = ACTION_CHANGE_BUTTONS; break;
			case ID_SETTINGS: action = ACTION_SETTINGS; break;
			default: action = ACTION_TYPE_UNKNOWN;
			} // switch (id)

			if (action != ACTION_TYPE_UNKNOWN)
			{
				DWORD shortcut = theApp.GetShortcutKey(action);
				TSTRING_SMALL2(strShortcutKey, size);
				int len = ShortcutToText(shortcut, strShortcutKey, size);
				if (shortcut)
					wsprintf(text, TEXT("%s\t%s"), string, strShortcutKey);
				else
					wsprintf(text, TEXT("%s"), string);
				// Обновляем текст меню:
#if 0
				menu.ModifyMenu(i, flags|MF_STRING, id, text); // не работает!
#else
				MENUITEMINFO info;
				info.cbSize = sizeof(MENUITEMINFO);
				info.fMask = MIIM_STRING;
				info.dwTypeData = text;
				info.cch = SIZEOF_ARRAY(text);
				menu.SetMenuItemInfo(i, TRUE, &info);
#endif
			}
		}
	} // for (i)
}

void MakeColorDefault(Settings::Presentation::ListOfDeals::Color & color, int index)
{
	COLORREF colorBuy = RGB(225, 255, 215);
	COLORREF colorSell = RGB(255, 225, 215);
	COLORREF colorActiveBuy = RGB(0, 64, 64);
	COLORREF colorActiveSell = RGB(64, 0, 64);
	COLORREF colorCanceledBuy = RGB(245, 255, 240);
	COLORREF colorCanceledSell = RGB(255, 245, 240);
	COLORREF colorText = ::GetSysColor(COLOR_WINDOWTEXT);
	COLORREF colorBackground = RGB(255, 255, 255);
	COLORREF colorGray = RGB(150, 150, 150);
	COLORREF colorFailedBuy = colorCanceledBuy;
	COLORREF colorFailedSell = colorCanceledSell;
	COLORREF colorFailedText = RGB(150, 50, 52);
	Settings::Presentation::ListOfDeals::Color colors[] = {
		{colorBackground, colorText},
		{colorBuy, colorText},
		{colorSell, colorText},
		{colorBuy, colorActiveBuy},
		{colorSell, colorActiveSell},
		{colorBuy, colorActiveBuy},
		{colorSell, colorActiveSell},
		{colorCanceledBuy, colorGray},
		{colorCanceledSell, colorGray},
		{colorCanceledBuy, colorGray},
		{colorCanceledSell, colorGray},
		{colorCanceledBuy, colorGray},
		{colorCanceledSell, colorGray},
		{colorFailedBuy, colorFailedText},
		{colorFailedSell, colorFailedText},
		{colorBackground, ::GetSysColor(COLOR_HIGHLIGHT)},
		{::GetSysColor(COLOR_BTNFACE), colorText},
	};
	color = colors[index];
}

void UpdateComboBoxOutline(CComboBox combo, DWORD & effects, int set)
{
	if (combo)
	{
		int i;
		if (set)
		{
			if (effects & CFE_BOLD)
				if (effects & CFE_ITALIC)
					i = I_OUTLINE_BOLDNITALIC; // жирный курсив;
				else
					i = I_OUTLINE_BOLD; // жирный;
			else
				if (effects & CFE_ITALIC)
					i = I_OUTLINE_ITALIC; // курсив;
				else
					i = I_OUTLINE_NORMAL; // обычный;
			combo.SetCurSel(i);
		}
		else
		{
			i = combo.GetCurSel();
			if (i != CB_ERR)
			{
				effects &= ~(CFE_BOLD|CFE_ITALIC);
				if (i == I_OUTLINE_ITALIC)
					effects |= CFE_ITALIC;
				else if (i == I_OUTLINE_BOLD)
					effects |= CFE_BOLD;
				else if (i == I_OUTLINE_BOLDNITALIC)
					effects |= CFE_BOLD|CFE_ITALIC;
			}
		}
	}
}

#if defined _DEBUG && 1

static DWORD ticks1, ticks2; // для замеров быстродействия в режиме отладки;

DWORD SetTicks1()
{
	ticks1 = ::GetTickCount();
	return ticks1;
}

DWORD SetTicks2()
{
	ticks2 = ::GetTickCount();
	return ticks2;
}

DWORD GetTicks1() {return ticks1;}
DWORD GetTicks2() {return ticks2;}
DWORD GetTicksDelta() {return (ticks2 - ticks1);}

#endif // #if defined _DEBUG && 1

//#if defined _DEBUG || defined DEBUG
//#define _STRING2(x) #x
//#define _STRING(x) _STRING2(x)
//#define DBG_WARNING() #pragma message ("WARNING" " in file ""\"" __FILE__"\"" " function "__FUNCTION__ "line " _STRING(__LINE__))
//#else
//#define DBG_WARNING() 
//#endif
