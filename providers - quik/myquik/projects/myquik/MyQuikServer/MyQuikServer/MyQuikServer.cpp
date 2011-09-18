/*
 *  Copyright (C) 2010 Alexander Chernykh 
 *
 *  This file is part of MyQuikServer: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  MyQuikServer is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 *  MyQuikServer.cpp : Defines the entry point for the application.
 */

#include "stdafx.h"
#include "MyQuikServer.h"
#include "myserv.h"
#include <std.h>
#include <str.h>
#include <CommCtrl.h>
#include <Commdlg.h>

#define USE_TRAY_ICON 1

#if USE_TRAY_ICON
#include <shellapi.h>
#endif
#if 1
#include <ShlObj.h>
#endif

//#pragma comment(linker, "/merge:ATL=.rdata")

#pragma comment(linker, "/SECTION:.shr,RWS")
#pragma data_seg(".shr")
HWND hGlobal = NULL;
#pragma data_seg()

#pragma comment(linker, "/merge:ATL=.rdata")

#define USE_DEFAULT_WINDOW 1

#define LOG_NAME MYQUIKDDESERVER TEXT(".log")

#if USE_TRAY_ICON
#define MY_TRAY_ICON_MESSAGE (WM_APP + 1)
#endif

struct Settings {
	struct SystemTray {
		int autohideIn;
		int showIcon;
		int minimizeTo;
		int closeTo;
	} systray;
	struct Caching {
		int enable;
	} caching;
	int writeMessagesToFile;
	TCHAR strPathLog[MAX_PATH_EX];
	int autorun;
};

void MinimizeToTray(HWND hWnd);
void RestoreWindow(HWND hWnd);

int Shutdown(HWND hWnd, int flags = 0);

CMyServer myServer;
HWND hWndMain;
HWND hWndList;
HANDLE g_hLog;
#if USE_TRAY_ICON
NOTIFYICONDATA niData;
HHOOK hHook;
int minimized;
LRESULT CALLBACK CBTProc(int nCode, WPARAM wParam, LPARAM lParam);
#endif
Settings settings;
int settingsAreModified;
WINDOWINFO windowInfo;

TCHAR strAppPath[MAX_PATH_EX];
TCHAR strAppName[MAX_PATH_EX];
TCHAR strAppData[MAX_PATH_EX];
TCHAR strPathHome[MAX_PATH_EX];

BOOL savePathLog = FALSE;

WNDPROC procWndList;
INT_PTR CALLBACK CallbackWndList(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);

#define COMPANY_NAME TEXT("Go15")

void LoadSettings();
void SaveSettings();

int LoadPosition(HWND hWnd, LPCTSTR name);
int SavePosition(HWND hWnd, LPCTSTR name);

void SetModifiedSettings(int modified = TRUE);

void SetAutorun(int autorun, BOOL set = TRUE, LPTSTR path = NULL, DWORD size = 0);

#define MAX_LOADSTRING 100

LPCTSTR GetWindowName() { return TEXT("Main"); }

// Global Variables:
HINSTANCE hInst;								// current instance
TCHAR szTitle[MAX_LOADSTRING];					// The title bar text
TCHAR szWindowClass[MAX_LOADSTRING];			// the main window class name

// Forward declarations of functions included in this code module:
ATOM				MyRegisterClass(HINSTANCE hInstance);
BOOL				InitInstance(HINSTANCE, int);
LRESULT CALLBACK	WndProc(HWND, UINT, WPARAM, LPARAM);
INT_PTR CALLBACK	About(HWND, UINT, WPARAM, LPARAM);

void PrintMessage(LPCTSTR msg);

#if defined _DEBUG
#define DEBUG_PROCESSES 1
#endif


int APIENTRY _tWinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPTSTR    lpCmdLine,
                     int       nCmdShow)
{
	UNREFERENCED_PARAMETER(hPrevInstance);
	UNREFERENCED_PARAMETER(lpCmdLine);
#if 1
	bool alreadyRunning;
	HANDLE hMutexOneInstance;
	hMutexOneInstance = ::CreateMutex(NULL, TRUE, MYQUIKDDESERVER TEXT("-86FBD2A6-2608-40bb-B350-88960E81E010"));
	alreadyRunning = (::GetLastError() == ERROR_ALREADY_EXISTS);
    if (hMutexOneInstance != NULL) 
    {
        ::ReleaseMutex(hMutexOneInstance);
    }
    if (alreadyRunning)
	{
#if 1
		HWND hOther = hGlobal;
        if (hOther != NULL)
		{
			PostMessage(hOther, UM_RESTORE, 0, 0);
		}
#endif
		return 0;
	}

#endif

	MSG msg;
	HACCEL hAccelTable;

#if 1
	int visible = 0;
	int autorun = 0;

	LPWSTR *szArglist;
	int nArgs;
	szArglist = ::CommandLineToArgvW(GetCommandLineW(), &nArgs);
	if (szArglist != NULL)
	{
		// Полное название приложения:
		SAFE_TSTRCPY(strAppPath, szArglist[0]);
		// Короткое название:
		SAFE_TSTRCPY(strAppName, strAppPath);
		LPCTSTR pStr = StrRChr(strAppName, NULL, TEXT('\\'));
		if (pStr != NULL)
		{
			LPTSTR pEnd = StrRChr(++pStr, NULL, TEXT('.'));
			if (pEnd != NULL)
				*pEnd = TEXT('\0');
			SAFE_TSTRCPY(strAppName, pStr);
		}
		
		for (int i = 1; i < nArgs; i++)
		{
			LPCTSTR str = szArglist[i];
			if (0 == lstrcmp(str, TEXT("--unvisible")))
				visible = 0;
			else if (0 == lstrcmp(str, TEXT("--visible")))
				visible = 1;
			else if (0 == lstrcmp(str, TEXT("--autorun")))
				autorun = TRUE;					
		} // for (i)
		
		::LocalFree(szArglist);
	}
#if 1
	TCHAR path[MAX_PATH_EX];
	memset(strAppData, 0, sizeof(strAppData));
#if 0
	BOOL success = ::SHGetSpecialFolderPath(NULL, path, CSIDL_APPDATA, FALSE);
	if (success)
	{
		_stprintf_s(strAppData, SIZEOF_ARRAY(strAppData), TEXT("%s\\%s"), path, TEXT("MyQuik"));
		success = ::CreateDirectory(strAppData, NULL);
	}
#else
	BOOL validHomePath = FALSE;
	HKEY hKey;
	_stprintf_s(path, SIZEOF_ARRAY(path), TEXT("Software\\%s\\%s\\Settings\\Common"), COMPANY_NAME, TEXT("MyQuik"));
	if (ERROR_SUCCESS == ::RegOpenKeyEx (HKEY_CURRENT_USER, path, 0, KEY_READ, &hKey))
	{
		DWORD dwType = REG_SZ;
		DWORD dwSize;
		if (ERROR_SUCCESS == ::RegQueryValueEx(hKey, TEXT("path_home"), NULL, &dwType, (LPBYTE)strPathHome, &dwSize))
		{
			::CreateDirectory(strPathHome, NULL);
			validHomePath = TRUE;
		}
		::RegCloseKey(hKey);
	}
	if (! validHomePath)
	{
		SAFE_TSTRCPY(strPathHome, strAppPath);
		my::str::TrimRight(strPathHome, lstrlen(strPathHome), TEXT('\\'));
		validHomePath = TRUE;
	}	
#endif		
#endif
	LoadSettings();
	if (autorun)
		settings.autorun = autorun;
	myServer.EnableCaching(settings.caching.enable);

	g_hLog = INVALID_HANDLE_VALUE;
	if (settings.writeMessagesToFile)
	{
		_stprintf_s(settings.strPathLog, SIZEOF_ARRAY(settings.strPathLog), TEXT("%s\\%s"), strPathHome, LOG_NAME);
#if 1
		_stprintf_s(path, SIZEOF_ARRAY(path), TEXT("Software\\%s\\%s\\Settings"), COMPANY_NAME, strAppName);
		if (ERROR_SUCCESS == ::RegOpenKeyEx (HKEY_CURRENT_USER, path, 0, KEY_READ, &hKey))
		{
			DWORD dwType = REG_SZ;
			DWORD dwSize = SIZEOF_ARRAY(settings.strPathLog);
			::RegQueryValueEx(hKey, TEXT("path_log"), NULL, &dwType, (LPBYTE)settings.strPathLog, &dwSize);
			::RegCloseKey(hKey);
		}
#endif		
		g_hLog = ::CreateFile(settings.strPathLog, GENERIC_WRITE, FILE_SHARE_READ, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL); 
	}

	memset(&windowInfo, 0, sizeof(windowInfo));
#endif

	// Initialize global strings
	LoadString(hInstance, IDS_APP_TITLE, szTitle, MAX_LOADSTRING);
	LoadString(hInstance, IDC_MYQUIKSERVER, szWindowClass, MAX_LOADSTRING);
#if USE_DEFAULT_WINDOW
	MyRegisterClass(hInstance);
#else
#endif

	// Perform application initialization:
#if USE_TRAY_ICON
	// После запуска приложение "минимизировано в системный трей":
//#if !defined _DEBUG
//	nCmdShow = SW_HIDE;
//#endif
	if (! visible)
	{
		if (settings.systray.autohideIn)
			nCmdShow = SW_HIDE;
	}
#endif
	if (! InitInstance(hInstance, nCmdShow))
	{
		return FALSE;
	}

	hAccelTable = LoadAccelerators(hInstance, MAKEINTRESOURCE(IDC_MYQUIKSERVER));

	// Main message loop:
	while (GetMessage(&msg, NULL, 0, 0))
	{
		if (!TranslateAccelerator(msg.hwnd, hAccelTable, &msg))
		{
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}

	return (int) msg.wParam;
}



//
//  FUNCTION: MyRegisterClass()
//
//  PURPOSE: Registers the window class.
//
//  COMMENTS:
//
//    This function and its usage are only necessary if you want this code
//    to be compatible with Win32 systems prior to the 'RegisterClassEx'
//    function that was added to Windows 95. It is important to call this function
//    so that the application will get 'well formed' small icons associated
//    with it.
//
ATOM MyRegisterClass(HINSTANCE hInstance)
{
	WNDCLASSEX wcex;

	memset(&wcex, 0, sizeof(wcex));
	wcex.cbSize = sizeof(WNDCLASSEX);

	wcex.style			= CS_HREDRAW | CS_VREDRAW;
	wcex.lpfnWndProc	= WndProc;
	wcex.cbClsExtra		= 0;
	wcex.cbWndExtra		= 0;
	wcex.hInstance		= hInstance;
	wcex.lpszClassName	= szWindowClass;
	if (1)
	{
		wcex.hbrBackground	= (HBRUSH)(COLOR_WINDOW+1);
		wcex.hIcon			= LoadIcon(hInstance, MAKEINTRESOURCE(IDI_MYQUIKSERVER));
		wcex.hCursor		= LoadCursor(NULL, IDC_ARROW);
#if 0
		wcex.hIconSm		= LoadIcon(wcex.hInstance, MAKEINTRESOURCE(IDI_SMALL));
#else
		wcex.hIconSm		= wcex.hIcon;
#endif
#if USE_MENU
		wcex.lpszMenuName	= MAKEINTRESOURCE(IDC_MYQUIKSERVER);
#else
		wcex.lpszMenuName	= NULL;
#endif
	}

	return RegisterClassEx(&wcex);
}

//
//   FUNCTION: InitInstance(HINSTANCE, int)
//
//   PURPOSE: Saves instance handle and creates main window
//
//   COMMENTS:
//
//        In this function, we save the instance handle in a global variable and
//        create and display the main program window.
//
BOOL InitInstance(HINSTANCE hInstance, int nCmdShow)
{
   HWND hWnd;

   hInst = hInstance; // Store instance handle in our global variable

#if USE_DEFAULT_WINDOW
#if 1
   _stprintf_s(szTitle, SIZEOF_ARRAY(szTitle), TEXT("%s"), strAppName);
#endif
   hWnd = CreateWindow(szWindowClass, szTitle, WS_OVERLAPPEDWINDOW,
      CW_USEDEFAULT, 0, CW_USEDEFAULT, 0, NULL, NULL, hInstance, NULL);
   hGlobal = hWndMain = hWnd;
#else
#if 0
	INITCOMMONCONTROLSEX icex;
    
    // Ensure that the common control DLL is loaded. 
    icex.dwSize = sizeof(INITCOMMONCONTROLSEX);
    icex.dwICC  = ICC_LISTVIEW_CLASSES;
    InitCommonControlsEx(&icex); 
#endif
    // Create the list-view window in report view with label editing enabled.
    hWndList = hWnd = CreateWindow(WC_LISTBOX, 
                                szTitle, 
                                WS_OVERLAPPEDWINDOW, // WS_CHILD
                                CW_USEDEFAULT, 0, CW_USEDEFAULT, 0, 
                                NULL, 
                                NULL, 
                                hInstance, 
								NULL);
	if (hWndList != NULL)
	{	
#if 1
		SendMessage(hWndList, LB_ADDSTRING, 0, (LPARAM)TEXT("Hello!")); 
		SendMessage(hWndList, LB_ADDSTRING, 0, (LPARAM)TEXT("Hello!"));
#endif

		//SetWindowLong(hWndList, GWL_WNDPROC, (LONG)WndProc);
	}
#endif

   if (!hWnd)
   {
      return FALSE;
   }	

	LoadPosition(hWnd, GetWindowName());

	ShowWindow(hWnd, nCmdShow);
	UpdateWindow(hWnd);	
	
#if 1
	RECT rcl;
	GetClientRect (hWnd, &rcl); 

	// Create the list-view window in report view with label editing enabled.
	hWndList = CreateWindow(WC_LISTBOX, TEXT(""), 
		WS_CHILD|WS_HSCROLL|WS_VSCROLL|LBS_EXTENDEDSEL, 
		0, 0, 0, 0, 
		hWnd, NULL, hInstance, NULL); 

	procWndList = (WNDPROC)GetWindowLong(hWndList, GWL_WNDPROC);
	SetWindowLong(hWndList, GWL_WNDPROC, (LONG)CallbackWndList);

	HFONT hFont;
	
#if 1
	hFont = CreateFont(15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, TEXT("Tahoma")); // TEXT("Microsoft Sans Serif"));
#else
	hFont = (HFONT)GetStockObject(SYSTEM_FONT);
	//hFont = CreateFont(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NULL);
#endif
	SendMessage(hWndList, WM_SETFONT, (WPARAM)hFont, 1);
	
	ShowWindow(hWndList, SW_SHOW);
	UpdateWindow(hWndList);

	MoveWindow(hWndList, 0, 0, rcl.right - rcl.left, rcl.bottom - rcl.top, TRUE);

	PrintMessage(TEXT("Hello!"));
#endif

	// Запускаем сервер:
	if (myServer.Create(DDE_SERVER_NAME))
	{
		myServer.SetParentWindow(hWnd);
		//PrintMessage(TEXT("DDE-server ")DDE_SERVER_NAME TEXT(" started successfully"));
	}

#if USE_TRAY_ICON	
	ZeroMemory(&niData,sizeof(NOTIFYICONDATA));

	niData.cbSize = sizeof(NOTIFYICONDATA);
	niData.uID = IDI_MYQUIKSERVER;
	niData.uFlags = NIF_ICON|NIF_MESSAGE|NIF_TIP;

	niData.hIcon = (HICON)LoadImage( hInstance,
			MAKEINTRESOURCE(IDI_MYQUIKSERVER),
			IMAGE_ICON,
			GetSystemMetrics(SM_CXSMICON),
			GetSystemMetrics(SM_CYSMICON),
			LR_DEFAULTCOLOR);

	SAFE_TSTRCPY(niData.szTip, DDE_SERVER_NAME);

	niData.hWnd = hWnd;
	niData.uCallbackMessage = MY_TRAY_ICON_MESSAGE;

	if (settings.systray.showIcon)
	{
		Shell_NotifyIcon(NIM_ADD, &niData);	

		//CloseHandle(niData.hIcon);

		hHook = ::SetWindowsHookEx(WH_CBT, CBTProc, hInstance, GetCurrentThreadId());
	}
#endif // USE_TRAY_ICON
	minimized = 0;

   return TRUE;
}

void ModifyContextMenu(HWND hWnd, HMENU hMenu)
{
	UINT id;
	LPCTSTR text;
	if ( !::IsWindowVisible(hWnd) || ::IsIconic(hWnd))
	{
		text = TEXT("Показать");
		id = IDM_SHOW;
	}
	else
	{
		text = TEXT("Убрать");
		id = IDM_HIDE_IN_TRAY;
	}
	ModifyMenu(hMenu, 0, MF_BYPOSITION|MF_STRING, id, text);

	CheckMenuItem(hMenu, ID_OPTIONS_SYSTRAY_AUTOHIDE, MF_BYCOMMAND|((settings.systray.autohideIn) ? MF_CHECKED : MF_UNCHECKED));
	CheckMenuItem(hMenu, ID_OPTIONS_SYSTRAY_SHOW_ICON, MF_BYCOMMAND|((settings.systray.showIcon) ? MF_CHECKED : MF_UNCHECKED));
	CheckMenuItem(hMenu, ID_OPTIONS_SYSTRAY_MINIMIZE, MF_BYCOMMAND|((settings.systray.minimizeTo) ? MF_CHECKED : MF_UNCHECKED));
	CheckMenuItem(hMenu, ID_OPTIONS_SYSTRAY_CLOSEWINDOW, MF_BYCOMMAND|((settings.systray.closeTo) ? MF_CHECKED : MF_UNCHECKED));
	CheckMenuItem(hMenu, ID_OPTIONS_ENABLE_CACHING, MF_BYCOMMAND|((settings.caching.enable) ? MF_CHECKED : MF_UNCHECKED));
	CheckMenuItem(hMenu, ID_OPTIONS_WRITE_MESSAGES_TO_FILE, MF_BYCOMMAND|((settings.writeMessagesToFile) ? MF_CHECKED : MF_UNCHECKED));
	EnableMenuItem(hMenu, ID_OPTIONS_PATH_LOG, MF_BYCOMMAND|((settings.writeMessagesToFile) ? MF_ENABLED : MF_DISABLED|MF_GRAYED));
	CheckMenuItem(hMenu, ID_OPTIONS_AUTORUN, MF_BYCOMMAND|((settings.autorun) ? MF_CHECKED : MF_UNCHECKED));	
}

void ShowContextMenu(HWND hWnd)
{
	HMENU hParent, hMenu;

// create or load a menu
	hParent = ::LoadMenu(hInst, MAKEINTRESOURCE(IDR_MENU_TRAY));
	hMenu = GetSubMenu(hParent, 1);

	ModifyContextMenu(hWnd, hMenu);

	POINT point;
	GetCursorPos(&point);
	::TrackPopupMenu(hMenu, TPM_LEFTALIGN|TPM_RIGHTBUTTON, point.x, point.y, 0, hWnd, NULL);
}

#if USE_TRAY_ICON
void ShowContextMenuTray(HWND hWnd)
{
	HMENU hParent, hMenu;

// create or load a menu
	hParent = ::LoadMenu(hInst, MAKEINTRESOURCE(IDR_MENU_TRAY));
	hMenu = GetSubMenu(hParent, 1);

	ModifyContextMenu(hWnd, hMenu);

	::SetForegroundWindow(hWnd);

	POINT point;
	GetCursorPos(&point);
	::TrackPopupMenu(hMenu, TPM_LEFTALIGN|TPM_RIGHTBUTTON, point.x, point.y, 0, hWnd, NULL);
}

LRESULT CALLBACK CBTProc(int nCode, WPARAM wParam, LPARAM lParam) 
{ 
	HRESULT hResult = S_OK;
    if (nCode >= 0)
	{
		switch (nCode) 
		{ 
			case HCBT_MINMAX:
				{
					HWND hWnd = (HWND)wParam;
					if (hWnd == hWndMain)
					{
						int cmd = LOWORD(lParam);
#if USE_TRAY_ICON
						if (cmd == SW_MINIMIZE)
						{
							if (settings.systray.minimizeTo && !minimized)
								::PostMessage(hWnd, UM_MINIMIZE, 0, 0);
						}
#endif
					}
				}
				break;  // case HCBT_MINMAX
		} 
	}

	return CallNextHookEx(hHook, nCode, wParam, lParam); 
} 

#endif // USE_TRAY_ICON

//
//  FUNCTION: WndProc(HWND, UINT, WPARAM, LPARAM)
//
//  PURPOSE:  Processes messages for the main window.
//
//  WM_COMMAND	- process the application menu
//  WM_PAINT	- Paint the main window
//  WM_DESTROY	- post a quit message and return
//
//
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	int wmId, wmEvent;
	PAINTSTRUCT ps;
	HDC hdc;

	switch (message)
	{
	case WM_COMMAND:
		wmId    = LOWORD(wParam);
		wmEvent = HIWORD(wParam);
		// Parse the menu selections:
		switch (wmId)
		{
		case IDM_ABOUT:
			DialogBox(hInst, MAKEINTRESOURCE(IDD_ABOUTBOX), hWnd, About);
			break;
		case IDM_EXIT:
			DestroyWindow(hWnd);
			break;
		case IDM_SHOW:
			::PostMessage(hWnd, UM_RESTORE, 0, 0);
			break;
		case IDM_HIDE_IN_TRAY:
			::PostMessage(hWnd, UM_MINIMIZE, 0, 0);
			break;
		case ID_OPTIONS_AUTORUN:
			settings.autorun = settings.autorun ^ 1;
			SetAutorun(settings.autorun);
			SetModifiedSettings();
			break;
		case ID_OPTIONS_SYSTRAY_SHOW_ICON:
			settings.systray.showIcon = settings.systray.showIcon ^ 1;
			SetModifiedSettings();
			break;
		case ID_OPTIONS_SYSTRAY_AUTOHIDE:
			settings.systray.autohideIn = settings.systray.autohideIn ^ 1;
			SetModifiedSettings();
			break;
		case ID_OPTIONS_SYSTRAY_MINIMIZE:
			settings.systray.minimizeTo = settings.systray.minimizeTo ^ 1;
			SetModifiedSettings();
			break;
		case ID_OPTIONS_SYSTRAY_CLOSEWINDOW:
			settings.systray.closeTo = settings.systray.closeTo ^ 1;
			SetModifiedSettings();
			break;
		case ID_OPTIONS_ENABLE_CACHING:
			settings.caching.enable = settings.caching.enable ^ 1;
			SetModifiedSettings();
			myServer.EnableCaching(settings.caching.enable);
			break;
		case ID_OPTIONS_WRITE_MESSAGES_TO_FILE:
			settings.writeMessagesToFile = settings.writeMessagesToFile ^ 1;
			SetModifiedSettings();
			break;
		case ID_OPTIONS_PATH_LOG:
			{
				OPENFILENAME ofn;
				TCHAR szFile[MAX_PATH_EX];

				TCHAR path[MAX_PATH_EX];
				SAFE_TSTRCPY(path, settings.strPathLog);
				my::str::TrimRight(path, -1, TEXT('\\'));

				ZeroMemory(&ofn, sizeof(ofn));
				ofn.lStructSize = sizeof(ofn);
				ofn.hwndOwner = NULL;//hWnd;
				ofn.lpstrFile = szFile;
				// Set lpstrFile[0] to '\0' so that GetOpenFileName does not 
				// use the contents of szFile to initialize itself.
				ofn.lpstrFile[0] = '\0';
				ofn.nMaxFile = sizeof(szFile);
				ofn.lpstrFilter = TEXT("Текстовые документы\0*.log;*.txt\0Все файлы\0*.*\0\0");
				ofn.nFilterIndex = 1;
				ofn.lpstrDefExt = TEXT("log");
				ofn.lpstrInitialDir = path;
				ofn.Flags = OFN_PATHMUSTEXIST;
				ofn.lpstrTitle = TEXT("Журнал сообщений");

				if (TRUE == ::GetSaveFileName(&ofn)) 
				{
					if (0 != lstrcmp(settings.strPathLog, szFile))
					{
						SAFE_TSTRCPY(settings.strPathLog, szFile);
						savePathLog = TRUE;
						SetModifiedSettings();
					}
				}
			}
			break;
		default:
			return DefWindowProc(hWnd, message, wParam, lParam);
		}
		break;
#if 1
	case WM_PAINT:
		hdc = BeginPaint(hWnd, &ps);
		// TODO: Add any drawing code here...
		EndPaint(hWnd, &ps);
		break;
#endif
	case WM_SIZE:
	case WM_MOVE:
		{
			RECT rcl;
			::GetClientRect (hWnd, &rcl); 
			if (message == WM_SIZE)
				::MoveWindow(hWndList, 0, 0, rcl.right - rcl.left, rcl.bottom - rcl.top, TRUE);
#if 1			
			if (rcl.left || rcl.right || rcl.top || rcl.bottom)
			{
				windowInfo.cbSize = sizeof(WINDOWINFO);
				::GetWindowInfo(hWnd, &windowInfo);
			}
#endif
		}
		break;
	case UM_CLIENT_MESSAGE:
		{// Сообщение от клиента:
			TRACE("UM_CLIENT_MESSAGE\r\n");
			void * pData = (void*)wParam;
			myServer.OnClientMessage(pData);
		}
		break;
	case WM_CHAR:
		{
			int vkey = (int)wParam;
			if (vkey == VK_ESCAPE)
			{
#if 0
				::PostMessage(hWnd, WM_CLOSE, 0, 0);
#else
				if (settings.systray.closeTo)
					MinimizeToTray(hWnd);
				else
					::ShowWindow(hWnd, SW_MINIMIZE);
#endif
			}
		}
		break;
	case WM_CLOSE:
		{
			if (settings.systray.closeTo)
				MinimizeToTray(hWnd);
			else
				DestroyWindow(hWnd);
		}
		break;
	case WM_DESTROY:
		Shutdown(hWnd);
		break;
	case WM_QUERYENDSESSION:
#if defined _DEBUG && 1
		PrintMessage(TEXT("WM_QUERYENDSESSION"));
#endif
		return TRUE;
	case WM_ENDSESSION:
		{
			int end = (int)wParam;
			UINT flags = (UINT)lParam;
#if defined _DEBUG && 1
			PrintMessage(TEXT("WM_ENDSESSION"));
			if (end)
				PrintMessage(TEXT("\tend"));
			if (flags)
				PrintMessage(TEXT("\tflags"));
			if (flags & ENDSESSION_CLOSEAPP)
				PrintMessage(TEXT("\tENDSESSION_CLOSEAPP"));
			if (flags & ENDSESSION_LOGOFF)
				PrintMessage(TEXT("\tENDSESSION_LOGOFF"));
			if (flags & ENDSESSION_CRITICAL)
				PrintMessage(TEXT("\tENDSESSION_CRITICAL"));
#endif
			if (end && ((flags & (ENDSESSION_CLOSEAPP|ENDSESSION_LOGOFF)) || !flags))
				Shutdown(hWnd, end);
		}
		break;
	case UM_SERVER_FAULT:
		{// Ошибка сервера
			TRACE("UM_SERVER_FAULT\r\n");
		}
		break;
	case UM_PRINT_MESSAGE:
		{
			LPCTSTR msg = (LPCTSTR)wParam;
			PrintMessage(msg);
		}
		break;
#if USE_TRAY_ICON
	case MY_TRAY_ICON_MESSAGE:
		{
			switch(lParam)
			{
#if 1
			case WM_LBUTTONUP:
#else
			case WM_LBUTTONDBLCLK:
#endif
				{
					int restore = 0;
					if ( ! ::IsWindowVisible(hWnd) || ::IsIconic(hWnd))
						restore = 1;
					if (restore)
						RestoreWindow(hWnd);
					else
						MinimizeToTray(hWnd);
				}
				break;
			case WM_RBUTTONUP:
			case WM_CONTEXTMENU:
				ShowContextMenuTray(hWnd);
				break;
			}
		}
		break;
	case WM_RBUTTONUP:
		{
			ShowContextMenu(hWnd);
		}
		break;
	case UM_MINIMIZE:
		{
			MinimizeToTray(hWnd);
		}
		break;
	case UM_RESTORE:
		{
			RestoreWindow(hWnd);
		}
		break;
#endif // USE_TRAY_ICON
	default:
		return DefWindowProc(hWnd, message, wParam, lParam);
	}
	return 0;
}

void RestoreWindow(HWND hWnd)
{	
	int show;
#if 0
	show = SW_RESTORE;
#else
	show = (::IsZoomed(hWnd)) ? SW_SHOWMAXIMIZED : SW_RESTORE;
#endif
	::ShowWindow(hWnd, show);
	::SetForegroundWindow(hWnd);
	::SetActiveWindow(hWnd);
	minimized = 0;
}

void MinimizeToTray(HWND hWnd)
{
	::ShowWindow(hWnd, SW_MINIMIZE);
	::ShowWindow(hWnd, SW_HIDE);
	minimized = 1;
}

// Message handler for about box.
INT_PTR CALLBACK About(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	UNREFERENCED_PARAMETER(lParam);
	switch (message)
	{
	case WM_INITDIALOG:
		return (INT_PTR)TRUE;

	case WM_COMMAND:
		if (LOWORD(wParam) == IDOK || LOWORD(wParam) == IDCANCEL)
		{
			EndDialog(hDlg, LOWORD(wParam));
			return (INT_PTR)TRUE;
		}
		break;
	}
	return (INT_PTR)FALSE;
}

INT_PTR CALLBACK CallbackWndList(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	switch (message)
	{
	case WM_CHAR:
	case WM_RBUTTONUP:
		{
			PostMessage(hWndMain, message, wParam, lParam);
			return (INT_PTR)TRUE;
		}
		break;
	}
	return (INT_PTR)CallWindowProc(procWndList, hWnd, message, wParam, lParam);
}

void PrintMessage(LPCTSTR msg)
{
	if (msg && lstrlen(msg) > 0)
	{
		SYSTEMTIME time;
		GetLocalTime(&time);
		CString str;
#if 0
		str.Format(TEXT("[%02d:%02d:%02d] %s"), time.wHour, time.wMinute, time.wSecond, msg);
#else
		str.Format(TEXT("[%02d.%02d.%02d][%02d:%02d:%02d] %s"), 
			time.wDay, time.wMonth, time.wYear % 100, time.wHour, time.wMinute, time.wSecond, msg);
#endif
		
		if (1)
		{// Запись на диск:
			if (g_hLog != INVALID_HANDLE_VALUE)
			{
				DWORD n, nrBytesWritten;
				CString str2;
				str2.Format(TEXT("%s\r\n"), str);
				USES_CONVERSION;
				LPCSTR data = T2A(str2);
				n = str2.GetLength();
				::WriteFile(g_hLog, data, n, &nrBytesWritten, NULL);
			}
		}

		SendMessage(hWndList, LB_ADDSTRING, 0, (LPARAM)(LPCTSTR)str); 

		//UpdateHorizontalExtent();
	}
}

HWND GetMainWindow() { return hWndMain; }

void LoadSettings()
{	
	BOOL loadDefault = FALSE;
	BOOL setAutorun = FALSE;
	settingsAreModified = FALSE;
	memset(&settings, 0, sizeof(settings));

	HKEY hKey;
	CString path;
#if REG_VISTA
	path.Format(TEXT("Software\\")COMPANY_NAME TEXT("\\%s"), strAppName);
#else
	path.Format(TEXT("Software\\")COMPANY_NAME TEXT("\\%s\\Settings"), strAppName);
#endif
	if (ERROR_SUCCESS == ::RegOpenKeyEx(HKEY_CURRENT_USER, path, 0, KEY_READ, &hKey))
	{		
		DWORD dwType = REG_DWORD;
		DWORD dwSize = sizeof(DWORD);
#if REG_VISTA
		DWORD dwData;
		if (ERROR_SUCCESS == ::RegGetValue(hKey, TEXT("Settings"), TEXT("systray_autohide_in"), RRF_RT_REG_DWORD, &dwType, &dwData, &dwSize))
			memcpy(&settings.systray.autohideIn, &dwData, sizeof(settings.autorun));
#else
		dwSize = sizeof(settings.systray.autohideIn);
		if (ERROR_SUCCESS == ::RegQueryValueEx(hKey, TEXT("systray_autohide_in"), NULL, &dwType, (LPBYTE)&settings.systray.autohideIn, &dwSize));
#endif			
		else
			loadDefault = TRUE;
#if REG_VISTA
		if (ERROR_SUCCESS == ::RegGetValue(hKey, TEXT("Settings"), TEXT("systray_show_icon"), RRF_RT_REG_DWORD, &dwType, &dwData, &dwSize))
			memcpy(&settings.systray.showIcon, &dwData, sizeof(settings.autorun));
#else
		dwSize = sizeof(settings.systray.showIcon);
		if (ERROR_SUCCESS == ::RegQueryValueEx(hKey, TEXT("systray_show_icon"), NULL, &dwType, (LPBYTE)&settings.systray.showIcon, &dwSize));
#endif
		else
			loadDefault = TRUE;
#if REG_VISTA
		if (ERROR_SUCCESS == ::RegGetValue(hKey, TEXT("Settings"), TEXT("systray_minimize_to"), RRF_RT_REG_DWORD, &dwType, &dwData, &dwSize))
			memcpy(&settings.systray.minimizeTo, &dwData, sizeof(settings.autorun));
#else
		dwSize = sizeof(settings.systray.minimizeTo);
		if (ERROR_SUCCESS == ::RegQueryValueEx(hKey, TEXT("systray_minimize_to"), NULL, &dwType, (LPBYTE)&settings.systray.minimizeTo, &dwSize));
#endif			
		else
			loadDefault = TRUE;
#if REG_VISTA
		if (ERROR_SUCCESS == ::RegGetValue(hKey, TEXT("Settings"), TEXT("systray_close_to"), RRF_RT_REG_DWORD, &dwType, &dwData, &dwSize))
			memcpy(&settings.systray.closeTo, &dwData, sizeof(settings.systray.closeTo));
#else
		dwSize = sizeof(settings.systray.closeTo);
		if (ERROR_SUCCESS == ::RegQueryValueEx(hKey, TEXT("systray_close_to"), NULL, &dwType, (LPBYTE)&settings.systray.closeTo, &dwSize));
#endif			
		else
			loadDefault = TRUE;

#if REG_VISTA
		if (ERROR_SUCCESS == ::RegGetValue(hKey, TEXT("Settings"), TEXT("enable_caching"), RRF_RT_REG_DWORD, &dwType, &dwData, &dwSize))
			memcpy(&settings.caching.enable, &dwData, sizeof(settings.caching.enable));
#else
		dwSize = sizeof(settings.caching.enable);
		if (ERROR_SUCCESS == ::RegQueryValueEx(hKey, TEXT("enable_caching"), NULL, &dwType, (LPBYTE)&settings.caching.enable, &dwSize));
#endif			
		else
			loadDefault = TRUE;

#if REG_VISTA
		if (ERROR_SUCCESS == ::RegGetValue(hKey, TEXT("Settings"), TEXT("write_messages_to_file"), RRF_RT_REG_DWORD, &dwType, &dwData, &dwSize))
			memcpy(&settings.writeMessagesToFile, &dwData, sizeof(settings.writeMessagesToFile));
#else
		dwSize = sizeof(settings.writeMessagesToFile);
		if (ERROR_SUCCESS == ::RegQueryValueEx(hKey, TEXT("write_messages_to_file"), NULL, &dwType, (LPBYTE)&settings.writeMessagesToFile, &dwSize));
#endif			
		else
			loadDefault = TRUE;
#if REG_VISTA
		if (ERROR_SUCCESS == ::RegGetValue(hKey, TEXT("Settings"), TEXT("autorun"), RRF_RT_REG_DWORD, &dwType, &dwData, &dwSize))
			memcpy(&settings.autorun, &dwData, sizeof(settings.autorun));
#else
		dwSize = sizeof(settings.autorun);
		if (ERROR_SUCCESS == ::RegQueryValueEx(hKey, TEXT("autorun"), NULL, &dwType, (LPBYTE)&settings.autorun, &dwSize))
		{// Проверяем совпадение текущего пути со значением в реестре:
			TCHAR path[SIZEOF_ARRAY(strAppPath)];
			SetAutorun(settings.autorun, FALSE, path, sizeof(path));
			if (0 != _tcscmp(path, strAppPath))
				setAutorun = TRUE; // необходимо переписать путь;
		}
#endif			
		else
			setAutorun = TRUE;

		::RegCloseKey(hKey);
	}
	else
		loadDefault = TRUE;
	if (loadDefault)
	{
		settings.systray.autohideIn = 1;
		settings.systray.showIcon = 1;
		settings.systray.minimizeTo = 0;
		settings.systray.closeTo = 1;
		settings.caching.enable = 1;
		settings.writeMessagesToFile = 1;
		settings.autorun = 1;
		setAutorun = TRUE;
	}
	if (setAutorun)
	{		
		SetAutorun(settings.autorun);
		SetModifiedSettings();
	}
}

void SaveSettings()
{
	HKEY hKey;
	CString path;
	path.Format(TEXT("Software\\")COMPANY_NAME TEXT("\\%s\\Settings"), strAppName);
	if (ERROR_SUCCESS == ::RegCreateKeyEx(HKEY_CURRENT_USER, path, 0, NULL, 0, KEY_ALL_ACCESS, NULL, &hKey, NULL))
	{
		::RegSetValueEx(hKey, TEXT("systray_autohide_in"), 0, REG_DWORD, reinterpret_cast<const BYTE*>(&settings.systray.autohideIn), sizeof(settings.systray.autohideIn));
		::RegSetValueEx(hKey, TEXT("systray_show_icon"), 0, REG_DWORD, reinterpret_cast<const BYTE*>(&settings.systray.showIcon), sizeof(settings.systray.showIcon));
		::RegSetValueEx(hKey, TEXT("systray_minimize_to"), 0, REG_DWORD, reinterpret_cast<const BYTE*>(&settings.systray.minimizeTo), sizeof(settings.systray.minimizeTo));
		::RegSetValueEx(hKey, TEXT("systray_close_to"), 0, REG_DWORD, reinterpret_cast<const BYTE*>(&settings.systray.closeTo), sizeof(settings.systray.closeTo));

		::RegSetValueEx(hKey, TEXT("enable_caching"), 0, REG_DWORD, reinterpret_cast<const BYTE*>(&settings.caching.enable), sizeof(settings.caching.enable));

		::RegSetValueEx(hKey, TEXT("write_messages_to_file"), 0, REG_DWORD, reinterpret_cast<const BYTE*>(&settings.writeMessagesToFile), sizeof(settings.writeMessagesToFile));
		if (savePathLog)
		{
			::RegSetValueEx(hKey, TEXT("path_log"), 0, REG_SZ, reinterpret_cast<const BYTE*>(settings.strPathLog), sizeof(TCHAR)*lstrlen(settings.strPathLog));
			savePathLog = FALSE;
		}

		::RegSetValueEx(hKey, TEXT("autorun"), 0, REG_DWORD, reinterpret_cast<const BYTE*>(&settings.autorun), sizeof(settings.autorun));

		::RegCloseKey(hKey);
	}
}

void SetModifiedSettings(int modified) 
{
#if 0
	TSTRING_SMALL2(str, size);
	_stprintf_s(str, size, TEXT(__FUNCTION__) TEXT("(%d)"), modified);
	MessageBox(NULL, str, NULL, MB_OK);
#endif
	settingsAreModified = modified; 
}

void SetAutorun(int autorun, BOOL set, LPTSTR path, DWORD size)
{
	int ret;
	HKEY hKey;		
	if (ERROR_SUCCESS == ::RegOpenKeyEx(HKEY_CURRENT_USER, TEXT("Software\\Microsoft\\Windows\\CurrentVersion\\Run"), 
		0, KEY_ALL_ACCESS, &hKey))
	{
		if (set)
		{
			if (autorun)
				::RegSetValueEx(hKey, strAppName, 0, REG_SZ, (BYTE*)strAppPath, sizeof(TCHAR)*lstrlen(strAppPath));
			else
#if REG_VISTA
				ret = ::RegDeleteKeyValue(hKey, NULL, strAppName);
#else
				ret = ::RegDeleteValue(hKey, strAppName);
#endif
			::RegCloseKey(hKey);      
		} // if (set)
		else
		{
			DWORD dwType = REG_SZ;
			DWORD dwSize = size;
			LONG ret = ::RegQueryValueEx(hKey, strAppName, NULL, &dwType, (LPBYTE)path, &dwSize);
#ifdef _DEBUG
			_asm nop;
#endif
		}
	}
}

int LoadPosition(CREATESTRUCT & cs, LPCTSTR name)
{
	int status;
	int x, y, cx, cy;
	int style;

	x = y = cx = cy = -1;
	style = -1;

	HKEY hKey;
	CString path;
#if REG_VISTA
	path.Format(TEXT("Software\\")COMPANY_NAME TEXT("\\%s\\Workspace"), strAppName);
#else
	path.Format(TEXT("Software\\")COMPANY_NAME TEXT("\\%s\\Workspace\\%s"), strAppName, name);
#endif
	if (ERROR_SUCCESS == ::RegOpenKeyEx(HKEY_CURRENT_USER, path, 0, KEY_READ, &hKey))
	{
		DWORD dwType = REG_DWORD;
		DWORD dwSize;

#ifndef _DEBUG
#if REG_VISTA
		DWORD dwData;
		LONG ret = ::RegGetValue(hKey, name, TEXT("x"), RRF_RT_REG_DWORD, &dwType, &dwData, &dwSize);
#endif
#endif
#if REG_VISTA
		if (ERROR_SUCCESS == ::RegGetValue(hKey, name, TEXT("x"), RRF_RT_REG_DWORD, &dwType, &dwData, &dwSize))
			memcpy(&x, &dwData, sizeof(x));
#else
		dwSize = sizeof(x);
		::RegQueryValueEx(hKey, TEXT("x"), NULL, &dwType, (LPBYTE)&x, &dwSize);
#endif
#if REG_VISTA
		if (ERROR_SUCCESS == ::RegGetValue(hKey, name, TEXT("y"), RRF_RT_REG_DWORD, &dwType, &dwData, &dwSize))
			memcpy(&y, &dwData, sizeof(y));
#else
		dwSize = sizeof(y);
		::RegQueryValueEx(hKey, TEXT("y"), NULL, &dwType, (LPBYTE)&y, &dwSize);
#endif
#if REG_VISTA
		if (ERROR_SUCCESS == ::RegGetValue(hKey, name, TEXT("cx"), RRF_RT_REG_DWORD, &dwType, &dwData, &dwSize))
			memcpy(&cx, &dwData, sizeof(cx));
#else
		dwSize = sizeof(cx);
		::RegQueryValueEx(hKey, TEXT("cx"), NULL, &dwType, (LPBYTE)&cx, &dwSize);
#endif
#if REG_VISTA
		if (ERROR_SUCCESS == ::RegGetValue(hKey, name, TEXT("cy"), RRF_RT_REG_DWORD, &dwType, &dwData, &dwSize))
			memcpy(&cy, &dwData, sizeof(cy));
#else
		dwSize = sizeof(cy);
		::RegQueryValueEx(hKey, TEXT("cy"), NULL, &dwType, (LPBYTE)&cy, &dwSize);
#endif
#if REG_VISTA
		if (ERROR_SUCCESS == ::RegGetValue(hKey, name, TEXT("style"), RRF_RT_REG_DWORD, &dwType, &dwData, &dwSize))
			memcpy(&style, &dwData, sizeof(style));
#else
		dwSize = sizeof(style);
		::RegQueryValueEx(hKey, TEXT("style"), NULL, &dwType, (LPBYTE)&style, &dwSize);
#endif

		::RegCloseKey(hKey);	
	}

	cs.x = x; cs.y = y; cs.cx = cx; cs.cy = cy; cs.style = style;

	if (x < 0 || y < 0 || cx < 0 || cy < 0)
		status = -1;
	else
		status = S_OK;

	return status;
}

int SavePosition(const CREATESTRUCT & cs, LPCTSTR name)
{
	int status = -1;
	int x, y, cx, cy;
	int style;

	x = cs.x; y = cs.y; cx = cs.cx; cy = cs.cy;
	style = cs.style;

	HKEY hKey;
	CString path;
	path.Format(TEXT("Software\\")COMPANY_NAME TEXT("\\%s\\Workspace\\%s"), strAppName, name);
	if (ERROR_SUCCESS == ::RegCreateKeyEx(HKEY_CURRENT_USER, path, 0, NULL, 0, KEY_ALL_ACCESS, NULL, &hKey, NULL))
	{
		if ((cs.style & WS_MAXIMIZE) == FALSE)
		{
			::RegSetValueEx(hKey, TEXT("x"), 0, REG_DWORD, reinterpret_cast<const BYTE*>(&x), sizeof(x));
			::RegSetValueEx(hKey, TEXT("y"), 0, REG_DWORD, reinterpret_cast<const BYTE*>(&y), sizeof(y));
			::RegSetValueEx(hKey, TEXT("cx"), 0, REG_DWORD, reinterpret_cast<const BYTE*>(&cx), sizeof(cy));
			::RegSetValueEx(hKey, TEXT("cy"), 0, REG_DWORD, reinterpret_cast<const BYTE*>(&cy), sizeof(cy));
		}
		::RegSetValueEx(hKey, TEXT("style"), 0, REG_DWORD, reinterpret_cast<const BYTE*>(&style), sizeof(style));

		::RegCloseKey(hKey);

		status = S_OK;
	}
	return status;
}

int LoadPosition(HWND hWnd, LPCTSTR name)
{
	CREATESTRUCT cs;
	int status = LoadPosition(cs, name);
	if (status == S_OK)
		::MoveWindow(hWnd, cs.x, cs.y, cs.cx, cs.cy, FALSE);
	return status;
}

int SavePosition(HWND hWnd, LPCTSTR name)
{
	WINDOWINFO info;
#if 0
	if (TRUE == ::GetWindowInfo(hWnd, &info))
#else
	info = windowInfo;
	if (info.cbSize > 0)
#endif
	{
		CREATESTRUCT cs, csPrev;	
		memset(&cs, 0, sizeof(cs));
		cs.x = info.rcWindow.left; cs.y = info.rcWindow.top;
		cs.cx = info.rcWindow.right - info.rcWindow.left; cs.cy = info.rcWindow.bottom - info.rcWindow.top;
		cs.style = info.dwStyle;
		int save = 1;
		if (S_OK == LoadPosition(csPrev, name))
		{
			if (csPrev.x != cs.x || csPrev.y != cs.y || csPrev.cx != cs.cx || csPrev.cy != cs.cy || csPrev.style != cs.style)
				save = 1;
			else
				save = 0;
		}
		if (save)
			return SavePosition(cs, name);
		else
			return S_OK;
	}
	else
		return ::GetLastError();
}

int Shutdown(HWND hWnd, int flags)
{
	if (settingsAreModified)
		SaveSettings();

	SavePosition(hWnd, GetWindowName());

	::UnhookWindowsHookEx(hHook);
	::Shell_NotifyIcon(NIM_DELETE, &niData);

	myServer.Shutdown(flags);

	PrintMessage(TEXT("Goodbye!"));	

	::CloseHandle(g_hLog);

	if (flags)
		myServer.Uninitialize();

	PostQuitMessage(0);

	return S_OK;
}
