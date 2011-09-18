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
 *  MyQuik.h : main header file for the PROJECT_NAME application
 */

#pragma once

#include "version.h"
#include "resource.h"
#include <std.h>
#include "tables.h"
#include "accounts.h"
#include "myclient.h"
#include "trade.h"
#include "shortcuts.h"
#include "common.h"
#include "um.h"
#include "actions.h"
#include <window.h>

#define COMPANY_NAME TEXT("Go15")

class Transaction;

//
// class Settings
//
class Settings 
{
public:
	Settings()
	{}

	struct Common {
		struct Quik {
			BOOL autoCheckConnection;
		} quik;
		struct Path {
			TCHAR home[MAX_PATH_EX];
			TCHAR quik[MAX_PATH_EX];
			TCHAR trans2quik[MAX_PATH_EX];
			TCHAR history[MAX_PATH_EX];
			TCHAR log[MAX_PATH_EX];
			TCHAR logServer[MAX_PATH_EX];
		} path;
	};

	struct Trading {
		BOOL confirmTransaction;
		BOOL confirmCancel;
		BOOL autoPrice;
		BOOL useSpread;
		BOOL bestIsCurrent;
		BOOL autoStop;
		DWORD flags;
		struct Instrument {
			TSTRING_SMALL(name);
		} instrument;
		struct History {
			BOOL keep;
			BOOL keepCancel;
			BOOL modify;
			BOOL confirmRemove;
		} history;
	};

	struct Log {
		BOOL printTransaction;
		BOOL saveOnDisk;
	};

	struct Server {
		TSTRING_SMALL(name);
	};

	struct Shortcuts {
		my::ShortcutMap layouts[2];
		int iCurrentLayout;
	};

	struct Presentation {
		struct Window {
			DWORD flags;
			struct Opacity {
				int active; // 0 - 100%;
				int inactive; // 0 - 100%;
			} opacity;
		};
		struct View {
			Window item[I_WINDOW_LAST];
		} view;		
		struct Main {
			DWORD flags;
			struct Show {
				DWORD wnd;
				DWORD frame;
			} show;
			int margins;
		} main;
		struct OSD {
			struct Prices {
				TxtFormat current;
				TxtFormat minimum;
				TxtFormat maximum;
			} prices;
			struct Trade {
				TxtFormat basic;
				TxtFormat profit;
				TxtFormat last;
			} trade;
			struct Account {
				TxtFormat limit;
				TxtFormat profit;
			} account;
			TxtFormat percent;
			struct Indicators {
				TxtFormat price;
				TxtFormat demandSupply;
			} indicators;
			struct Background {
				COLORREF color;
			} backgrnd;
			TxtFormat foreground;
			TxtFormat transaction;
			TxtFormat instrument;
			TxtFormat seccode;
			TxtFormat frame;
			BOOL useForegroundFont;
		} osd;

		struct GlassItem { // вынесено отдельно, чтобы не глючил Intelliscene в Visual Studio 2008;
			int i;
			struct Text {
				BOOL enable;
				CHARFORMAT cf;
				DWORD val1;
			} text;
			struct Background {
				BOOL enable;
				COLORREF color;
				DWORD opacity;
				DWORD val1;
			} backgrnd;
			struct Other {
				BOOL enable;
				DWORD val1;
			} other;
			union Union {
				struct Values {
					DWORD value1;
					DWORD value2;					
				} values;
				struct Alignment {
					DWORD value;
					DWORD enable;
				} alignment;
			} u;
		};
		struct Glass {
			GlassItem items[GLASS_ITEM_COLUMNS - GLASS_ITEM_COMMON];
			struct Background {
				COLORREF color;
			} backgrnd;
			struct View {
				int style;
				int flipHorizontal;
				DWORD flags;
			} view;
			struct User {
				DWORD flags;
			} user;
			struct Columns {
				DWORD width;
			} columns;
			struct Margins {
				DWORD value;
				BOOL enable;
			} margins;
			struct Grid {
				COLORREF color;
				DWORD width;
				DWORD lines;
				BOOL show;
			} grid;
			struct Border {
				COLORREF color;
				DWORD width;
				DWORD lines;
				BOOL show;
			} border;
			struct Marker {
				COLORREF color;
				DWORD width;
				BOOL show;
				DWORD follow;
			} marker;

			static DWORD MakeMargins(int top, int bottom, int left, int right) {return MAKE_DWORD_BYTES_4(top, bottom, left, right);}
			static void GetMargins(DWORD spacing, int & top, int & bottom, int & left, int & right) {GET_BYTES_4_DWORD(spacing, top, bottom, left, right);}
			static DWORD MakeColumnWidth(int col1, int col2, int col3) {return MAKE_DWORD_BYTES_3(col1, col2, col3);}
			static void GetColumnWidth(DWORD width, int & col1, int & col2, int & col3) {GET_BYTES_3_DWORD(width, col1, col2, col3);}
			static DWORD MakeColumnAlignment(int col1, int col2, int col3) {return MAKE_DWORD_BYTES_3(col1, col2, col3);}
			static void GetColumnAlignment(DWORD alignment, int & col1, int & col2, int & col3) {GET_BYTES_3_DWORD(alignment, col1, col2, col3);}
		} glass;

		struct ListOfDeals 
		{
			struct Column {
				char index;
				char visible;
				char format;
			};
			struct Columns {
				Column item[NR_COLS_MAX];
			} columns;	

			struct Color {
				COLORREF backgrnd;
				COLORREF text;
				BOOL operator == (const Color & color) {return (this->backgrnd == color.backgrnd && this->text == color.text);}
				BOOL operator != (const Color & color) {return (this->backgrnd != color.backgrnd || this->text != color.text);}
			};
			struct Colors {
				ListOfDeals::Color item[NR_LIST_COLORS_MAX];
				BOOL useDifferentColors;
				BOOL useDifferentColorsText;
			} colors;
		} list;
	};

	Common common;
	Trading trading;
	Log log;
	Server server;
	Shortcuts shortcuts;
	Presentation presentation;

	typedef Presentation::GlassItem GlassItem;

	enum Flags {
		F_PATH_HISTORY = 0x1,
		F_PATH_LOG = 0x2,
	};

}; // class Settings

//
// class Application
//
class Application : public CAppModule
{
public:
	Application();

	LPCTSTR GetAppName() const;
	LPCTSTR GetAppPath() const;

	DWORD GetCurrentVersion() const;
	DWORD GetVersionReg() const;

	LPCTSTR GetProductName() const;
	int GetProductBuild() const;
	int GetProductTime(SYSTEMTIME & time) const;

public:
	HWND GetMainWnd() {return m_hWnd;}

	CRegKey & GetRegKey();

	int SaveWindowPosition(HWND hWnd, LPCTSTR name, int flags = my::lib::FWP_GENERIC);

	int SaveWindowPosition(const LPCREATESTRUCT lpCreateStruct, LPCTSTR name, int flags = my::lib::FWP_GENERIC);
	int LoadWindowPosition(LPCREATESTRUCT lpCreateStruct, LPCTSTR name, int flags = my::lib::FWP_GENERIC);

	int LoadDlgWindowPosition(LPCREATESTRUCT lpCreateStruct, LPCTSTR name);
	int SaveDlgWindowPosition(HWND hWnd, LPCTSTR name);

	int ClearWindowPosition(LPCTSTR name, int flags = my::lib::FWP_GENERIC);

	HICON LoadIcon(LPCTSTR name);
	HICON LoadIcon(UINT id);

	int CreateSubkey(CRegKey & subKey, LPCTSTR path);
	int OpenSubkey(CRegKey & subKey, LPCTSTR path);

	LONG DeleteKey(LPCTSTR path, LPCTSTR name, BOOL recursive = FALSE);

protected:
	int ProcessCmdLine(LPTSTR lpstrCmdLine);
	int SetRegistryKey(LPCTSTR company, LPCTSTR application);

protected:
	int LoadVersion();
	int SaveVersion();

	int LoadProductInfo();

protected:
	struct Product {
		struct Version {
			DWORD cur;
			DWORD reg;
		} version;
		struct Info {
			DWORD versionMS;
			DWORD versionLS;
			String name;
			SYSTEMTIME time;
		} info;
	} m_product;
	
	String m_name;
	String m_path;
	
	HWND m_hWnd;
	struct Reg {
		CString path;
		CRegKey key;
	} m_reg;
};

// CMyQuikApp:
// See MyQuik.cpp for the implementation of this class
//

class CMyQuikApp : public Application
{
	friend int WINAPI _tWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpstrCmdLine, int nCmdShow);
public:
	CMyQuikApp();

public:
	Settings m_settings;

// Overrides
public:
	int DoTransaction(Transaction * pTransaction, const QuoteTable::Properties * pProperties = NULL);

	const Settings & GetSettings() const {return m_settings;}

	const ListOfTableProperties * GetListOfTableProperties() {return &m_listOfTableProperties;}
	int UpdateListOfTableProperties(LPCTSTR name, const QuoteTable::Properties * pProperties, int flags);

	void SaveTableProperties();

	const QuoteTable::Properties * FindTableProperty(LPCTSTR name, int flags);
	const QuoteTable::Properties * FindTableProperty(LPCTSTR name, LPCTSTR seccode = NULL, LPCTSTR classcode = NULL, LPCTSTR clientcode = NULL, LPCTSTR account = NULL);
	const QuoteTable::Properties * FindTableProperty(const Transaction * pTa);
	const QuoteTable::Properties * GetCurrentTableProperty();

	QuoteTable * FindTable(LPCTSTR name, int flags) {return m_tables.FindTable(name, flags);}
	QuoteTable * FindTable(LPCTSTR name, LPCTSTR seccode = NULL, LPCTSTR classcode = NULL, LPCTSTR clientcode = NULL, LPCTSTR account = NULL) {return m_tables.FindTable(name, seccode, classcode, clientcode, account);}
	QuoteTable * FindTable(const Transaction * pTa);
	QuoteTable * CreateTable(LPCTSTR name, const QuoteTable::Properties * pProperties) {return m_tables.CreateTable(name, pProperties);}

	const QuoteTable * GetCurrentTable() const {return m_tables.GetCurrentTable();}
	QuoteTable * GetCurrentTable() {return m_tables.GetCurrentTable();}
	void SetCurrentTable(QuoteTable * pCurrent) {m_tables.SetCurrentTable(pCurrent);}
	QuoteTable * GetTableTmp() {return &m_tables.table;}

	QuoteTable * GetFirstTable(Tables::Iterator & it) {return m_tables.GetFirstTable(it);}
	QuoteTable * GetNextTable(Tables::Iterator & it, int cycle = 1) {return m_tables.GetNextTable(it, cycle);}
	QuoteTable * GetPreviosTable(Tables::Iterator & it, int cycle = 1) {return m_tables.GetPreviosTable(it, cycle);}
	QuoteTable * GetFirstTable() {return m_tables.GetFirstTable();}
	QuoteTable * GetNextTable(int cycle = 1) {return m_tables.GetNextTable(cycle);}
	QuoteTable * GetPreviosTable(int cycle = 1) {return m_tables.GetPreviosTable(cycle);}

	int GetNrInstruments() const {return static_cast<int>(m_tables.size());}

	Account * FindAccount(LPCTSTR name, int flags) {return m_accounts.Find(name, flags);}
	Account * CreateAccount(LPCTSTR name) {return m_accounts.Create(name);}

	void SetCurrentTrade(LPCTSTR name) {m_trader.SetCurrent(name);}
	int OnTradeAction(LPCTSTR name, Transaction * pTransaction) {return m_trader.OnAction(name, pTransaction);}
	void ResetTrade(LPCTSTR name) {m_trader.ResetTrade(name);}
	void ResetAllTrades() {m_trader.ResetAllTrades();}
	BOOL IsActiveTrade(LPCTSTR name) {return m_trader.IsActiveTrade(name);}

	void BeginFindItem() {m_trader.BeginFindItem();}
	void EndFindItem() {m_trader.EndFindItem();}
	const TradeItem * FindItem(int flags, double N, int action, int status, int operation, double price, double quantity) {return m_trader.FindItem(flags, N, action, status, operation, price, quantity);}
	
	int FindItems(::std::list<TradeItem> & list, double price) {return m_trader.FindItems(list, price);}
	int FindItems(::std::list<TradeItem> & list, int flags, int operation, double price) {return m_trader.FindItems(list, flags, operation, price);}

	int GetCurrentTrade(Trade & trade, double price);
	int GetTrade(LPCTSTR name, Trade & trade, double price, double * pLastProfit = NULL);
	int GetTrade(LPCTSTR name, Trade & trade);
	void UpdateTrade(Trade & trade, double price);

	void UpdateTables(const Account * pAccount) {m_tables.UpdateTables(pAccount);}
	void UpdateTables(const ListOfTableProperties * pListOfProperties) {&m_listOfTableProperties;}
	void UpdateTablesProperties() {m_tables.UpdateTables(&m_listOfTableProperties);}

	CImageList & GetIconList() {return m_iconList;}

	void LogMessage(LPCTSTR msg, int code = 0);

	int CreateClient(HWND hWnd);
	int CloseClient();
	int ClearCacheServer();
	int GetServerStatus() const;

	void SetCurrentToolName(LPCTSTR name);
	LPCTSTR GetCurrentToolName() const;
	BOOL IsSameToolName(LPCTSTR name) const;
	BOOL IsAnotherToolName(LPCTSTR name) const;

	int RemoveInstrument(LPCTSTR name);
	int RemoveInstrumentsAll();
	int DoExportInstruments(LPCTSTR name);
	int DoImportInstruments(LPCTSTR name);

	void EnterCriticalSection();
	void LeaveCriticalSection();

	HWND GetMainWnd();
	void SetMainWnd(HWND hWnd);

	Action::Type CheckActionAndExecute(int vkey, UINT msg, LPARAM lParam, HWND hWnd = NULL);

	my::ShortcutMap & GetCurrentLayout() {return m_settings.shortcuts.layouts[m_settings.shortcuts.iCurrentLayout];}
	DWORD GetShortcutKey(int i) {return GetCurrentLayout().at(i);}

	void SetModifySettings(const Settings * pSettings = NULL, int modify = TRUE, int flags = 0);
	BOOL GetModifySettings() const;

	void GetStartTime(SYSTEMTIME & stime);

	HICON LoadSmallIcon(UINT id);
	static HICON LoadIconEx(HINSTANCE hInst, UINT id, int cx, int cy = -1, int flags = LR_DEFAULTCOLOR);

	int ClearWindowPosition(LPCTSTR name, int flags = my::lib::FWP_GENERIC);

	void FlashWindow(HWND hWnd, UINT count = 2, DWORD timeout = 55);

public:
	int EnablelHooks(BOOL global = FALSE);
	int DisablelHooks(BOOL cbt = TRUE, BOOL keyboard = TRUE, BOOL mouse = TRUE);

protected:
	friend LRESULT CALLBACK CBTProc(int nCode, WPARAM wParam, LPARAM lParam);
	friend LRESULT CALLBACK KeyboardProc(int nCode, WPARAM wParam, LPARAM lParam);
	friend LRESULT CALLBACK MouseProc(int nCode, WPARAM wParam, LPARAM lParam);

	BOOL PreTranslateMessage(MSG* pMsg);

protected:
	BOOL InitInstance(LPTSTR lpstrCmdLine);
	int ExitInstance();

	int LoadSettings();
	int SaveSettings();

	void MergeSettings(CRegKey & key, LPCTSTR from, LPCTSTR to);
	void MergeSettings(CRegKey & key, LPCTSTR name, DWORD & dwValue);
	void MergeSettingsGridLines(CRegKey & key, DWORD & dwValue);

	friend DWORD MakeShortcutKey(int vkey, int & flags, UINT msg, LPARAM lParam = 0);
	friend void ResetShortcutsLL();

protected:
	BOOL m_settingsAreModified;
	int m_settingsFlags;

	SYSTEMTIME m_stime;

protected:
	CImageList m_iconList;

	ListOfTableProperties m_listOfTableProperties;
	Tables m_tables;
	Accounts m_accounts;
	Trader m_trader;	

	//CMyServer m_Server;
	CMyClient m_client;	

	CRITICAL_SECTION m_criticalSection;

	enum {
		I_HOOK_CBT,
		I_HOOK_KEYBOARD,
		I_HOOK_MOUSE,
		I_HOOK_LAST,
	};
	struct Hook
	{ 
		int type; 
		HOOKPROC proc; 
		HHOOK hHook; 
	};

	Hook m_hooks[I_HOOK_LAST];

	struct Shortcuts {
		my::Shortcut current;
		my::Shortcut previos;
	} m_shortcuts;

#if USE_GDIPLUS
	ULONG_PTR m_gdiToken;
#endif
}; // CMyQuikApp

extern CMyQuikApp _Module;
#define theApp _Module


DWORD SetDlgCursor(CWindow * pDlg, UINT id, LPCTSTR cursor);
DWORD SetDlgCursorHand(CWindow * pDlg, UINT id);

extern void ModifyMenuItemsText(CMenu & menu);

void MakeColorDefault(Settings::Presentation::ListOfDeals::Color & color, int index);

void UpdateComboBoxOutline(CComboBox combo, DWORD & effects, int set);


struct ParamsShowProperties {
	LPCTSTR name;
	const QuoteTable::Properties * pProperties;
	int flags;

	enum {
		F_NEW = 0x1,
		F_COPY = 0x2,
		F_EDIT = 0x4,
	};
	
	ParamsShowProperties()
	{}
	ParamsShowProperties(LPCTSTR name, const QuoteTable::Properties * pProperties = NULL, int flags = 0) 
	{this->name = name; this->pProperties = pProperties; this->flags = flags;}
};

#include "common.h"
#include "reg.h"


