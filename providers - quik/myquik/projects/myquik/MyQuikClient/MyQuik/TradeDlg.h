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
 *  TradeDlg.h : interface of the CTradeDlg class
 */

#pragma once

#include "tables.h"
#include "transactions.h"
#include "MyTradeListCtrl.h"
#include "history.h"
#include "myctrlx.h"
#include <vector>
#include <thread.h>
#include <atlctrlx.h>

//
// CTradeDlg dialog
//

class CTradeDlg : public CDialogImpl<CTradeDlg>, public CWinDataExchange<CTradeDlg>
{
	friend class CMyQuikApp;

public:
	CTradeDlg();
	~CTradeDlg();

// Dialog Data
	enum {IDD = IDD_TRADE};

public:
	LPCTSTR GetName() const {return TEXT("DlgTrade");};

	void UpdateCaption();
	void UpdateCaption(const QuoteTable::Properties * pProperties);
	void UpdatePropertiesEx(const QuoteTable::Properties * pProperties);

	int UpdateColumn(int col);
	int UpdateColumnItem(int col, int iItem, const Transaction * pTransaction, const QuoteTable::Properties * pProperties);
	int UpdateColumns(int iItem, const Transaction * pTransaction, const QuoteTable::Properties * pProperties);
	void UpdateColumns(const Settings::Presentation::ListOfDeals & settings, const Settings::Presentation::ListOfDeals & prev);

	int UpdateColumnIndex();

	void UpdatePresentation(const Settings::Presentation & presentation, int show = TRUE, int resize = FALSE);
	void UpdateTransparence(const Settings::Presentation & presentation);

	void EnableItems(BOOL enable = TRUE);

	int AddTransaction(const Transaction * pTransaction, const QuoteTable::Properties * pProperties);
	int UpdateTransaction(const Transaction * pTransaction, const QuoteTable::Properties * pProperties);
	int RemoveTransaction(const Transaction * pTransaction, int flags);	

	enum {
		OPERATION_ANY = 0x00,
		OPERATION_FIXED = 0x01,
		OPERATION_MARKET = 0x02,
		OPERATION_CURRENT = 0x04,
		OPERATION_BEST = 0x08,
		OPERATION_SPREAD = 0x10,
		OPERATION_OFFSET = 0x20,
		OPERATION_TA = 0x100,
		OPERATION_TRADE = 0x200,
	};
	void Buy(int flags);
	void Sell(int flags);

	enum {
		CANCEL_ORDER = 0x01,		
		CANCEL_ORDERS_BUY = 0x02,
		CANCEL_ORDERS_SELL = 0x04,
		CANCEL_ORDERS_STOP = 0x10,
		CANCEL_ORDERS_ALL = (CANCEL_ORDER|CANCEL_ORDERS_BUY|CANCEL_ORDERS_SELL),
	};

	void CancelOrders(int flags);
	void CancelOrderGlass();

public:
	LPCTSTR ColumnIndexToName(int index);
	int GetColumnWidth(int index, LPCTSTR name);

	void ChangeMode();
	void ChangeStyleQuotes();

	void SetHidden(BOOL hidden);
	BOOL IsHidden() const;

	BOOL IsMaster() const;

protected:
	void KeepUserValues(QuoteTable * pTable);
	void RestoreUserValues(QuoteTable * pTable);

protected:
	friend int CALLBACK FuncCompareItems(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort);

	void InitListCtrl();
	void SaveListCtrlParams();
	void SetItemIcon(int iItem, int action, int status, int operation);
	int GetIconIndex(int action, int status, int operation);
	void UpdateStateButtons(int iItem = -1);
	void UpdateTextButtons();
	void UpdateHintButtons();
	
	void EnableButtons(BOOL enable = TRUE); 

	void EnableBuySell(BOOL enable = TRUE);
	void EnableCancel(BOOL enable = TRUE);
	void EnableCancelAll(BOOL enable = TRUE);
	void EnableCancelBuy(BOOL enable = TRUE);
	void EnableCancelSell(BOOL enable = TRUE);
	void EnableTradeZero(BOOL enable = TRUE);
	void EnableTradeReverse(BOOL enable = TRUE);
	void EnableStop(BOOL enable = TRUE);

	void EnableTrading(BOOL enable);
	void CheckAndEnableTrading();
	void CheckAndEnableTrade();

	BOOL CheckEnableBuySell(BOOL & enableBuy, BOOL & enableSell);
	BOOL CheckEnableStopOrder(BOOL & enable);
	BOOL CheckEnableCancel(BOOL & enable, BOOL & enableAll, BOOL & enableBuy, BOOL & enableSell, BOOL & enableStop, int iItem = -1);
	BOOL CheckEnableTrade(BOOL & enableZero, BOOL & enableReverse);
	BOOL CheckActiveBid(BOOL & active, int iItem);

	enum {KEY_TO_CHANGE_MODE = VK_CONTROL};
	enum Mode {
		MODE_NORMAL,
		MODE_ALTERNATIVE,
	};
	Mode GetMode() const;
	void SetMode(Mode mode);

	void Resize(int cx, int cy, BOOL autoSize = FALSE);

	void ModifyMenuEdit(CMenu & menu, UINT id);
	void ModifyMenuButton(CMenu & menu, UINT id);
	void ModifyMenuTrading(CMenu & menu, int flags = OPERATION_ANY|OPERATION_FIXED|OPERATION_TA);
	void ModifyMenuInstruments(CMenu & menu);
	void ModifyMenuShow(CMenu & menu);
	void ModifyMenuShow2(CMenu & menu, CMenu & menuViewDeals);
	void ModifyMenuViewDeals(CMenu & menu);
	void RemoveMenu(CMenu & menu, int pos, int flags);

	void ShowMenuTrading();

	void SelectInstrument(QuoteTable * pTable, LPCTSTR name = NULL);
	void SelectInstrument(LPCTSTR name, DWORD flags = 0x0);

	int ExtractInstrumentName(LPCTSTR name, size_t size);

	double GetPriceUser() const;
	double GetPriceCurrent() const;
	double GetPriceTa() const;

	double GetQuantity() const;

	void ShowGlass(int show = SW_SHOW, int autoHide = 0);
	void ShowHistory(int show = SW_SHOW, int autoHide = 0);
	void ShowLog(int show = SW_SHOW, int autoHide = 0);
	void ShowDlgStop(int show = SW_SHOW, int autoHide = 0);

	BOOL ShowProperties(LPCTSTR name = NULL, LPCTSTR seccode = NULL, const QuoteTable::Properties * pProperties = NULL, int flags = 0);

	void LogMessage(LPCTSTR msg, int code = S_OK);

	void UpdateOSD();

	QuoteTable * OnSecCode(LPCTSTR name, LPCTSTR seccode, LPCTSTR classcode, LPCTSTR clientcode, LPCTSTR account, 
		double price = 0, double priceMin = 0, double priceMax = 0, 
		double demand = 0, double supply = 0, double percent = 0, double priceBid = 0, double priceOffer = 0);

	virtual void IterateTableQuotes(LPCTSTR name, const BYTE * data, DWORD size) = 0;
	virtual void IterateTableCurrent(LPCTSTR name, const BYTE * data, DWORD size) = 0;
	virtual void IterateTableBids(LPCTSTR name, const BYTE * data, DWORD size, const Transaction * pExtTa = NULL) = 0;
	virtual void IterateTableDeals(LPCTSTR name, const BYTE * data, DWORD size, const Transaction * pExtTa = NULL) = 0;
	virtual void IterateTableStops(LPCTSTR name, const BYTE * data, DWORD size, const Transaction * pExtTa = NULL) = 0;
	virtual void IterateTablePortfolio(LPCTSTR name, const BYTE * data, DWORD size) = 0;
	virtual void IterateTableLimits(LPCTSTR name, const BYTE * data, DWORD size) = 0;

	void SetUserTrade(int set = 1, const Settings::Presentation::Glass * pSettings = NULL);
	void UpdateUserTrade(int flags = 0);

	virtual int SaveTrade() = 0;
	virtual int LoadTrade() = 0;

	History * GetHistory() {return m_pHistory;}
	int AddToHistory(const Transaction * pTa);
	int UpdateHistory(const Transaction * pTa);
	void SetHistoryModified(Transaction * pTa);	

	int MakeMainMenu(CMenu & menu, int get = F_GET_MENU_MAIN);

	BOOL IsGlassFrozen() const;
	BOOL IsActiveDlgSettings(CWindow activeWnd) const;

	Transaction * GetSelectedTa();
	const Transaction * GetSelectedTa() const;

	void SetLoading(BOOL loading = TRUE);
	BOOL IsLoading() const {return m_loading;}

	void OnFailTransaction(int result);

public:
	virtual int OnAction(Action::Type action, int flags = 0, HWND hWnd = NULL) = 0;

protected:
#if LOAD_TRADE_IN_SEPARATE_THREAD
	virtual int BeginLoadTrade() = 0;
	virtual int EndLoadTrade() = 0;
#endif
	virtual void OnBeginLoadTrade() = 0;
	virtual void OnEndLoadTrade() = 0;

protected:
	int MessageBox(LPCTSTR lpszText, UINT nType = MB_OK, UINT nIDHelp = 0);	

protected:
	enum {ID_INSTRUMENT_XXX = 55000};

protected:
	BOOL m_initialized;
	BOOL m_viewLog;
	CMyTradeListCtrl m_listCtrl;
	CRect m_rect;

#if USE_STATIC_MENU_MAIN
	CMenu m_menu;
	CMenu m_menuTmp;
#endif
#if USE_STATIC_MENU_USER_VALUES
	CMenu m_menuUserValues;
#endif

	TransCtrl m_transCtrl;

	History * m_pHistory;

	Mode m_mode;
	enum {
		INFO_DISABLED,
		INFO_TRANSACTION,
		INFO_INSTRUMENT,
	};
	int m_info;
	int m_selectInstrument;

	QuoteTable m_userTable;

	CToolTipCtrl * m_pToolTipCtrl;
	CToolTipCtrl m_hints;

	int m_colOrder[NR_COLS_MAX];

	struct Icons {
		Icons() : size(16, 16)
		{}
		HICON hIcon[2];
		CSize size;
	} m_icons;

	struct OSDCtrl {
		DWORD active;
	} m_osdCtrl;

	struct Quik {
		BOOL connected;
	} m_quik;

	int m_activePropertyPage;

	BOOL m_hidden;

	BOOL m_freeze;

	HWND m_hActiveWnd;

protected:
	struct Buttons {
		CMyButton buy;
		CMyButton sell;
		CMyButton cancel;
		CMyButton cancelAll;
		CMyButton zero;
		CMyButton reverse;
		CMyButton stop;
	} m_buttons;

	struct Edits {
		CMyEdit price;
		CMyEdit quantity;
	} m_edits;

	BOOL m_loading;

protected:
	void Close();
	int SaveWindowPosition();

	void ToggleFreeze();

protected:
	BOOL IsAllowed(UINT id) const;
	BOOL IsAllowedBuy() const;
	BOOL IsAllowedSell() const;
	BOOL IsAllowedCancel() const;
	BOOL IsAllowedCancelAll() const;
	BOOL IsAllowedStop() const;
	BOOL IsAllowedReverse() const;
	BOOL IsAllowedZero() const;

	virtual void BuySell(int buy, int flags) = 0;
	virtual void BuySell(int buy, double price, double quantity, int flags) = 0;
	virtual void StopBid(int buy, int flags) = 0;
	virtual void StopBid(int buy, double price, double quantity, int flags) = 0;

protected:
	void OnBuy();
	void OnSell();
	void OnBuyFixed();
	void OnSellFixed();
	void OnBuyMarket();
	void OnSellMarket();
	void OnBuyCurrent();
	void OnSellCurrent();
	void OnBuyTa();
	void OnSellTa();

	void OnStop();
	void OnStopBuy();
	void OnStopSell();
	void OnStopBuyOffset();
	void OnStopSellOffset();
	void OnStopBuyTa();
	void OnStopSellTa();
	void OnStopBuyOffsetTa();
	void OnStopSellOffsetTa();
	void OnStopBidTa();

	void OnChangeActiveBid();

	void OnCancelOrder();
	void OnCancelOrdersAll();
	void OnCancelOrdersBuy();
	void OnCancelOrdersSell();
	void OnCancelOrdersStop();

	void OnTradeZero();
	void OnTradeReverse();

	void OnInstrumentPrevios();
	void OnInstrumentNext();

	void OnTransactionInfo();

	void OnUserTrade();

	void OnShowMain();
	void OnGlass();
	void OnHistory();
	void OnMsglog();

	void OnShowListOfDeals();
	void OnShowOsd();
	void OnShowControls();

	void OnTopmost();

	void OnSetBase();
	void OnSetExtended();

	void OnSettings();

	void OnAbout();

#ifdef _DEBUG
#endif

	void OnViewGlass(UINT id);

protected:
	virtual BOOL InitDialog(CWindow wndFocus, LPARAM lInitParam);

protected:
	BEGIN_MSG_MAP_EX(CTradeDlg)
		MSG_WM_INITDIALOG(OnInitDialog)
		COMMAND_ID_HANDLER_EX(IDCANCEL, OnCancel)
		MSG_WM_CLOSE(OnClose)
		MSG_WM_PAINT(OnPaint)
		MSG_WM_SIZE(OnSize)
		MSG_WM_LBUTTONDOWN(OnLButtonDown)
		MSG_WM_RBUTTONUP(OnRButtonUp)
		MSG_WM_MOUSEMOVE(OnMouseMove)
		MSG_WM_TIMER(OnTimer)		
		MSG_WM_SHOWWINDOW(OnShowWindow)
		MSG_WM_ACTIVATE(OnActivate)
		MSG_WM_ACTIVATEAPP(OnActivateApp)
		MSG_WM_WINDOWPOSCHANGING(OnWindowPosChanging)
		COMMAND_HANDLER_EX(IDC_BUTTON_BUY, BN_CLICKED, OnBnClickedButtonBuy)
		COMMAND_HANDLER_EX(IDC_BUTTON_SELL, BN_CLICKED, OnBnClickedButtonSell)
		COMMAND_HANDLER_EX(IDC_BUTTON_STOP, BN_CLICKED, OnBnClickedButtonStop)
		COMMAND_HANDLER_EX(IDC_BUTTON_CANCEL, BN_CLICKED, OnBnClickedButtonCancel)
		COMMAND_HANDLER_EX(IDC_BUTTON_CANCEL_ALL, BN_CLICKED, OnBnClickedButtonCancelAll)
		COMMAND_HANDLER_EX(IDC_BUTTON_ZERO, BN_CLICKED, OnBnClickedButtonZero)
		COMMAND_HANDLER_EX(IDC_BUTTON_REVERSE, BN_CLICKED, OnBnClickedButtonReverse)
		NOTIFY_HANDLER_EX(IDC_LIST1, LVN_ITEMCHANGED, OnLvnItemchangedList1)
		NOTIFY_HANDLER_EX(IDC_LIST1, NM_RCLICK, OnNMRClickList1)
		NOTIFY_HANDLER_EX(0, HDN_ITEMCLICK, OnHdnItemclickList1)
		COMMAND_HANDLER_EX(IDC_EDIT_TRADE_PRICE1, EN_CHANGE, OnEnChangeTradePrice)
		COMMAND_HANDLER_EX(IDC_EDIT_TRADE_QUANTITY, EN_CHANGE, OnEnChangeTradeQuantity)
		NOTIFY_HANDLER_EX(IDC_SPIN_TRADE_PRICE1, UDN_DELTAPOS, OnDeltaposSpinTradePrice1)
		NOTIFY_HANDLER_EX(IDC_SPIN_TRADE_QUANTITY, UDN_DELTAPOS, OnDeltaposSpinTradeQuantity)
		MESSAGE_RANGE_HANDLER(UM_BASE + 1, UM_LAST - 1, OnWndMsg)
		MESSAGE_HANDLER(WM_COMMAND, OnCmdMsg)
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	BEGIN_DDX_MAP(CTradeDlg)
		DDX_CONTROL(IDC_LIST1, m_listCtrl)
		DDX_CONTROL(IDC_EDIT_TRADE_PRICE1, m_edits.price)
		DDX_CONTROL(IDC_EDIT_TRADE_QUANTITY, m_edits.quantity)
		DDX_CONTROL(IDC_BUTTON_BUY, m_buttons.buy)
		DDX_CONTROL(IDC_BUTTON_SELL, m_buttons.sell)
		DDX_CONTROL(IDC_BUTTON_CANCEL, m_buttons.cancel)
		DDX_CONTROL(IDC_BUTTON_CANCEL_ALL, m_buttons.cancelAll)
		DDX_CONTROL(IDC_BUTTON_ZERO, m_buttons.zero)
		DDX_CONTROL(IDC_BUTTON_REVERSE, m_buttons.reverse)
		DDX_CONTROL(IDC_BUTTON_STOP, m_buttons.stop)
	END_DDX_MAP()

	BOOL OnInitDialog(CWindow wndFocus, LPARAM lInitParam);
	void OnCancel(UINT uNotifyCode, int nID, CWindow wndCtl);

	void OnClose();	
	void OnPaint(CDCHandle );
	void OnSize(UINT nType, CSize size);
	void OnLButtonDown(UINT nFlags, CPoint point);
	void OnRButtonUp(UINT nFlags, CPoint point);
	void OnMouseMove(UINT nFlags, CPoint point);	
	void OnShowWindow(BOOL bShow, UINT nStatus);
	void OnTimer(UINT_PTR nIDEvent);

	void OnBnClickedButtonBuy(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonSell(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonStop(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonCancel(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonCancelAll(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonZero(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonReverse(UINT uNotifyCode, int nID, CWindow wndCtl);

	LRESULT OnLvnItemchangedList1(NMHDR *pNMHDR);
	LRESULT OnNMRClickList1(NMHDR *pNMHDR);
	LRESULT OnHdnItemclickList1(NMHDR *pNMHDR);

	void OnEnChangeTradePrice(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnEnChangeTradeQuantity(UINT uNotifyCode, int nID, CWindow wndCtl);
	LRESULT OnDeltaposSpinTradePrice1(NMHDR *pNMHDR);
	LRESULT OnDeltaposSpinTradeQuantity(NMHDR *pNMHDR);

	LRESULT OnWndMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
	LRESULT OnCmdMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);

	void OnActivate(UINT nState, BOOL bMinimized, CWindow wndOther);
	void OnActivateApp(BOOL bActive, DWORD dwThreadID);

	void OnWindowPosChanging(LPWINDOWPOS lpWndPos);
};
