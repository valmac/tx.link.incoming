// API_Tester_DLGDlg.cpp : implementation file
//

#include "stdafx.h"
#include "API_Tester_DLG.h"
#include "API_Tester_DLGDlg.h"
#include <string>

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

char* replace_char (char* src_str, char chr_to_replace, char new_chr)
{
  char* target_char = src_str;

  while ((target_char = strchr (target_char, chr_to_replace)) && (*target_char != '\0')) 
  {
     *target_char = new_chr;
     target_char += 1;  
  }

  return src_str;
}

struct SColumns 
{
	short col_id; 
	char title[128]; 
	int width;} 
FAR orders_pars [] = 
{
	{0, "",				-1},
	{1, "Mode",			-1},
	{2, "TransId",		-1},
	{3, "Num",			-1},
	{4, "Class",		-1},
	{5, "Sec",			-1},
	{6, "Price",		-1},
	{7, "Balance",		-1},
	{8, "Value",		-1},
	{9, "IsSell",		-1},
	{10, "Status",		-1},
	{11, "Qty",			-1},
	{12, "Date",		-1},
	{13, "Time",		-1},
	{14, "ActTime",		-1},
	{15, "WDTime",		-1},
	{16, "Expiry",		-1},
	{17, "Accuredint",	-1},
	{18, "Yield",		-1},
	{19, "UID",			-1},
	{20, "USERID",		-1},
	{21, "Account",		-1},
	{22, "Brokerref",	-1},
	{23, "ClientCode",	-1},
	{24, "FIRMID",		-1}
},

FAR trades_pars [] =
{
	{0	, "",					-1},
	{1, "Mode",					-1},
	{2, "TradeNum",				-1},
	{3, "OrderNum",				-1},
	{4, "Class",				-1},
	{5, "Sec",					-1},
	{6, "Price",				-1},
	{7, "Volume",				-1},
	{8, "Value",				-1},
	{9, "IsSell",				-1},
	{10, "SettleDate",			-1},
	{11, "TradeDate",           -1},
	{12, "TradeTime",			-1},
	{13, "IsMarginal",			-1},
	{14, "Accuredint",			-1},
	{15, "Yield",				-1},
	{16, "ClearingCommission",	-1},
	{17, "ExchangeCommission",	-1},
	{18, "TSComm",				-1},
	{19, "TradinSysComm",		-1},
	{20, "Price2",				-1},
	{21, "RepoRate",			-1},
	{22, "Repo2Value",			-1},
	{23, "Accuredint2",			-1},
	{24, "RepoTerm",			-1},
	{25, "StartDiscount",		-1},
	{26, "LowerDiscount",		-1},
	{27, "UpperDiscount",		-1},
	{28, "BlockSec",			-1},
	{29, "Currency",			-1},
	{30, "SettleCurrency",		-1},
	{31, "SettleCode",			-1},
	{32, "Account",				-1},
	{33, "Brokerref",			-1},
	{34, "Clientcode",			-1},
	{35, "Userid",				-1},
	{36, "Firmid",				-1},
	{37, "PartnFirmid",			-1},
	{38, "ExchangeCode",		-1},
	{39, "StationId",			-1}
};

void set_list_view_columns (HWND hwnd, int list_view_id, SColumns* cols, int col_nums)
{
	LVCOLUMN   lvcolumn;
	RECT       rect;
	int        res, i;
    int        col_width, w, n;
	
    GetClientRect (GetDlgItem (hwnd, list_view_id), &rect);
	for (i=w=n=0; i < col_nums; ++i)
	{
		if (cols[i].width != -1)
		{
			w += cols[i].width;
			n++;
		}
	}
	if (n < col_nums)
		col_width = (int)((rect.right - rect.left+1-w) / (col_nums-n));
	
	lvcolumn.mask = LVCF_TEXT | LVCF_FMT | LVCF_WIDTH;
	lvcolumn.fmt = LVCFMT_LEFT;
	lvcolumn.cchTextMax = 0;
    lvcolumn.cx = 0;
    lvcolumn.iImage = 0;
	lvcolumn.iSubItem = 0;
    lvcolumn.iOrder = 0;
	
	for (i = 0; i < col_nums; ++i)
    {   	
		if (cols [i].width == -1) 
			lvcolumn.cx = col_width;
		else
			lvcolumn.cx = cols [i].width;
		
		lvcolumn.pszText = cols[i].title;
		res = SendDlgItemMessage (hwnd, list_view_id, LVM_INSERTCOLUMN, (WPARAM) i, (LPARAM)&lvcolumn);
    } 
	
    SendDlgItemMessage (hwnd, list_view_id, LVM_SETEXTENDEDLISTVIEWSTYLE,  
		(WPARAM) LVS_EX_FULLROWSELECT|LVS_EX_GRIDLINES,
		(LPARAM) LVS_EX_FULLROWSELECT|LVS_EX_GRIDLINES
					   );
}

/////////////////////////////////////////////////////////////////////////////
// CAPI_Tester_DLGDlg dialog

CAPI_Tester_DLGDlg::CAPI_Tester_DLGDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CAPI_Tester_DLGDlg::IDD, pParent),
	m_nQUIKConnectionStatus (TRANS2QUIK_QUIK_NOT_CONNECTED),
	m_nDLLConnectionStatus (TRANS2QUIK_DLL_NOT_CONNECTED),
	m_nExtendedErrorCode (0),
	m_bShowMessages (true)
{
	//{{AFX_DATA_INIT(CAPI_Tester_DLGDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
	m_szErrorMessage [0] = 0;
	m_szResultMessage [0] = 0;
}

int CAPI_Tester_DLGDlg::MsgBox (LPCTSTR lpszText, LPCTSTR lpszCaption, UINT nType)
{
	if (!m_bShowMessages)
		return -1;
	else
		return MessageBox (lpszText, lpszCaption, nType);
}

void CAPI_Tester_DLGDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAPI_Tester_DLGDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAPI_Tester_DLGDlg, CDialog)
	//{{AFX_MSG_MAP(CAPI_Tester_DLGDlg)
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_BN_CLICKED(IDC_CONNECT, OnConnect)
	ON_BN_CLICKED(IDC_IS_DLL_CONNECTED, OnIsDllConnected)
	ON_BN_CLICKED(IDC_IS_QUIK_CONNECTED, OnIsQuikConnected)
	ON_BN_CLICKED(IDC_SET_CALLBACKS, OnSetCallbacks)
	ON_BN_CLICKED(IDC_SEND_SYNC_TRANS, OnSyncTrans)
	ON_BN_CLICKED(IDC_DISCONNECT, OnDisconnect)
	ON_BN_CLICKED(IDC_SEND_ASYNC_TRANS2, OnSendAsyncTrans)
	ON_BN_CLICKED(IDC_SHOW_MESSAGE, OnShowMessages)
	ON_BN_CLICKED(IDC_BUTTON_ORDERS_START, OnOrdersStart)
	ON_BN_CLICKED(IDC_BUTTON_ORDERS_SUBSCRIBE, OnOrdersSubscribe)
	ON_BN_CLICKED(IDC_BUTTON_ORDERS_UNSUBSCRIBE, OnOrdersUnsubscribe)
	ON_BN_CLICKED(IDC_BUTTON_TRADES_START, OnTradesStart)
	ON_BN_CLICKED(IDC_BUTTON_TRADES_SUBSCRIBE, OnTradesSubscribe)
	ON_BN_CLICKED(IDC_BUTTON_TRADES_UNSUBSCRIBE, OnTradesUnsubscribe)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CAPI_Tester_DLGDlg message handlers

HWND g_hWndParent = NULL;

BOOL CAPI_Tester_DLGDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon
	
	g_hWndParent = m_hWnd;

	CheckDlgButton (IDC_SHOW_MESSAGE, m_bShowMessages ? BST_CHECKED : BST_UNCHECKED);
	// TODO: Add extra initialization here

	set_list_view_columns (m_hWnd, IDC_LIST_ORDERS, orders_pars, sizeof(orders_pars)/sizeof(SColumns));
	set_list_view_columns (m_hWnd, IDC_LIST_TRADES, trades_pars, sizeof(trades_pars)/sizeof(SColumns));
	
	return TRUE;  // return TRUE  unless you set the focus to a control
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CAPI_Tester_DLGDlg::OnPaint() 
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, (WPARAM) dc.GetSafeHdc(), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}

// The system calls this to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR CAPI_Tester_DLGDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

BOOL g_bOrdersUnsubscribed = FALSE;
BOOL g_bTradesUnsubscribed = FALSE;

void CAPI_Tester_DLGDlg::OnConnect() 
{
	// TODO: Add your control notification handler code here

	char szQUIKTerminalPath [MAX_PATH] = "";

	::GetDlgItemText (m_hWnd, IDC_QUIK_TERMINAL_PATH, szQUIKTerminalPath, sizeof (szQUIKTerminalPath));
	if (szQUIKTerminalPath [0] == 0)
		return;

	m_nResult = TRANS2QUIK_CONNECT (szQUIKTerminalPath, &m_nExtendedErrorCode, m_szErrorMessage, sizeof (m_szErrorMessage));
	if (m_nResult != TRANS2QUIK_SUCCESS)
	{
		MsgBox (m_szErrorMessage, "Error", MB_ICONHAND);
	}
	else
	{
		MsgBox ("DLL is connected to QUIK terminal", "Attention", MB_ICONINFORMATION);
		g_bTradesUnsubscribed = g_bOrdersUnsubscribed = TRUE;
	}
}

void CAPI_Tester_DLGDlg::OnIsDllConnected() 
{
	// TODO: Add your control notification handler code here

	m_nResult = TRANS2QUIK_IS_DLL_CONNECTED (&m_nExtendedErrorCode, m_szErrorMessage, sizeof (m_szErrorMessage));
	MsgBox ((m_nResult == TRANS2QUIK_DLL_CONNECTED) ? "DLL is connected" : "DLL is not connected", "Attention!!!");
}

void CAPI_Tester_DLGDlg::OnIsQuikConnected() 
{
	// TODO: Add your control notification handler code here
	
	m_nResult = TRANS2QUIK_IS_QUIK_CONNECTED (&m_nExtendedErrorCode, m_szErrorMessage, sizeof (m_szErrorMessage));
	MsgBox ((m_nResult == TRANS2QUIK_QUIK_CONNECTED) ? "QUIK is connected" : (m_nResult == TRANS2QUIK_DLL_NOT_CONNECTED) ? "DLL is not connected" : "QUIK is not connected", "Attention!!!");
}

extern "C" void __stdcall TRANS2QUIK_ConnectionStatusCallback (long nConnectionEvent, long nExtendedErrorCode, LPCSTR lpcstrInfoMessage)
{
	if (nConnectionEvent == TRANS2QUIK_DLL_CONNECTED)
		MessageBox (g_hWndParent, "DLL is connected", "Attention!!!", MB_ICONINFORMATION);

	if (nConnectionEvent == TRANS2QUIK_DLL_DISCONNECTED)
		MessageBox (g_hWndParent, (char*)(_bstr_t ("DLL is disconnected: ") + _bstr_t (lpcstrInfoMessage)), "Attention!!!", MB_ICONINFORMATION);

	if (nConnectionEvent == TRANS2QUIK_QUIK_CONNECTED)
		MessageBox (g_hWndParent, "QUIK is connected", "Attention!!!", MB_ICONINFORMATION);

	if (nConnectionEvent == TRANS2QUIK_QUIK_DISCONNECTED)
		MessageBox (g_hWndParent, (char*)(_bstr_t ("QUIK is disconnected: ") + _bstr_t (lpcstrInfoMessage)), "Attention!!!", MB_ICONINFORMATION);
}

extern "C" void __stdcall TRANS2QUIK_TransactionsReplyCallback (long nTransactionResult, long nTransactionExtendedErrorCode, long nTransactionReplyCode, DWORD dwTransId, double dOrderNum, LPCSTR lpcstrTransactionReplyMessage)
{
	char Message [1024] = "";

	sprintf (
				Message, "Transaction result:\n"
				"nTransactionResult = %ld,\n"
				"nReturnCode = %ld,\n"
				"dwTransId = %u,\n"
				"dOrderNum = %.0f,\n"
				"szResultMessage = \'%s\',\n"
				"nExtendedErrorCode = %ld,\n",
				nTransactionResult, nTransactionReplyCode, dwTransId, dOrderNum, lpcstrTransactionReplyMessage, nTransactionExtendedErrorCode
			);

	::MessageBox (g_hWndParent, Message, "Attention!!!", MB_ICONINFORMATION);
}

extern "C" void __stdcall TRANS2QUIK_TransactionsReplyCallbackEx (long nTransactionResult, long nTransactionExtendedErrorCode, long nTransactionReplyCode, DWORD dwTransId, double dOrderNum, LPCSTR lpcstrTransactionReplyMessage)
{
	char Message [1024] = "";
	
	sprintf (
		Message, "Transaction result:\n"
		"nTransactionResult = %ld,\n"
		"nReturnCode = %ld,\n"
		"dwTransId = %u,\n"
		"dOrderNum = %.0f,\n"
		"szResultMessage = \'%s\',\n"
		"nExtendedErrorCode = %ld,\n",
		nTransactionResult, nTransactionReplyCode, dwTransId, dOrderNum, lpcstrTransactionReplyMessage, nTransactionExtendedErrorCode
		);
	
	//::MessageBox (g_hWndParent, Message, "Attention!!!", MB_ICONINFORMATION);
}



struct SSyncTransThreadParam
{
	HWND hwndButton;
	char *pszTransaction;
};

UINT SyncTransThread (LPVOID pvParam)
{
	char szMessage [1024] = "";
	SSyncTransThreadParam *p = (SSyncTransThreadParam*)pvParam;
	if (!p)
		return 0;

	char *pszTransaction = p->pszTransaction;
	
	if (!pszTransaction)
		return 0;

	long nReturnCode;
	DWORD dwTransId;
	double dOrderNum;
	char szResultMessage[1024];
	long nExtendedErrorCode;
	char szErrorMessage[1024];
	long nResult;
	
	nResult = TRANS2QUIK_SEND_SYNC_TRANSACTION (pszTransaction, &nReturnCode, &dwTransId, &dOrderNum, szResultMessage, sizeof (szResultMessage), &nExtendedErrorCode, szErrorMessage, sizeof (szErrorMessage));
	
	sprintf (
		szMessage, "Sync transaction result:"
		"nReturnCode = %ld,\n"
		"dwTransId = %u,\n"
		"dOrderNum = %.0f,\n"
		"nExtendedErrorCode = %ld,\n"
		"szResultMessage = \'%s\',\n",
		nReturnCode, dwTransId, dOrderNum, nExtendedErrorCode, szResultMessage
		);
	
	//	if (m_nResult != TRANS2QUIK_SUCCESS)
	//		MsgBox (m_szErrorMessage, "Error", MB_ICONHAND);
	//	else
	MessageBox (NULL, szMessage, "Attention", MB_ICONINFORMATION);

	delete [] pszTransaction;
	EnableWindow(p->hwndButton, TRUE);
	delete p;
	
	return 1;
	
}
void CAPI_Tester_DLGDlg::OnSyncTrans() 
{
	// TODO: Add your control notification handler code here
	char szMessage [1024] = "";
	char szTransaction [1024] = "";

	::GetDlgItemText (m_hWnd, IDC_TRANSACTION_BODY, szTransaction, sizeof (szTransaction));
 
	replace_char (szTransaction, '\r', ' ');
	replace_char (szTransaction, '\n', ' ');

	char *pszTransaction = new char[strlen(szTransaction)+1];
	strcpy (pszTransaction, szTransaction);
	SSyncTransThreadParam *p = new SSyncTransThreadParam();
	p->pszTransaction = pszTransaction;
	p->hwndButton = ::GetDlgItem(m_hWnd, IDC_SEND_SYNC_TRANS);

	::EnableWindow (p->hwndButton, FALSE);
	AfxBeginThread(SyncTransThread,p);
	return;

	m_nResult = TRANS2QUIK_SEND_SYNC_TRANSACTION (szTransaction, &m_nReturnCode, &m_dwTransId, &m_dOrderNum, m_szResultMessage, sizeof (m_szResultMessage), &m_nExtendedErrorCode, m_szErrorMessage, sizeof (m_szErrorMessage));
	
	sprintf (
				szMessage, "Sync transaction result:"
				"nReturnCode = %ld,\n"
				"dwTransId = %u,\n"
				"dOrderNum = %.0f,\n"
				"nExtendedErrorCode = %ld,\n"
				"szResultMessage = \'%s\',\n",
				m_nReturnCode, m_dwTransId, m_dOrderNum, m_nExtendedErrorCode, m_szResultMessage
			);

//	if (m_nResult != TRANS2QUIK_SUCCESS)
//		MsgBox (m_szErrorMessage, "Error", MB_ICONHAND);
//	else
		MsgBox (szMessage, "Attention", MB_ICONINFORMATION);
}

void CAPI_Tester_DLGDlg::OnDisconnect() 
{
	// TODO: Add your control notification handler code here
	m_nResult = TRANS2QUIK_DISCONNECT (&m_nExtendedErrorCode, m_szErrorMessage, sizeof (m_szErrorMessage));
	if (m_nResult != TRANS2QUIK_SUCCESS)
	{
		MsgBox (m_szErrorMessage, "Error", MB_ICONHAND);
	}
	else
	{
		MsgBox ("DLL is disconnected from QUIK terminal", "Attention", MB_ICONASTERISK);
	}
}

void CAPI_Tester_DLGDlg::OnSendAsyncTrans() 
{
	char szTransaction [1024] = "";

	::GetDlgItemText (m_hWnd, IDC_TRANSACTION_BODY, szTransaction, sizeof (szTransaction));

	replace_char (szTransaction, '\r', ' ');
	replace_char (szTransaction, '\n', ' ');

	// TODO: Add your control notification handler code here
	m_nResult = TRANS2QUIK_SEND_ASYNC_TRANSACTION (szTransaction, &m_nExtendedErrorCode, m_szErrorMessage, sizeof (m_szErrorMessage));

	if (m_nResult != TRANS2QUIK_SUCCESS)
		MsgBox (m_szErrorMessage, "Attention", MB_ICONINFORMATION);
}

void CAPI_Tester_DLGDlg::OnShowMessages() 
{
	// TODO: Add your control notification handler code here

	if (IsDlgButtonChecked (IDC_SHOW_MESSAGE) == BST_CHECKED)
		m_bShowMessages = true;
	else
		m_bShowMessages = false;		
}

std::string Trans2QuikResultToStr (long result)
{
	switch (result)
	{
	case 0:
		return "TRANS2QUIK_SUCCESS";
	case 1:
		return "TRANS2QUIK_FAILED";
	case 2:
		return "TRANS2QUIK_QUIK_TERMINAL_NOT_FOUND";
	case 3:
		return "TRANS2QUIK_DLL_VERSION_NOT_SUPPORTED";
	case 4:
		return "TRANS2QUIK_ALREADY_CONNECTED_TO_QUIK";
	case 5:
		return "TRANS2QUIK_WRONG_SYNTAX";
	case 6:
		return "TRANS2QUIK_QUIK_NOT_CONNECTED";
	case 7:
		return "TRANS2QUIK_DLL_NOT_CONNECTED";
	case 8:
		return "TRANS2QUIK_QUIK_CONNECTED";
	case 9:
		return "TRANS2QUIK_QUIK_DISCONNECTED";
	case 10:
		return "TRANS2QUIK_DLL_CONNECTED";
	case 11:
		return "TRANS2QUIK_DLL_DISCONNECTED";
	case 12:
		return "TRANS2QUIK_MEMORY_ALLOCATION_ERROR";
	case 13:
		return "TRANS2QUIK_WRONG_CONNECTION_HANDLE";
	case 14:
		return "TRANS2QUIK_WRONG_INPUT_PARAMS";
	}
	return "";
}



void CAPI_Tester_DLGDlg::OnOrdersSubscribe()
{
	char szClass [1024] = "";
	char szSecs  [1024] = "";

	::GetDlgItemText (m_hWnd, IDC_CLASS, szClass, sizeof(szClass));
	::GetDlgItemText (m_hWnd, IDC_SECS, szSecs, sizeof(szClass));

	long nRet = TRANS2QUIK_SUBSCRIBE_ORDERS (szClass, szSecs);
	if (nRet != TRANS2QUIK_SUCCESS)
		MsgBox (Trans2QuikResultToStr(nRet).c_str(), "Attention", MB_ICONINFORMATION);
}	

void CAPI_Tester_DLGDlg::OnTradesSubscribe()
{
	char szClass [1024] = "";
	char szSecs  [1024] = "";
	
	::GetDlgItemText (m_hWnd, IDC_CLASS, szClass, sizeof(szClass));
	::GetDlgItemText (m_hWnd, IDC_SECS, szSecs, sizeof(szClass));
	
	long nRet = TRANS2QUIK_SUBSCRIBE_TRADES (szClass, szSecs);
	if (nRet != TRANS2QUIK_SUCCESS)
		MsgBox (Trans2QuikResultToStr(nRet).c_str(), "Attention", MB_ICONINFORMATION);
}	

void CAPI_Tester_DLGDlg::OnOrdersUnsubscribe()
{
	long nRet = TRANS2QUIK_UNSUBSCRIBE_ORDERS();
	if (nRet != TRANS2QUIK_SUCCESS)
		MsgBox (Trans2QuikResultToStr(nRet).c_str(), "Attention", MB_ICONINFORMATION);	

	g_bOrdersUnsubscribed = TRUE;
}

void CAPI_Tester_DLGDlg::OnTradesUnsubscribe()
{
	long nRet = TRANS2QUIK_UNSUBSCRIBE_TRADES();
	if (nRet != TRANS2QUIK_SUCCESS)
		MsgBox (Trans2QuikResultToStr(nRet).c_str(), "Attention", MB_ICONINFORMATION);	

	g_bTradesUnsubscribed = TRUE;
}



HWND orders_list_view = NULL;

void __stdcall ORDER_STATUS_CALLBACK(long nMode, DWORD dwTransID, double dNumber, LPCTSTR ClassCode, LPCTSTR SecCode, double dPrice, long nBalance, double dValue, long nIsSell, long nStatus, long nOrderDescriptor)
{
	// if list_view_index == -1, than insert into list view, else insert to list_view_index
	char   item_text[200] = "";
	char   tmp[200] = "";
	LVITEM lvitem;
	int    Item = ListView_GetItemCount (orders_list_view);
	
	lvitem.mask = LVIF_TEXT | LVIF_PARAM;
	
	lvitem.cchTextMax = 0;
    lvitem.iItem = Item;
    lvitem.iSubItem = 0;
    lvitem.stateMask = 0;
    lvitem.state = 0;
    lvitem.iIndent = 0;
    lvitem.pszText = item_text;
    lvitem.lParam = 0; 

	sprintf (item_text, "%d", Item);
	
    Item = SendMessage (orders_list_view, LVM_INSERTITEM, (WPARAM) 0, (LPARAM)&lvitem);
    if (Item == -1) 
		return;
    else
        lvitem.iItem = Item;
	
    lvitem.mask = LVIF_TEXT;
    lvitem.iSubItem++;
	
	sprintf (item_text, "%ld", nMode);
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;
	
	sprintf (item_text, "%lu", dwTransID);
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%.0lf", dNumber);
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	_bstr_t bstr = ClassCode;
	sprintf (item_text, "%s", bstr.operator char*());
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	bstr = SecCode;
	sprintf (item_text, "%s", bstr.operator char*());
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%lf", dPrice);
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%ld", nBalance);
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;
	
	sprintf (item_text, "%lf", dValue);
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%ld", nIsSell);
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%ld", nStatus);
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;
	
	sprintf (item_text, "%ld", TRANS2QUIK_ORDER_QTY(nOrderDescriptor));
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%ld", TRANS2QUIK_ORDER_DATE(nOrderDescriptor));
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%ld", TRANS2QUIK_ORDER_TIME(nOrderDescriptor));
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%ld", TRANS2QUIK_ORDER_ACTIVATION_TIME(nOrderDescriptor));
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%ld", TRANS2QUIK_ORDER_WITHDRAW_TIME(nOrderDescriptor));
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%ld", TRANS2QUIK_ORDER_EXPIRY(nOrderDescriptor));
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%lf", TRANS2QUIK_ORDER_ACCRUED_INT(nOrderDescriptor));
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%lf", TRANS2QUIK_ORDER_YIELD(nOrderDescriptor));
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%ld", TRANS2QUIK_ORDER_UID(nOrderDescriptor));
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", TRANS2QUIK_ORDER_USERID(nOrderDescriptor));
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", TRANS2QUIK_ORDER_ACCOUNT(nOrderDescriptor));
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", TRANS2QUIK_ORDER_BROKERREF(nOrderDescriptor));
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", TRANS2QUIK_ORDER_CLIENT_CODE(nOrderDescriptor));
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", TRANS2QUIK_ORDER_FIRMID(nOrderDescriptor));
	SendMessage (orders_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;
}

void CAPI_Tester_DLGDlg::OnOrdersStart()
{
	orders_list_view = ::GetDlgItem(m_hWnd, IDC_LIST_ORDERS);

	if (g_bOrdersUnsubscribed)
	{
		g_bOrdersUnsubscribed = FALSE;
		::SendMessage (orders_list_view, LVM_DELETEALLITEMS, 0, 0);
	}

	long nRet = TRANS2QUIK_START_ORDERS (ORDER_STATUS_CALLBACK);

	if (nRet != TRANS2QUIK_SUCCESS)
		MsgBox (Trans2QuikResultToStr(nRet).c_str(), "Attention", MB_ICONINFORMATION);	
}

HWND trades_list_view = NULL;

void __stdcall TRADE_STATUS_CALLBACK (long nMode, double dNumber, double dOrderNumber, LPCTSTR ClassCode, LPCTSTR SecCode, double dPrice, long nQty, double dValue, long nIsSell, long nTradeDescriptor)
{
	// if list_view_index == -1, than insert into list view, else insert to list_view_index
	char   item_text[200] = "";
	char   tmp[200] = "";
	LVITEM lvitem;
	int    Item = ListView_GetItemCount (trades_list_view);
	
	lvitem.mask = LVIF_TEXT | LVIF_PARAM;
	
	lvitem.cchTextMax = 0;
    lvitem.iItem = Item;
    lvitem.iSubItem = 0;
    lvitem.stateMask = 0;
    lvitem.state = 0;
    lvitem.iIndent = 0;
    lvitem.pszText = item_text;
    lvitem.lParam = 0; 

	sprintf (item_text, "%d", Item);
	
    Item = SendMessage (trades_list_view, LVM_INSERTITEM, (WPARAM) 0, (LPARAM)&lvitem);
    if (Item == -1) 
		return;
    else
        lvitem.iItem = Item;
	
    lvitem.mask = LVIF_TEXT;
    lvitem.iSubItem++;
	
	sprintf (item_text, "%ld", nMode);
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%.0lf", dNumber);
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%.0lf", dOrderNumber);
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", ClassCode ? ClassCode : "");
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", SecCode ? SecCode : "");
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%.0lf", dPrice);
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%ld", nQty);
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%.0lf", dValue);
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%ld", nIsSell);
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%ld", TRANS2QUIK_TRADE_SETTLE_DATE(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%ld", TRANS2QUIK_TRADE_DATE(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%ld", TRANS2QUIK_TRADE_TIME(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%ld", TRANS2QUIK_TRADE_IS_MARGINAL(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%lf", TRANS2QUIK_TRADE_ACCRUED_INT(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%lf", TRANS2QUIK_TRADE_YIELD(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%lf", TRANS2QUIK_TRADE_CLEARING_CENTER_COMMISSION(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%lf", TRANS2QUIK_TRADE_EXCHANGE_COMMISSION(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%lf", TRANS2QUIK_TRADE_TS_COMMISSION(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%lf", TRANS2QUIK_TRADE_TRADING_SYSTEM_COMMISSION(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%lf", TRANS2QUIK_TRADE_PRICE2(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%lf", TRANS2QUIK_TRADE_REPO_RATE(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%lf", TRANS2QUIK_TRADE_REPO2_VALUE(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%lf", TRANS2QUIK_TRADE_ACCRUED_INT2(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%ld", TRANS2QUIK_TRADE_REPO_TERM(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%lf", TRANS2QUIK_TRADE_START_DISCOUNT(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%lf", TRANS2QUIK_TRADE_LOWER_DISCOUNT(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%lf", TRANS2QUIK_TRADE_UPPER_DISCOUNT(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%ld", TRANS2QUIK_TRADE_BLOCK_SECURITIES(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", TRANS2QUIK_TRADE_CURRENCY(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", TRANS2QUIK_TRADE_SETTLE_CURRENCY(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", TRANS2QUIK_TRADE_SETTLE_CODE(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", TRANS2QUIK_TRADE_ACCOUNT(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", TRANS2QUIK_TRADE_BROKERREF(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", TRANS2QUIK_TRADE_CLIENT_CODE(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", TRANS2QUIK_TRADE_USERID(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", TRANS2QUIK_TRADE_FIRMID(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", TRANS2QUIK_TRADE_PARTNER_FIRMID(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", TRANS2QUIK_TRADE_EXCHANGE_CODE(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;

	sprintf (item_text, "%s", TRANS2QUIK_TRADE_STATION_ID(nTradeDescriptor));
	SendMessage (trades_list_view, LVM_SETITEMTEXT, (WPARAM) Item, (LPARAM)&lvitem);
    lvitem.iSubItem++;
}

void CAPI_Tester_DLGDlg::OnTradesStart()
{
	trades_list_view = ::GetDlgItem(m_hWnd, IDC_LIST_TRADES);

	if (g_bTradesUnsubscribed)
	{
		g_bTradesUnsubscribed = FALSE;
		::SendMessage (trades_list_view, LVM_DELETEALLITEMS, 0, 0);
	}

	long nRet = TRANS2QUIK_START_TRADES (TRADE_STATUS_CALLBACK);

	if (nRet != TRANS2QUIK_SUCCESS)
		MsgBox (Trans2QuikResultToStr(nRet).c_str(), "Attention", MB_ICONINFORMATION);	
}

UINT ASyncTransThread (LPVOID pvParam)
{
	char szMessage [1024] = "";
	SSyncTransThreadParam *p = (SSyncTransThreadParam*)pvParam;
	if (!p)
		return 0;
	
	char *pszTransaction = p->pszTransaction;
	
	if (!pszTransaction)
		return 0;
	
//	long nReturnCode;
//	DWORD dwTransId;
//	double dOrderNum;
//	char szResultMessage[1024];
	long nExtendedErrorCode;
	char szErrorMessage[1024];
//	long nResult;
	
	while (true)
	{
		// TODO: Add your control notification handler code here
		int m_nResult = TRANS2QUIK_SEND_ASYNC_TRANSACTION (pszTransaction, &nExtendedErrorCode, szErrorMessage, sizeof (szErrorMessage));
		
		Sleep(100);	
	}
	
	return 1;
	
}


void CAPI_Tester_DLGDlg::OnSetCallbacks() 
{
	// TODO: Add your control notification handler code here
	m_nResult = TRANS2QUIK_SET_CONNECTION_STATUS_CALLBACK (TRANS2QUIK_ConnectionStatusCallback, &m_nExtendedErrorCode, m_szErrorMessage, sizeof (m_szErrorMessage));
	
	if (m_nResult != TRANS2QUIK_SUCCESS)
		MsgBox ("Error", m_szErrorMessage, MB_ICONINFORMATION);
	
	m_nResult = TRANS2QUIK_SET_TRANSACTIONS_REPLY_CALLBACK (TRANS2QUIK_TransactionsReplyCallback, &m_nExtendedErrorCode, m_szErrorMessage, sizeof (m_szErrorMessage));
	
	if (m_nResult != TRANS2QUIK_SUCCESS)
		MsgBox ("Error", m_szErrorMessage, MB_ICONINFORMATION);
/*	
	if (m_nResult == TRANS2QUIK_SUCCESS)
	{
		TRANS2QUIK_SUBSCRIBE_ORDERS("", "");
		orders_list_view = ::GetDlgItem(m_hWnd, IDC_LIST_ORDERS);
		
		if (g_bOrdersUnsubscribed)
		{
			g_bOrdersUnsubscribed = FALSE;
			::SendMessage (orders_list_view, LVM_DELETEALLITEMS, 0, 0);
		}
		
		long nRet = TRANS2QUIK_START_ORDERS (ORDER_STATUS_CALLBACK);



		TRANS2QUIK_SUBSCRIBE_TRADES("", "");
		trades_list_view = ::GetDlgItem(m_hWnd, IDC_LIST_TRADES);
		
		if (g_bTradesUnsubscribed)
		{
			g_bTradesUnsubscribed = FALSE;
			::SendMessage (trades_list_view, LVM_DELETEALLITEMS, 0, 0);
		}
		
		nRet = TRANS2QUIK_START_TRADES (TRADE_STATUS_CALLBACK);


		char szTransaction [1024] = "";
		
		::GetDlgItemText (m_hWnd, IDC_TRANSACTION_BODY, szTransaction, sizeof (szTransaction));
		
		replace_char (szTransaction, '\r', ' ');
		replace_char (szTransaction, '\n', ' ');
		
		char *pszTransaction = new char[strlen(szTransaction)+1];
		strcpy (pszTransaction, szTransaction);
		SSyncTransThreadParam *p = new SSyncTransThreadParam();
		p->pszTransaction = pszTransaction;
		p->hwndButton = ::GetDlgItem(m_hWnd, IDC_SEND_SYNC_TRANS);
		
		AfxBeginThread(ASyncTransThread,p);
		return;
		

		for (int i=0; i < 100; ++i)
		{
			char szTransaction[1024];
			::GetDlgItemText (m_hWnd, IDC_TRANSACTION_BODY, szTransaction, sizeof (szTransaction));
			
			replace_char (szTransaction, '\r', ' ');
			replace_char (szTransaction, '\n', ' ');
			
			// TODO: Add your control notification handler code here
			m_nResult = TRANS2QUIK_SEND_ASYNC_TRANSACTION (szTransaction, &m_nExtendedErrorCode, m_szErrorMessage, sizeof (m_szErrorMessage));
			
			Sleep(1000);
			this->UpdateWindow();
		}
	}
*/
}
