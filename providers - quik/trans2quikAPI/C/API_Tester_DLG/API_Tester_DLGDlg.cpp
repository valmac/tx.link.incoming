// API_Tester_DLGDlg.cpp : implementation file
//

#include "stdafx.h"
#include "API_Tester_DLG.h"
#include "API_Tester_DLGDlg.h"

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

void CAPI_Tester_DLGDlg::OnSetCallbacks() 
{
	// TODO: Add your control notification handler code here
	m_nResult = TRANS2QUIK_SET_CONNECTION_STATUS_CALLBACK (TRANS2QUIK_ConnectionStatusCallback, &m_nExtendedErrorCode, m_szErrorMessage, sizeof (m_szErrorMessage));

	if (m_nResult != TRANS2QUIK_SUCCESS)
		MsgBox ("Error", m_szErrorMessage, MB_ICONINFORMATION);

	m_nResult = TRANS2QUIK_SET_TRANSACTIONS_REPLY_CALLBACK (TRANS2QUIK_TransactionsReplyCallback, &m_nExtendedErrorCode, m_szErrorMessage, sizeof (m_szErrorMessage));

	if (m_nResult != TRANS2QUIK_SUCCESS)
		MsgBox ("Error", m_szErrorMessage, MB_ICONINFORMATION);
}

void CAPI_Tester_DLGDlg::OnSyncTrans() 
{
	// TODO: Add your control notification handler code here
	char szMessage [1024] = "";
	char szTransaction [1024] = "";

	::GetDlgItemText (m_hWnd, IDC_TRANSACTION_BODY, szTransaction, sizeof (szTransaction));
 
	replace_char (szTransaction, '\r', ' ');
	replace_char (szTransaction, '\n', ' ');

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

	if (m_nResult != TRANS2QUIK_SUCCESS)
		MsgBox (m_szErrorMessage, "Error", MB_ICONHAND);
	else
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
