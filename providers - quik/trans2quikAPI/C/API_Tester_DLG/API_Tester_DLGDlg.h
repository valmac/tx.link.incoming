// API_Tester_DLGDlg.h : header file
//

#if !defined(AFX_API_TESTER_DLGDLG_H__4CCEA28C_7970_43F1_A503_A0E7F6E30C71__INCLUDED_)
#define AFX_API_TESTER_DLGDLG_H__4CCEA28C_7970_43F1_A503_A0E7F6E30C71__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CAPI_Tester_DLGDlg dialog

class CAPI_Tester_DLGDlg : public CDialog
{
// Construction
public:
	CAPI_Tester_DLGDlg(CWnd* pParent = NULL);	// standard constructor
	int MsgBox (LPCTSTR lpszText, LPCTSTR lpszCaption = "Error", UINT nType = MB_OK|MB_ICONHAND); 

// Dialog Data
	//{{AFX_DATA(CAPI_Tester_DLGDlg)
	enum { IDD = IDD_API_TESTER_DLG_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAPI_Tester_DLGDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	HICON	m_hIcon;
	long	m_nDLLConnectionStatus;
	long	m_nQUIKConnectionStatus;
	long	m_nExtendedErrorCode;
	long	m_nResult;
	char	m_szErrorMessage [1024];
	long	m_nReturnCode;
	DWORD	m_dwTransId;
	double	m_dOrderNum;
	char	m_szResultMessage [1024];
	bool	m_bShowMessages;

	// Generated message map functions
	//{{AFX_MSG(CAPI_Tester_DLGDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnConnect();
	afx_msg void OnIsDllConnected();
	afx_msg void OnIsQuikConnected();
	afx_msg void OnSetCallbacks();
	afx_msg void OnSyncTrans();
	afx_msg void OnDisconnect();
	afx_msg void OnSendAsyncTrans();
	afx_msg void OnShowMessages();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_API_TESTER_DLGDLG_H__4CCEA28C_7970_43F1_A503_A0E7F6E30C71__INCLUDED_)
