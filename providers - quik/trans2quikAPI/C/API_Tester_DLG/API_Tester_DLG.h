// API_Tester_DLG.h : main header file for the API_TESTER_DLG application
//

#if !defined(AFX_API_TESTER_DLG_H__EE9B6087_6487_4AC4_950A_23982639FC1F__INCLUDED_)
#define AFX_API_TESTER_DLG_H__EE9B6087_6487_4AC4_950A_23982639FC1F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CAPI_Tester_DLGApp:
// See API_Tester_DLG.cpp for the implementation of this class
//

class CAPI_Tester_DLGApp : public CWinApp
{
public:
	CAPI_Tester_DLGApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAPI_Tester_DLGApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CAPI_Tester_DLGApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_API_TESTER_DLG_H__EE9B6087_6487_4AC4_950A_23982639FC1F__INCLUDED_)
