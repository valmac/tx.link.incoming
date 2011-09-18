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
 *  DlgAbout.h
 */

#pragma once

#if USE_ABOUT_TABS
#include "PageAboutDescription.h"
#include "PageAboutLicence.h"
#endif

//
// CDlgAbout dialog
//
class CDlgAbout : public CDialogImpl<CDlgAbout>
{
public:

	enum { IDD = IDD_ABOUTBOX };

	static LPCTSTR GetName() { return TEXT("DlgAbout"); };

protected:
	CFont m_headerFont;
#if USE_ABOUT_TABS
	CPageAboutDescription m_pageDescription;
	CPageAboutLicence m_pageLicence;
	CTabCtrl m_tabCtrl;
#endif

protected:
	void SavePosition();
	
	LPCTSTR GetLicenceText();

	void OpenLink(PNMLINK pNMLink);

	BEGIN_MSG_MAP_EX(CTradeDlg)
		MSG_WM_INITDIALOG(OnInitDialog)
		MSG_WM_CLOSE(OnClose)
		COMMAND_ID_HANDLER_EX(IDOK, OnOK)
		COMMAND_ID_HANDLER_EX(IDCANCEL, OnCancel)
		NOTIFY_HANDLER_EX(IDC_SYSLINK1, NM_CLICK, OnNMClickSyslink1)
		NOTIFY_HANDLER_EX(IDC_SYSLINK2, NM_CLICK, OnNMClickSyslink2)
		NOTIFY_HANDLER_EX(IDC_SYSLINK3, NM_CLICK, OnNMClickSyslink3)
	END_MSG_MAP()

	BOOL OnInitDialog(CWindow wndFocus, LPARAM lInitParam);
	void OnClose();
	void OnOK(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnCancel(UINT uNotifyCode, int nID, CWindow wndCtl);

	LRESULT OnNMClickSyslink1(LPNMHDR pnmh);
	LRESULT OnNMClickSyslink2(LPNMHDR pnmh);
	LRESULT OnNMClickSyslink3(LPNMHDR pnmh);
};
