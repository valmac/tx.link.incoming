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
 *  DlgAbout.cpp : implementation file
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "DlgAbout.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

//
// CDlgAbout dialog
//

BOOL CDlgAbout::OnInitDialog(CWindow wndFocus, LPARAM lInitParam)
{
	CREATESTRUCT cs;
	if (S_OK == theApp.LoadWindowPosition(&cs, GetName()))
		MoveWindow(cs.x, cs.y, cs.cx, cs.cy);

	HICON hIcon = CMyQuikApp::LoadIconEx(theApp.GetModuleInstance(), IDR_MAINFRAME, 16);
	SetIcon(hIcon, FALSE);

	CString text;
	LPCTSTR appName = theApp.GetAppName();
	LPCTSTR productName = theApp.GetProductName();
	int build = theApp.GetProductBuild();
	SYSTEMTIME time;
	theApp.GetProductTime(time);
#if 1
	text.Format(TEXT("О программе %s"), appName);
#else
	text.Format(TEXT("О программе"));
#endif
	SetWindowText(text);

	// Настраиваем шрифт:
	CWindow wnd = GetDlgItem(IDC_HEADER);
	HFONT hFont = wnd.GetFont();

	CLogFont lfont(hFont);
	lfont.lfHeight = lfont.lfHeight * 5/4;
	lfont.lfWeight = FW_BOLD;

	m_headerFont.CreateFontIndirect(&lfont);
	wnd.SetFont(m_headerFont);

	text.Format(TEXT("%s, версия %s"), appName, productName);
	wnd.SetWindowText(text);

	CEdit edit = (GetDlgItem(IDC_EDIT1));
	ASSERT(edit);
	edit.EnableWindow(TRUE);
#if 0
	edit.SetSel(0, -1, TRUE);
	edit.ReplaceSel(GetLicenceText());
#else
	edit.SetWindowText(GetLicenceText());
#endif

	wnd = GetDlgItem(IDC_STATIC_BUILD);
#if 1
	text.Format(TEXT("Сборка %d"), build);
#else
	text.Format(TEXT("Сборка"));
#endif
	wnd.SetWindowText(text);
#if 1
	wnd = GetDlgItem(IDC_STATIC_BUILD2);
	text.Format(TEXT("%02d.%02d.%02d %02d:%02d:%02d"), time.wDay, time.wMonth, time.wYear, time.wHour, time.wMinute, time.wSecond);
	wnd.SetWindowText(text);
#endif

	int show;
	wnd = GetDlgItem(IDC_SYSLINK2);
#if 0//SHOW_PRODUCT_MAIL
	show = SW_SHOW;
#else
	show = SW_HIDE;
#endif
	wnd.ShowWindow(show);

#if USE_ABOUT_TABS
	CDialog * pPage;
	TCITEM ti;
	pPage = &m_pageDescription;
	ti.mask = TCIF_PARAM|TCIF_TEXT;
  	ti.lParam = (LPARAM)pPage;	
	ti.pszText = TEXT("Описание");
	m_tabCtrl.InsertItem(0, &ti);
	pPage->Create(CPageAboutDescription::IDD, &m_tabCtrl);
	pPage->SetWindowPos(NULL, 50, 30, 0, 0, SWP_NOSIZE | SWP_NOZORDER);
  	pPage->ShowWindow(SW_SHOW);
	pPage = &m_pageLicence;
	ti.mask = TCIF_PARAM|TCIF_TEXT;
  	ti.lParam = (LPARAM)pPage;	
	ti.pszText = TEXT("Лицензия");
	m_tabCtrl.InsertItem(1, &ti);
	//pPage->Create(CPageAboutLicence::IDD, &m_tabCtrl);
#endif // USE_ABOUT_TABS

	return TRUE;
}

void CDlgAbout::SavePosition()
{
	theApp.SaveWindowPosition(this->m_hWnd, GetName());
}

LPCTSTR CDlgAbout::GetLicenceText()
{
	static LPCTSTR text = TEXT("MyQuik является свободным программным обеспечением; вы можете распространять и/или изменять его согласно условиям Стандартной Общественной Лицензии GNU (GNU GPL), опубликованной Фондом свободного программного обеспечения (FSF), либо Лицензии версии 3, либо (на ваше усмотрение) любой более поздней версии.\r\n\r\nПрограмма распространяется в надежде, что она будет полезной, но БЕЗ КАКИХ БЫ ТО НИ БЫЛО ГАРАНТИЙНЫХ ОБЯЗАТЕЛЬСТВ; даже без косвенных гарантийных обязательств, связанных с ПОТРЕБИТЕЛЬСКИМИ СВОЙСТВАМИ и ПРИГОДНОСТЬЮ ДЛЯ ОПРЕДЕЛЕННЫХ ЦЕЛЕЙ. Для подробностей смотрите Стандартную Общественную Лицензию GNU.\r\n\r\nВы должны были получить копию Стандартной Общественной Лицензии GNU вместе с этой программой. Если это не так, посетите сайт http://www.gnu.org/licenses/");
	return text;
}

// CDlgAbout message handlers

void CDlgAbout::OnClose()
{
	SavePosition();
	EndDialog(IDCANCEL);
}

void CDlgAbout::OnOK(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	SavePosition();
	EndDialog(nID);
}

void CDlgAbout::OnCancel(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	SavePosition();
	EndDialog(nID);
}

LRESULT CDlgAbout::OnNMClickSyslink1(LPNMHDR pnmh)
{
	PNMLINK pNMLink = (PNMLINK) pnmh;
	OpenLink(pNMLink);
	return 0;
}

LRESULT CDlgAbout::OnNMClickSyslink2(LPNMHDR pnmh)
{
	PNMLINK pNMLink = (PNMLINK) pnmh;
	OpenLink(pNMLink);
	return 0;
}

LRESULT CDlgAbout::OnNMClickSyslink3(LPNMHDR pnmh)
{
	PNMLINK pNMLink = (PNMLINK) pnmh;
	OpenLink(pNMLink);
	return 0;
}

void CDlgAbout::OpenLink(PNMLINK pNMLink)
{
	HINSTANCE hInst = ::ShellExecute(NULL, TEXT("open"), pNMLink->item.szUrl, NULL, NULL, SW_SHOW);
}
