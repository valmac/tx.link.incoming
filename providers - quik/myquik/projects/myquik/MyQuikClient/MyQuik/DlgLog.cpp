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
 *  DlgLog.cpp : implementation file
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "DlgLog.h"
#include "dlg.h"
#include <color.h>

//
// class CMyListBoxLog
//

CMyListBoxLog::CMyListBoxLog()
{
}

void CMyListBoxLog::OnRButtonUp(UINT nFlags, CPoint point)
{
	CMenu root;
	root.LoadMenu(IDR_MENU_LOG);
	CMenu menu = root.GetSubMenu(0);

	GetCursorPos(&point);
	CWindow wnd = GetParent();
	menu.TrackPopupMenu(TPM_LEFTALIGN |TPM_RIGHTBUTTON, point.x, point.y, wnd);
}

void CMyListBoxLog::DrawItem(int nIDCtl, LPDRAWITEMSTRUCT lpDrawItemStruct)
{
	int iItem = lpDrawItemStruct->itemID;
	if (iItem < 0)
	   return;

	TSTRING_STD2(str, size);
	this->GetText(iItem, str);
	LPCTSTR lpszText = str;// lpDrawItemStruct->itemData;
	CDC dc;

	dc.Attach(lpDrawItemStruct->hDC);

	// Save these value to restore them when done drawing.
	COLORREF crOldTextColor = dc.GetTextColor();
	COLORREF crOldBkColor = dc.GetBkColor();
	COLORREF colorHighlight, colorInactive;
	COLORREF color;
	double opacity;

	GetHighlightParams(*this, colorHighlight, colorInactive, colorHighlight, opacity);

	int code = this->GetItemData(iItem);
	if (code < 0)
	{
		if (code == CODE_ERROR)
			color = RGB(192, 0, 32);
		else if (code == CODE_USER_ERROR)
			color = RGB(128, 0, 103);
		else //if (code == CODE_USER_WARNING)
			color = RGB(0, 39, 128);
	}
	else if (code > 0)
	{
		color = RGB(0, 98, 95);
	}
	else
		color = RGB(20, 20, 20);

	dc.SetBkMode(TRANSPARENT);
	// If this item is selected, set the background color and the text color to appropriate values. 
	// Also, erase rect by filling it with the background color.
	if ((lpDrawItemStruct->itemAction | (ODA_SELECT|ODA_FOCUS)) &&
	  (lpDrawItemStruct->itemState & (ODS_SELECTED|ODS_FOCUS)))
	{
		COLORREF background;
		background = my::lib::SumColorsAlpha(crOldBkColor, colorHighlight, opacity);
		dc.FillSolidRect(&lpDrawItemStruct->rcItem, background);
	}
	else
		dc.FillSolidRect(&lpDrawItemStruct->rcItem, crOldBkColor);
#if 0
	// If this item has the focus, draw a red frame around the item's rect.
	if ((lpDrawItemStruct->itemAction | ODA_FOCUS) && (lpDrawItemStruct->itemState & ODS_FOCUS))
	{
		CBrush br;
		br.CreateSolidBrush(colorHighlight);
		dc.FrameRect(&lpDrawItemStruct->rcItem, br);
	}
#endif
	// Draw the text.
	CRect rect(lpDrawItemStruct->rcItem);
	rect.left += 1;
	dc.SetTextColor(color);
	dc.DrawText(lpszText, (int)_tcslen(lpszText), &rect, DT_LEFT|DT_SINGLELINE|DT_VCENTER);

	// Reset the background color and the text color back to their original values.
	dc.SetTextColor(crOldTextColor);
	dc.SetBkColor(crOldBkColor);

	dc.Detach();
}


//
// CDlgLog dialog
//

CDlgLog::CDlgLog()
{
	m_hLog = INVALID_HANDLE_VALUE;
}

CDlgLog::~CDlgLog()
{
	::CloseHandle(m_hLog);
}

void CDlgLog::Resize()
{
	if (::IsWindow(m_listBox))
	{
		CRect rect;
		GetClientRect(rect);
		m_listBox.MoveWindow(rect.left, rect.top, rect.Width(), rect.Height());

		UpdateHorizontalExtent();
	}
}

void CDlgLog::UpdateHorizontalExtent()
{
	// Find the longest string in the list box.
	TSTRING_STD(str);
	CSize sz;
	int dx = 0;
	TEXTMETRIC tm;
	CDC dc = m_listBox.GetDC();
	CFont font, oldFont;
#if 0
	LOGFONT lf;
	memset(&lf, 0, sizeof(lf));
	font.CreateFontIndirect(&lf);
#else
	font.Attach(m_listBox.GetFont());
#endif	
#if 1
	oldFont = dc.SelectFont(font);
#else
	oldFont = dc.SelectStockFont(DEFAULT_GUI_FONT);
#endif
	// Get the text metrics for avg char width
	dc.GetTextMetrics(&tm); 

	for (int i = 0; i < m_listBox.GetCount(); i++)
	{
	   m_listBox.GetText(i, str);
	   dc.GetTextExtent(str, lstrlen(str), &sz);

	   // Add the avg width to prevent clipping
	   sz.cx += tm.tmAveCharWidth;

	   if (sz.cx > dx)
		  dx = sz.cx;
	}

	dc.SelectFont(oldFont);

	font.Detach();
	m_listBox.ReleaseDC(dc);

	// Set the horizontal extent so every character of all strings can be scrolled to.
	m_listBox.SetHorizontalExtent(dx);
}

void CDlgLog::PutMessage(LPCTSTR msg, int code)
{
	CListBox listBox(GetDlgItem(IDC_LIST1));
	if (msg && lstrlen(msg) > 0)
	{
		SYSTEMTIME time;
		::GetLocalTime(&time);

		TSTRING_SMALL(buf);
		TSTRING_STD(strFile);
		TSTRING_STD(strMsg);
		int n;
		n = _stprintf_s(buf, SIZEOF_ARRAY(buf), TEXT("[%02d:%02d:%02d]"), time.wHour, time.wMinute, time.wSecond);
		TCHAR sign;
		int code2 = code;
		if (code < 0)
		{
			sign = TEXT('-');
			code2 = -code;
		}
		else if (code > 0)
			sign = TEXT('+');
		else
			sign = TEXT(' ');
		n = _stprintf_s(strFile, SIZEOF_ARRAY(strFile), TEXT("[%c%d]%s %s\r\n"), sign, code2, buf, msg);
		_stprintf_s(strMsg, SIZEOF_ARRAY(strFile), TEXT("%s %s"), buf, msg);

		// Запись в файл:
		if (theApp.m_settings.log.saveOnDisk)
		{
			if (m_hLog != INVALID_HANDLE_VALUE)
			{
				DWORD nrBytesWritten;
				USES_CONVERSION;
				LPCSTR data = T2A(strFile);
				::WriteFile(m_hLog, data, n, &nrBytesWritten, NULL);
			}
		}

		// Вывод в окно:
		if (CB_ERR != listBox.InsertString(-1, strMsg))
		{
			listBox.SetItemData(listBox.GetCount() - 1, static_cast<DWORD_PTR>(code));
			UpdateHorizontalExtent();
		}
	}
}

// CDlgLog message handlers

void CDlgLog::OnSize(UINT nType, CSize size)
{
	Resize();
}

BOOL CDlgLog::OnInitDialog(CWindow wndFocus, LPARAM lInitParam)
{
	DoDataExchange(false);
	my::DialogMessageHook::InstallHook(*this);

	HICON hIcon = theApp.LoadSmallIcon(IDI_OUTPUT);
	SetIcon(hIcon, FALSE);

	if (theApp.m_settings.log.saveOnDisk)
	{
		m_hLog = ::CreateFile(theApp.GetSettings().common.path.log, 
			GENERIC_WRITE, FILE_SHARE_READ, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL); 
	}
	Resize();

	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX Property Pages should return FALSE
}

void CDlgLog::OnCancel(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	::PostMessage(GetParent(), UM_CLOSE_WINDOW, this->IDD, 0);
}

void CDlgLog::OnLogClose(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnCancel(uNotifyCode, nID, wndCtl);
}

LRESULT CDlgLog::OnClose(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	if (lParam == 0)
		OnCancel(0, 0, NULL);
	else
		bHandled = TRUE;
	return 0;
}

void CDlgLog::OnDestroy()
{
	my::DialogMessageHook::UninstallHook(*this);
}

LRESULT CDlgLog::OnWndMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = TRUE;
	switch (uMsg)
	{
	case UM_MENU:
		{
			int what = (int)wParam;
			if (what == F_SHOW_MENU)
				m_listBox.SendMessage(WM_RBUTTONUP);
		}
		break;
	default:
		bHandled = FALSE;
	} // switch (uMsg)

	return 0;
}

void CDlgLog::OnActivate(UINT nState, BOOL bMinimized, CWindow wndOther)
{
	::SendMessage(GetParent(), UM_ACTIVE, (WPARAM)this->m_hWnd, (LPARAM)nState);
}
