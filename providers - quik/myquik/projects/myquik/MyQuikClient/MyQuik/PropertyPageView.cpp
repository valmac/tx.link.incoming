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
 *  PropertyPageView.cpp : implementation file
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "PropertyPageView.h"

//
// CPropertyPageView dialog
//
CPropertyPageView::CPropertyPageView(const ::Settings::Presentation & presentation)
{
	m_initialized = FALSE;
	m_updating = FALSE;
	m_settings = presentation;
}

CPropertyPageView::~CPropertyPageView()
{
}

void CPropertyPageView::UpdateSettings(int set)
{
	UpdatePresentation(set);
	UpdatePresentationDeals(set);
	UpdateMargins(set);
	UpdatePlacement(set);
	UpdateFlashing(set);
}

void CPropertyPageView::UpdatePresentation(int set, int index)
{	
	Settings::Presentation::Window * pWindow;
	if (index == -1)
		index = GetCurrentWindow(&pWindow);
	else
		pWindow = (Settings::Presentation::Window*)m_listWindows.GetItemData(index);
	if (index >= 0)
	{		
		CButton btn = GetDlgItem(IDC_CHECKBOX_SHOW_TITLE);
		BOOL visible = m_listWindows.GetCheckState(index);
		if (set)
		{			
			TSTRING_SMALL2(str, size);
			m_listWindows.GetItemText(index, 1, str, size);
			CWindow wnd = GetDlgItem(IDC_STATIC_WINDOW);
			wnd.SetWindowText(str);

			btn.EnableWindow((index != I_WINDOW_STOP) ? visible : FALSE);
			btn.SetCheck((pWindow->flags & F_SHOW_TITLE) ? BST_CHECKED : BST_UNCHECKED);
		}
		else
		{
			pWindow->flags = visible ? F_SHOW_WINDOW : 0;
			if (btn.GetCheck() == BST_CHECKED)
				pWindow->flags |= F_SHOW_TITLE;
		}

		UpdateOpacity(set, pWindow, visible);
	}
}

void CPropertyPageView::UpdateOpacity(int set, Settings::Presentation::Window * pWindow, BOOL visible)
{
	if (! pWindow)
		return;

	TSTRING_SMALL2(str, size);
	CWindow wnd, wnd2;
	wnd = GetDlgItem(IDC_EDIT_OPACITY);
	wnd2 = GetDlgItem(IDC_EDIT_OPACITY2);
	if (set)
	{		
		CWindow wndStatic;
		wndStatic = GetDlgItem(IDC_STATIC_OPACITY);
		wndStatic.EnableWindow(visible);
		wndStatic = GetDlgItem(IDC_STATIC_OPACITY_INACTIVE);
		wndStatic.EnableWindow(visible);
		wndStatic = GetDlgItem(IDC_STATIC_OPACITY_ACTIVE);
		wndStatic.EnableWindow(visible);
		wnd.EnableWindow(visible);
		wnd2.EnableWindow(visible);

		m_updating = TRUE;
		_stprintf_s(str, size, TEXT("%d"), pWindow->opacity.inactive);
		wnd.SetWindowText(str);
		_stprintf_s(str, size, TEXT("%d"), pWindow->opacity.active);
		wnd2.SetWindowText(str);
		m_updating = FALSE;
	}
	else
	{				
		int val;
		wnd.GetWindowText(str, size);
		val = StrToInt(str);
		if (val > 100)
			val = 100;
		pWindow->opacity.inactive = val;
		wnd2.GetWindowText(str, size);
		val = StrToInt(str);
		if (val > 100)
			val = 100;
		pWindow->opacity.active = val;		
	}
}

void CPropertyPageView::UpdatePresentationDeals(int set)
{
	DWORD & flagsWnd = m_settings.main.show.wnd;
	DWORD & flagsFrame = m_settings.main.show.frame;
	DWORD mask;

	if (! set)
		flagsWnd = flagsFrame = 0;

	CButton btn;	
	mask = F_MAIN_SHOW_LIST;
	btn = GetDlgItem(IDC_CHECK_SHOW_LISTOFDEALS);
	if (set)
		btn.SetCheck((flagsWnd & mask) ? BST_CHECKED : BST_UNCHECKED);
	else if (btn.GetCheck() == BST_CHECKED)
		flagsWnd |= mask;
	btn = GetDlgItem(IDC_CHECK_SHOW_FRAME_LISTOFDEALS);
	if (set)
		btn.SetCheck((flagsFrame & mask) ? BST_CHECKED : BST_UNCHECKED);
	else if (btn.GetCheck() == BST_CHECKED)
		flagsFrame |= mask;
	mask = F_MAIN_SHOW_OSD;
	btn = GetDlgItem(IDC_CHECK_SHOW_OSD);
	if (set)
		btn.SetCheck((flagsWnd & mask) ? BST_CHECKED : BST_UNCHECKED);
	else if (btn.GetCheck() == BST_CHECKED)
		flagsWnd |= mask;
	btn = GetDlgItem(IDC_CHECK_SHOW_FRAME_OSD);
	if (set)
		btn.SetCheck((flagsFrame & mask) ? BST_CHECKED : BST_UNCHECKED);
	else if (btn.GetCheck() == BST_CHECKED)
		flagsFrame |= mask;
	mask = F_MAIN_SHOW_CONTROLS;
	btn = GetDlgItem(IDC_CHECK_SHOW_CONTROLS);
	if (set)
		btn.SetCheck((flagsWnd & mask) ? BST_CHECKED : BST_UNCHECKED);
	else if (btn.GetCheck() == BST_CHECKED)
		flagsWnd |= mask;
	btn = GetDlgItem(IDC_CHECK_SHOW_FRAME_CONTROLS);
	if (set)
		btn.SetCheck((flagsFrame & mask) ? BST_CHECKED : BST_UNCHECKED);
	else if (btn.GetCheck() == BST_CHECKED)
		flagsFrame |= mask;
}

void CPropertyPageView::UpdateMargins(int set)
{
	TSTRING_SMALL2(str, size);
	CWindow wnd;
	wnd = GetDlgItem(IDC_EDIT_MARGINS);
	if (set)
	{		
		wsprintf(str, TEXT("%d"), m_settings.main.margins);
		m_updating = TRUE;
		wnd.SetWindowText(str);
		m_updating = FALSE;
	}
	else
	{
		wnd.GetWindowText(str, size);
		int val = StrToInt(str);
		if (val < -1)
			val = -1;
		m_settings.main.margins = val;
	}
}

void CPropertyPageView::UpdatePlacement(int set)
{
	DWORD & flags = m_settings.main.flags;
	DWORD mask = F_VIEW_ALLWAYS_ON_TOP;
	CButton btn = GetDlgItem(IDC_CHECK_ALLWAYS_ON_TOP);
	if (set)
		btn.SetCheck((flags & mask) ? BST_CHECKED : BST_UNCHECKED);
	else 
	{
		if (btn.GetCheck() == BST_CHECKED)
			flags |= mask;
		else
			flags &= ~mask;
	}
}

void CPropertyPageView::UpdateFlashing(int set)
{
	DWORD & flags = m_settings.main.flags;
	DWORD mask = F_VIEW_ENABLE_FLASHING;
	CButton btn = GetDlgItem(IDC_CHECKBOX_ENABLE_FLASHING);
	if (set)
		btn.SetCheck((flags & mask) ? BST_CHECKED : BST_UNCHECKED);
	else 
	{
		if (btn.GetCheck() == BST_CHECKED)
			flags |= mask;
		else
			flags &= ~mask;
	}
}

void CPropertyPageView::EnableCheckBoxTitleBar(UINT id, BOOL enable)
{
	CButton btn = GetDlgItem(id);
	btn.EnableWindow(enable);
}

void CPropertyPageView::EnableCheckBoxFrame(UINT id, BOOL enable)
{
	CButton btn = GetDlgItem(id);
	btn.EnableWindow(enable);
}

void CPropertyPageView::SetModified(BOOL bChanged)
{
	if (bChanged && m_initialized)
	{
		::SendMessage(theApp.GetMainWnd(), UM_APPLY_SETTINGS, APPLY_SETTINGS_PRESENTATION_MAIN, (LPARAM)&m_settings);
	}
	CPropertyPageImpl::SetModified(bChanged);
}

void CPropertyPageView::InitListWindows()
{
	m_listWindows = GetDlgItem(IDC_LIST_WINDOWS);

	CRect rect;
	m_listWindows.GetClientRect(rect);

	LPCTSTR name;
	int width;
	int format = LVCFMT_LEFT;

	width = 25;
	m_listWindows.InsertColumn(0, TEXT(""), format, width);
	rect.left += width;
	name = TEXT("Название"); 
	width = rect.Width();
	m_listWindows.InsertColumn(1, name, format, width);

	m_listWindows.SetExtendedListViewStyle(m_listWindows.GetExtendedListViewStyle()|
		LVS_EX_FULLROWSELECT|0|LVS_EX_DOUBLEBUFFER|LVS_EX_CHECKBOXES|
		0  ); // LVS_EX_BORDERSELECT LVS_EX_GRIDLINES LVS_EX_FULLROWSELECT


	LPCTSTR names[] = {
		TEXT("Окно сделок"),
		TEXT("Стакан заявок"),
		TEXT("История транзакций"),
		TEXT("Журнал сообщений"),
		TEXT("Окно ввода стоп-заявок"),
		NULL,
	};
	int i, iItem = 0;
	for (i = 0; (name = names[i]) != NULL; i++)
	{		
		m_listWindows.InsertItem(i, TEXT(""));
		m_listWindows.SetItemText(i, 1, name);
		Settings::Presentation::Window & window = m_settings.view.item[i];
		m_listWindows.SetItemData(i, (DWORD_PTR)&window);
		BOOL visible = window.flags & F_SHOW_WINDOW;
		m_listWindows.SetCheckState(i, visible);
	} // for (i)
	my::ctrl::SetFocusedItem(m_listWindows, 0, LVIS_SELECTED);

	::SetWindowTheme(m_listWindows, TEXT("explorer"), NULL);
}

void CPropertyPageView::InitListMain()
{
	m_listMain = GetDlgItem(IDC_LIST2);
#if 0
	CRect rect;
	m_listMain.GetClientRect(rect);

	LPCTSTR name;
	int width;
	int format = LVCFMT_LEFT;

	width = 25;
	m_listMain.InsertColumn(0, TEXT(""), format, width);
	rect.left += width;
	name = TEXT("Название"); 
	width = rect.Width();
	m_listMain.InsertColumn(1, name, format, width);

	m_listMain.SetExtendedListViewStyle(m_listMain.GetExtendedListViewStyle()|
		LVS_EX_FULLROWSELECT|0|LVS_EX_DOUBLEBUFFER|LVS_EX_CHECKBOXES|
		0  ); // LVS_EX_BORDERSELECT LVS_EX_GRIDLINES LVS_EX_FULLROWSELECT


	LPCTSTR names[] = {
		TEXT("Список сделок"),
		TEXT("Область информации"),
		TEXT("Элементы управления"),
	};
	int i, iItem = 0;
	for (i = 0; i < SIZEOF_ARRAY(names); i++)
	{		
		int index = i;
		name = names[i];
		m_listMain.InsertItem(i, TEXT(""));
		m_listMain.SetItemText(i, 1, name);
		m_listMain.SetItemData(i, (DWORD_PTR)index);
		BOOL visible = m_settings.view.item[index].flags & F_SHOW_TITLE;
		m_listMain.SetCheckState(i, visible);
	} // for (i)

	::SetWindowTheme(m_listMain, TEXT("explorer"), NULL);
#else
	m_listMain.ShowWindow(SW_HIDE);
	m_listMain.EnableWindow(FALSE);
#endif
}

int CPropertyPageView::GetCurrentWindowIndex()
{
	return my::ctrl::GetFocusedItem(m_listWindows);
}

int CPropertyPageView::GetCurrentWindow(Settings::Presentation::Window ** ppWindow)
{
	int i = my::ctrl::GetFocusedItem(m_listWindows);
	if (i >= 0)
		*ppWindow = (Settings::Presentation::Window*)m_listWindows.GetItemData(i);
	else
		*ppWindow = NULL;
	return i;
}

//
// CPropertyPageView message handlers
//

BOOL CPropertyPageView::OnInitDialog(CWindow wndFocus, LPARAM lInitParam)
{
	InitListWindows();
	InitListMain();

	UpdateSettings(SET);

	EnableCheckBoxFrame(IDC_CHECK_SHOW_FRAME_LISTOFDEALS, m_settings.main.show.wnd & F_MAIN_SHOW_LIST);
	EnableCheckBoxFrame(IDC_CHECK_SHOW_FRAME_OSD, m_settings.main.show.wnd & F_MAIN_SHOW_OSD);
	EnableCheckBoxFrame(IDC_CHECK_SHOW_FRAME_CONTROLS, m_settings.main.show.wnd & F_MAIN_SHOW_CONTROLS);

	m_initialized = TRUE;

	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX Property Pages should return FALSE
}

BOOL CPropertyPageView::OnQueryCancel()
{
	UpdateSettings(GET);
	::SendMessage(GetParent(), UM_CANCEL, this->IDD, (LPARAM)&m_settings);
	return TRUE;    // ok to cancel
}

BOOL CPropertyPageView::OnApply()
{
	UpdateSettings(GET);

	::SendMessage(GetParent(), UM_APPLY, this->IDD, (LPARAM)&m_settings);

	SetModified(FALSE);

	return CPropertyPageImpl<CPropertyPageView>::OnApply();
}

LRESULT CPropertyPageView::OnLvnItemchangedListWindows(NMHDR *pNMHDR)
{
	LPNMLISTVIEW pNMLV = reinterpret_cast<LPNMLISTVIEW>(pNMHDR);

	if (m_initialized)
	{
		int iItem = pNMLV->iItem;
		if ((pNMLV->uNewState == 4096 && pNMLV->uOldState == 8192) || (pNMLV->uNewState == 8192 && pNMLV->uOldState == 4096))
		{// Флажок:
			UpdatePresentation(GET, iItem);
			if (iItem == GetCurrentWindowIndex())
				UpdatePresentation(SET, iItem);
			SetModified();
			SetFocus();
		}
		else if (pNMLV->uNewState == (LVIS_SELECTED|LVIS_FOCUSED) && pNMLV->uOldState == 0)
		{		
			UpdatePresentation(SET, iItem);
		}
	}

	return 0;
}

void CPropertyPageView::OnBnClickedShowTitle(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdatePresentation(GET);
	SetModified();
	SetFocus();
}

void CPropertyPageView::OnBnClickedCheckAllwaysOnTop(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdatePlacement(GET);
	SetModified();
	SetFocus();
}

void CPropertyPageView::OnBnClickedCheckEnableFlashing(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateFlashing(GET);
	SetModified();
}

void CPropertyPageView::OnDeltaposSpinOpacity(LPNMUPDOWN pNMUpDown, UINT id)
{
	CWindow wnd = GetDlgItem(id);
	TSTRING_SMALL2(str, size);
	wnd.GetWindowText(str, size);
	int val = StrToInt(str);
	int prev = val;
	if (pNMUpDown->iDelta < 0)
	{
		if (val < 100)
			val++;
	}
	else
	{
		if (val > 0)
			val--;
	}

	wsprintf(str, TEXT("%d"), val);
	wnd.SetWindowText(str);
}

void CPropertyPageView::OnEnChangeEditOpacity(UINT id)
{
	if (! m_initialized || m_updating)
		return;

	Settings::Presentation::Window * pWindow;
	GetCurrentWindow(&pWindow);

	CWindow wnd = GetDlgItem(id);
#if 0
	TSTRING_SMALL2(str, size);
	wnd.GetWindowText(str, size);
	int val = StrToInt(str);
	int prev = val;
	if (val > 100)
		val = 100;
	else
		if (val < 0)
			val = 0;
	if (val != prev)
#endif
	{
		UpdateOpacity(GET, pWindow, pWindow->flags & F_SHOW_WINDOW);
		SetModified();
	}
}

LRESULT CPropertyPageView::OnDeltaposSpinOpacity(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);
	OnDeltaposSpinOpacity(pNMUpDown, IDC_EDIT_OPACITY);
	return 0;
}

LRESULT CPropertyPageView::OnDeltaposSpinOpacity2(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);
	OnDeltaposSpinOpacity(pNMUpDown, IDC_EDIT_OPACITY2);
	return 0;
}

void CPropertyPageView::OnEnChangeEditOpacity(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnEnChangeEditOpacity(IDC_EDIT_OPACITY);
}

void CPropertyPageView::OnEnChangeEditOpacity2(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnEnChangeEditOpacity(IDC_EDIT_OPACITY2);
}

void CPropertyPageView::OnEnChangeEditMargins(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	if (! m_initialized || m_updating)
		return;

	UpdateMargins(GET);
	SetModified();
}

LRESULT CPropertyPageView::OnDeltaposSpin2(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);

	CWindow wnd = GetDlgItem(IDC_EDIT_MARGINS);
	TSTRING_SMALL2(str, size);
	wnd.GetWindowText(str, size);
	int val = StrToInt(str);
	int prev = val;
	if (pNMUpDown->iDelta < 0)
		val++;
	else
	{
		if (val >= 0)
			val--;
	}

	wsprintf(str, TEXT("%d"), val);
	wnd.SetWindowText(str);

	return 0;
}

void CPropertyPageView::OnBnClickedCheckShowListofdeals(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdatePresentationDeals(GET);
	EnableCheckBoxFrame(IDC_CHECK_SHOW_FRAME_LISTOFDEALS, m_settings.main.show.wnd & F_MAIN_SHOW_LIST);
	SetModified();
}

void CPropertyPageView::OnBnClickedCheckShowOsd(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdatePresentationDeals(GET);
	EnableCheckBoxFrame(IDC_CHECK_SHOW_FRAME_OSD, m_settings.main.show.wnd & F_MAIN_SHOW_OSD);
	SetModified();
}

void CPropertyPageView::OnBnClickedCheckShowControls(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdatePresentationDeals(GET);
	EnableCheckBoxFrame(IDC_CHECK_SHOW_FRAME_CONTROLS, m_settings.main.show.wnd & F_MAIN_SHOW_CONTROLS);
	SetModified();
}

void CPropertyPageView::OnBnClickedCheckShowFrameListofdeals(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdatePresentationDeals(GET);
	SetModified();
}

void CPropertyPageView::OnBnClickedCheckShowFrameOsd(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdatePresentationDeals(GET);
	SetModified();
}

void CPropertyPageView::OnBnClickedCheckShowFrameControls(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdatePresentationDeals(GET);
	SetModified();
}
