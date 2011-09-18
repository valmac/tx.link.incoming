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
 *  PropertyPageKeys.cpp : implementation file
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "PropertyPageKeys.h"
#include <color.h>


CEditableListCtrl::CEditableListCtrl()
{
	m_edit.iItem = -1;
	m_edit.iSubItem = -1;
}

int CEditableListCtrl::GetSelectedItem() const
{
	return CListViewCtrl::GetSelectionMark();
}

int CEditableListCtrl::GetEditingItem() const
{
	return m_edit.iItem;
}

BOOL CEditableListCtrl::IsEditing() const
{
	return (GetEditingItem() >= 0);
}

int CEditableListCtrl::BeginEdit(DWORD shortcut)
{
	int iItem = GetSelectedItem();
	if (iItem >= 0)
	{
		m_edit.iSubItem = 1;
		m_edit.iItem = iItem;
		m_shortcut.current = m_shortcut.previos = shortcut;
		Invalidate();
	}
	return iItem;
}

int CEditableListCtrl::EndEdit()
{
	if (IsEditing())
	{
		m_edit.iItem = -1;
		Invalidate();
#if 1
		ResetShortcutsLL();
#endif
	}
	return 0;
}

void CEditableListCtrl::DrawItem (UINT idCtrl, LPDRAWITEMSTRUCT lpDrawItemStruct)
{
	int iItem = lpDrawItemStruct->itemID;
	if (iItem < 0)
		return;

	CDC dc;
	dc.Attach(lpDrawItemStruct->hDC);

	COLORREF crOldTextColor = dc.GetTextColor();
	COLORREF crOldBkColor = dc.GetBkColor();
	COLORREF colorSelection;	
	COLORREF colorHighlight, colorInactive;
	COLORREF background, color;
	double opacity;
	
	GetHighlightParams(*this, colorHighlight, colorInactive, colorSelection, opacity);
	color = crOldTextColor; background = crOldBkColor;

	int iEditedItem = GetEditingItem();
	BOOL focused = FALSE;

	CHeaderCtrl header = GetHeader();

	LV_COLUMN lvc;
	lvc.mask = LVCF_FMT|LVCF_WIDTH;

	CRect frameRect(lpDrawItemStruct->rcItem);
	//frameRect.DeflateRect(0, 1);

	if ((lpDrawItemStruct->itemAction | (ODA_SELECT|ODA_FOCUS)) && 
		(lpDrawItemStruct->itemState & (ODS_SELECTED|ODS_FOCUS)))
	{
		background = my::lib::SumColorsAlpha(background, colorSelection, opacity);
		dc.FillSolidRect(&frameRect, background);
	}
	if ((lpDrawItemStruct->itemAction | ODA_FOCUS) && (lpDrawItemStruct->itemState & ODS_FOCUS))
	{
		focused = TRUE;
		CBrush br;
		br.CreateSolidBrush(colorSelection);
		dc.FrameRect(&frameRect, br);
	}

	CRect rcItem(lpDrawItemStruct->rcItem);
	rcItem.right = rcItem.left;
	TSTRING_STD(text);

#if 0
	if (m_boldFont.IsNull())
	{
		HFONT hFont = this->GetFont();

		CLogFont lfont(hFont);
		//lfont.lfHeight = lfont.lfHeight * 5/4;
		lfont.lfWeight = FW_BOLD;

		m_boldFont.CreateFontIndirect(&lfont);
	}
	this->SetFont(m_boldFont, FALSE);
#endif

	for (int i = 0; GetColumn(i, &lvc); i++)
	{
		BOOL edit = FALSE;
		color = crOldTextColor;

		rcItem.left = rcItem.right;
		rcItem.right += lvc.cx;

		if (focused && (iItem == iEditedItem) && (i == 1))
		{
			CBrush br2;
			br2.CreateSolidBrush(crOldBkColor);
			dc.SelectBrush(br2);
			CPen pen;
			pen.CreatePen(PS_SOLID, 1, ::GetSysColor(COLOR_WINDOWFRAME));
			dc.SelectPen(pen);
			dc.Rectangle(&rcItem);
			edit = TRUE;
		}

		int len = GetItemText(iItem, i, text, sizeof(text));

		const int margin = header.GetBitmapMargin();
		rcItem.left += margin;
		
		if (edit)
		{
			CSize size;
			dc.GetTextExtent(text, len, &size);
			if (size.cx)
			{
				CRect rect(rcItem.left - 2, rcItem.top + 1 + 1, rcItem.left + size.cx + 2, rcItem.bottom - 1 - 1);
				CBrush brush;
				brush.CreateSolidBrush(colorHighlight);
				dc.FillRect(&rect, brush);
				color = RGB(255, 255, 255);
			}
		}

		dc.SetTextColor(color);
		dc.DrawText(text, len, &rcItem, DT_LEFT|DT_SINGLELINE|DT_VCENTER);
	} // for (i)

	dc.SetTextColor(crOldTextColor);
	dc.SetBkColor(crOldBkColor);

	dc.Detach();
}


LRESULT CEditableListCtrl::OnKey(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = FALSE;

	if (! IsEditing())
		return 0;

	int msg = uMsg;
	int vkey = (int)(wParam);
	int force = (lParam & 0x80000000);
#if 1
	if (msg == WM_SYSKEYDOWN)
		msg = WM_KEYDOWN;
	else if (msg == WM_SYSKEYUP)
		msg = WM_KEYUP;
#endif
	if (msg == WM_KEYDOWN)
	{
		bHandled = TRUE;

		DWORD shortcut = 0;
		BOOL endEdit = FALSE;
		if (vkey == VK_CLEAR)
		{
			shortcut = 0;
		}
		else if (vkey == VK_ESCAPE || vkey == VK_TAB)
		{
			shortcut = m_shortcut.previos;
			endEdit = TRUE;			
		}
		else if (vkey == VK_RETURN)
		{
			shortcut = m_shortcut.current;
			endEdit = TRUE;
		}
		else
		{
			shortcut = INVALID_SHORTCUT;
			int flags = Action::F_CHECKMOUSEASYNC;
			int repeat = (lParam & 0x40000000);
			if (! repeat)				
				shortcut = MakeShortcutKey(vkey, flags, msg);
			if (shortcut == INVALID_SHORTCUT)
				goto end;
		}		
		if (m_shortcut.current != shortcut || force)
		{
			m_shortcut.current = shortcut;
			TRACE("MAKESHORTCUT 0x%08x\r\n", shortcut);			
			TSTRING_SMALL2(text, size);
			ShortcutToText(shortcut, text, size);
			SetItemText(m_edit.iItem, m_edit.iSubItem, text);
		}
		if (endEdit)
		{
			::SendMessage(GetParent(), UM_EDIT, vkey, m_shortcut.current);
		}
	} // if (msg == WM_KEYDOWN)
	else if (msg == WM_KEYUP)
	{
		// Вызываем эту функцию, чтобы изменился внутренний буфер:
		int flags = Action::F_UP|Action::F_CHECKMOUSEASYNC;
		MakeShortcutKey(vkey, flags, msg);		
		bHandled = TRUE;
	}
end:
	return 0;
}

LRESULT CEditableListCtrl::OnMouse(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = FALSE;

	if (! IsEditing())
		return 0;

	int msg = uMsg;
	int vkey = (int)wParam;
	if (msg != WM_MOUSEMOVE)
	{
		bHandled = TRUE;

		DWORD shortcut = 0;
		BOOL endEdit = FALSE;

		int flags = Action::F_CHECKMOUSEASYNC;
		shortcut = MakeShortcutKey(0, flags, msg, lParam);
		if (shortcut == INVALID_SHORTCUT)
			goto end;

		if (m_shortcut.current != shortcut)
		{
			m_shortcut.current = shortcut;
			TRACE("MAKESHORTCUT 0x%08x\r\n", shortcut);			
			TSTRING_SMALL2(text, size);
			ShortcutToText(shortcut, text, size);
			SetItemText(m_edit.iItem, m_edit.iSubItem, text);
		}
		if (endEdit)
		{
			::SendMessage(GetParent(), UM_EDIT, vkey, m_shortcut.current);
		}
	}
end:
	return 0;
}

// CPropertyPageKeys dialog

CPropertyPageKeys::CPropertyPageKeys(const Settings::Shortcuts & settings)
{
	m_settings = settings;
}

CPropertyPageKeys::~CPropertyPageKeys()
{
}

void CPropertyPageKeys::InitCombo()
{
	LPCTSTR names[] = {
		TEXT("Схема по умолчанию"),
		TEXT("Собственная схема"),
	};
	CComboBox combo = GetDlgItem(IDC_COMBO_SHORTCUTS);
	combo.AddString(names[I_LAYOUT_DEFAULT]);
	combo.AddString(names[I_LAYOUT_CUSTOM]);
	combo.SetCurSel(m_settings.iCurrentLayout);
}

int CPropertyPageKeys::InitListOfShortcuts()
{
	CEditableListCtrl & listCtrl = m_listOfShortcuts;

	int format = LVCFMT_LEFT;
	LPCTSTR str;
	int width;
	CRect rect;
	listCtrl.GetClientRect(rect);
	int dx = ::GetSystemMetrics(SM_CXSMSIZE);
	str = TEXT("Действие"); 
	width = rect.Width() * 3 / 5;
	listCtrl.InsertColumn(0, str, format, width);
	str = TEXT("Комбинация"); 
	width = rect.Width() - width - dx;
	listCtrl.InsertColumn(1, str, format, width);
	
	listCtrl.SetExtendedListViewStyle(listCtrl.GetExtendedListViewStyle()|
		LVS_EX_FULLROWSELECT|0|LVS_EX_DOUBLEBUFFER|LVS_EX_BORDERSELECT);

	// Сочетания клавиш:
	my::ShortcutMap & layout = GetCurrentLayout();
	InitListOfShortcuts(layout, TRUE);

	return S_OK;
}

void CPropertyPageKeys::InitListOfShortcuts(my::ShortcutMap & layout, BOOL clear)
{
	// Обновляем текст: 
	CEditableListCtrl & listCtrl = m_listOfShortcuts;
	if (clear)
		listCtrl.DeleteAllItems();

	int size = layout.size();
	LPCTSTR name;
	for (int i = 0; i < size; i++)
	{
		name = GetActionName(i);
		if (name)
		{
			if (clear)
				listCtrl.InsertItem(i, name, -1);
			TCHAR text[64];
			DWORD shortcut = layout[i];
			ShortcutToText(shortcut, text, SIZEOF_ARRAY(text));
			listCtrl.SetItemText(i, 1, text);
		}
		else
			break;
	} // for (i)
}

int CPropertyPageKeys::BeginEdit()
{
	int iItem = m_listOfShortcuts.GetSelectedItem();
	if (iItem >= 0)
	{
		my::ShortcutMap & layout = GetCurrentLayout();
		DWORD shortcut = layout[iItem];
		m_listOfShortcuts.SetFocus();
		m_listOfShortcuts.BeginEdit(shortcut);
#if 0
		CWindow wnd = GetDlgItem(IDC_BUTTON_EDIT);
		wnd.EnableWindow(FALSE);
#endif
	}
	return iItem;
}

int CPropertyPageKeys::EndEdit()
{
	if (IsEditingNow())
	{
		m_listOfShortcuts.EndEdit();		
	}
	return 0;
}

BOOL CPropertyPageKeys::IsEditingNow() const
{
	return m_listOfShortcuts.IsEditing();
}

void CPropertyPageKeys::SetModified(BOOL bChanged)
{
	CPropertyPageKeysBase::SetModified(bChanged);
}

void CPropertyPageKeys::ClearShortcut()
{
	BeginEdit();
	m_listOfShortcuts.SendMessage(WM_KEYDOWN, (WPARAM)VK_CLEAR);
	m_listOfShortcuts.SendMessage(WM_KEYDOWN, (WPARAM)VK_RETURN);
}

// CPropertyPageKeys message handlers

BOOL CPropertyPageKeys::OnInitDialog(CWindow wndFocus, LPARAM lInitParam)
{
	DoDataExchange(FALSE);
	//m_listOfShortcuts.Attach(GetDlgItem(IDC_LIST_SHORTCUTS));

	InitCombo();

	InitListOfShortcuts();

	CWindow wnd = GetDlgItem(IDC_BUTTON_EDIT);
	wnd.EnableWindow(FALSE);
#if 0
	wnd.ShowWindow(SW_HIDE);
#endif

	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX Property Pages should return FALSE
}

void CPropertyPageKeys::OnBnClickedButtonEdit(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	BeginEdit();
}

void CPropertyPageKeys::OnBnClickedButtonClear(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	ClearShortcut();
}

void CPropertyPageKeys::OnBnClickedButtonResetShortcuts(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	if (IsEditingNow())
		m_listOfShortcuts.SendMessage(WM_KEYDOWN, VK_ESCAPE, 0);

	LPCTSTR msg = TEXT("Вы уверены, что хотите восстановить значения по умолчанию?");
	int ret = my::MessageBox(*this, msg, MB_ICONQUESTION|MB_YESNOCANCEL);
	if (ret == IDYES)
	{	
		// Восстанавливаем исходные сочетания:
		my::ShortcutMap & layout = m_settings.layouts[I_LAYOUT_CUSTOM];
		InitActionsDefault(layout);
		InitListOfShortcuts(layout);		
		SetModified();
	}
}

LRESULT CPropertyPageKeys::OnNMDblclkListShortcuts(NMHDR *pNMHDR)
{
	LPNMITEMACTIVATE pNMItemActivate = reinterpret_cast<LPNMITEMACTIVATE>(pNMHDR);	
	
	BeginEdit();

	return 0;
}

LRESULT CPropertyPageKeys::OnNMClickListShortcuts(NMHDR *pNMHDR)
{
	LPNMITEMACTIVATE pNMItemActivate = reinterpret_cast<LPNMITEMACTIVATE>(pNMHDR);

	if (pNMItemActivate->iSubItem == 1)
		BeginEdit();

	return 0;
}

LRESULT CPropertyPageKeys::OnNMRClickListShortcuts(NMHDR *pNMHDR)
{
	LPNMITEMACTIVATE pNMItemActivate = reinterpret_cast<LPNMITEMACTIVATE>(pNMHDR);
	
	int iSubMenu = -1;
	if (IsEditingNow())
	{		
#if 0
		TSTRING_SMALL2(text, size);
		int len = m_listOfShortcuts.GetItemText(m_listOfShortcuts.GetEditingItem(), 1, text, size);
		if (len > 0)
			iSubMenu = 1;
#endif
	}
	else
		iSubMenu = 0;
	if (iSubMenu >= 0)
	{
		CMenu root, menu;
		root.LoadMenu(IDR_MENU_SHORTCUTKEYS);

		menu = root.GetSubMenu(iSubMenu);
		if (menu)
		{
			my::ShortcutMap & layout = GetCurrentLayout();
			int iItem = my::ctrl::GetFocusedItem(m_listOfShortcuts);
			if (iItem >= 0 && layout[iItem] == 0)
				menu.DeleteMenu(ID_SHORTCUTKEYS_CLEAR, MF_BYCOMMAND);
#if 1
			if (m_settings.iCurrentLayout == I_LAYOUT_DEFAULT)
			{
				menu.DeleteMenu(menu.GetMenuItemCount() - 1, MF_BYPOSITION);
				menu.DeleteMenu(menu.GetMenuItemCount() - 1, MF_BYPOSITION);
			}
#endif
			CPoint point;
			GetCursorPos(&point);
			menu.TrackPopupMenu(TPM_LEFTALIGN |TPM_RIGHTBUTTON, point.x, point.y, *this);
		}
	}

	return 0;
}

void CPropertyPageKeys::OnShortcutkeysEdit(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	BeginEdit();
}

void CPropertyPageKeys::OnShortcutkeysClear(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	ClearShortcut();
}

void CPropertyPageKeys::OnShortcutkeysDefault(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	BeginEdit();
	int i = my::ctrl::GetFocusedItem(m_listOfShortcuts);
	m_listOfShortcuts.m_shortcut.current = m_settings.layouts[I_LAYOUT_DEFAULT].at(i);
	m_listOfShortcuts.SendMessage(WM_KEYDOWN, (WPARAM)VK_RETURN, (LPARAM)0x80000000);	
}

LRESULT CPropertyPageKeys::OnLvnItemchangedListShortcuts(NMHDR *pNMHDR)
{
	LPNMLISTVIEW pNMLV = reinterpret_cast<LPNMLISTVIEW>(pNMHDR);
#if defined _DEBUG && 0
	TRACE("iItem=%d, iSubItem=%d, uChanged=%d, uNewState=%d, uOldState=%d\r\n", 
		pNMLV->iItem, pNMLV->iSubItem, pNMLV->uChanged, pNMLV->uNewState, pNMLV->uOldState);
#endif
	if (pNMLV->uOldState == 0)
	{
		int iItem = pNMLV->iItem;

		if (IsEditingNow())
			m_listOfShortcuts.SendMessage(WM_KEYDOWN, VK_ESCAPE, 0);

		CWindow wnd = GetDlgItem(IDC_BUTTON_EDIT);
		if (wnd.IsWindowEnabled() == FALSE)
			wnd.EnableWindow();

		wnd = GetDlgItem(IDC_BUTTON_CLEAR);
		my::ShortcutMap & layout = GetCurrentLayout();
		wnd.EnableWindow(layout[iItem]);
	}
	return 0;
}

void CPropertyPageKeys::OnCbnSelchangeComboShortcuts(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	CComboBox combo = GetDlgItem(IDC_COMBO_SHORTCUTS);
	m_settings.iCurrentLayout = combo.GetCurSel();
	my::ShortcutMap & layout = GetCurrentLayout();
	InitListOfShortcuts(layout);
	SetModified();
}

LRESULT CPropertyPageKeys::OnWndMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = TRUE;
	switch (uMsg)
	{
	case UM_EDIT:
		{
			int iItem = m_listOfShortcuts.GetEditingItem();
			if (iItem >= 0)
			{
				int iSubItem = 1;
				int vkey = (int)wParam;
				DWORD shortcutKey32 = (DWORD)lParam;

				EndEdit();

				my::ShortcutMap * pLayout = &GetCurrentLayout();
				if (pLayout->at(iItem) != shortcutKey32)
				{// Новое значение:
					if (m_settings.iCurrentLayout == I_LAYOUT_DEFAULT)
					{
#if 0
						if (IDYES != my::MessageBox(*this, TEXT("Изменить собственную схему?"), 
							MB_ICONQUESTION|MB_YESNOCANCEL))
							break;
						else
#endif
						{

							my::ShortcutMap & custom = m_settings.layouts[I_LAYOUT_CUSTOM];
#if 1
							for (size_t i = 0; i < custom.size(); i++)
								custom[i] = pLayout->at(i);
#endif
							pLayout = &custom;
							m_settings.iCurrentLayout = I_LAYOUT_CUSTOM;
							CComboBox combo = GetDlgItem(IDC_COMBO_SHORTCUTS);
							combo.SetCurSel(m_settings.iCurrentLayout);
						}
					}
					if (shortcutKey32 != 0)
					{
						// Проверим на совпадение с другими комбинациями:
						for (my::ShortcutMap::const_iterator it = pLayout->begin(); it != pLayout->end(); it++)
						{
							if (it->first == iItem)
								continue;
							if (it->second == shortcutKey32)
							{
								TSTRING_SMALL2(text, size);
								m_listOfShortcuts.GetItemText(iItem, iSubItem, text, size);
								CString msg;
								msg.Format(TEXT("Сочетание \"%s\" совпадает с ранее заданной комбинацией для действия\"%s\"! \n")
									TEXT("\nХотите использовать новое сочетание?"), 
									text, GetActionName(it->first));
								int ret = my::MessageBox(*this, msg, MB_ICONQUESTION|MB_YESNOCANCEL);
								if (ret == IDYES)
								{// Стираем старую комбинацию:
									pLayout->at(it->first) = 0;
									m_listOfShortcuts.SetItemText(it->first, iSubItem, TEXT(""));
								}
								else
								{// Возвращаем старое значение:
									shortcutKey32 = pLayout->at(iItem);
									TSTRING_SMALL2(text, size);
									ShortcutToText(shortcutKey32, text, size);
									m_listOfShortcuts.SetItemText(iItem, iSubItem, text);
								}
								break;
							}
						} // for (it)	
					}
					pLayout->at(iItem) = shortcutKey32;
					SetModified();
				}
				CWindow wnd = GetDlgItem(IDC_BUTTON_CLEAR);
				wnd.EnableWindow(shortcutKey32);
			}
		}
		break;
	default:
		bHandled = FALSE;
	} // switch (uMsg)

	return 0;
}

BOOL CPropertyPageKeys::OnApply()
{
	if (IsEditingNow())
	{
		m_listOfShortcuts.SendMessage(WM_KEYDOWN, VK_RETURN, 0);
		if (GetFocus() == m_listOfShortcuts)
			return FALSE;
	}

	CWindow wnd = GetParent();
	if (wnd)
		wnd.SendMessage(UM_APPLY, this->IDD, (LPARAM)&m_settings);

	SetModified(FALSE);

	return CPropertyPageKeysBase::OnApply();
}

BOOL CPropertyPageKeys::OnQueryCancel()
{
	if (IsEditingNow())
	{
		m_listOfShortcuts.SendMessage(WM_KEYDOWN, VK_ESCAPE, 0);
		if (GetFocus() == m_listOfShortcuts)
			return FALSE;
	}
	return CPropertyPageKeysBase::OnQueryCancel();
}

BOOL CPropertyPageKeys::OnKillActive()
{
#if 0
	if (IsEditingNow())
	{
		m_listOfShortcuts.SendMessage(WM_KEYDOWN, VK_ESCAPE, 0);
	}
#endif
	return CPropertyPageKeysBase::OnKillActive();
}
