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
 *  PropertyPageListOfDeals.cpp : implementation file
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "PropertyPageListOfDeals.h"
#include <color.h>


CColorListBox::CColorListBox()
{
	m_pColors = NULL;
}

void CColorListBox::DrawItem(UINT idCtrl, LPDRAWITEMSTRUCT lpDrawItemStruct)
{
	int iItem = lpDrawItemStruct->itemID;
	if (iItem < 0)
		return;
	
	const Settings::Presentation::ListOfDeals::Color * pColor = &m_pColors->item[iItem];

	COLORREF colorHighlight, colorInactive;
	COLORREF colorBackground, colorText;
	double opacity;

	const Settings::Presentation & settings = theApp.m_settings.presentation;

	colorHighlight = settings.list.colors.item[I_COLOR_FRAME].text;
	GetHighlightParams(*this, colorHighlight, colorInactive, colorHighlight, opacity, F_VALID_COLOR_HIGHLIGHT);

	colorText = m_pColors->item[I_COLOR_NORMAL].text;
	colorBackground = m_pColors->item[I_COLOR_NORMAL].backgrnd;

	if (m_pColors->useDifferentColors)
		colorBackground = pColor->backgrnd;
	if (m_pColors->useDifferentColorsText)
		colorText = pColor->text;

	TSTRING_SMALL2(str, size);
	this->GetItemText(iItem, 0, str, size);
	LPCTSTR lpszText = (LPCTSTR)str;// lpDrawItemStruct->itemData;
	CDC dc;

	CRect rect;
	GetClientRect(&rect);
	dc.Attach(lpDrawItemStruct->hDC);

	//CDC dc;
	//CBitmap bitmap;
	//bitmap.CreateCompatibleBitmap(&dc0, rect.Width(), rect.Height());
	//dc.CreateCompatibleDC(&dc0);
	//dc.SelectObject(&bitmap);

	// Save these value to restore them when done drawing.
	COLORREF crOldTextColor = dc.GetTextColor();
	COLORREF crOldBkColor = dc.GetBkColor();

	dc.FillSolidRect(&lpDrawItemStruct->rcItem, colorBackground);

	CRect itemRect(lpDrawItemStruct->rcItem);
	itemRect.left += 3;

	dc.SetBkMode(TRANSPARENT);	
	// If this item is selected, set the backgrnd color and the text color to appropriate values. 
	// Also, erase rect by filling it with the backgrnd color.
	if ((lpDrawItemStruct->itemAction | (ODA_SELECT|ODA_FOCUS)) &&
	  (lpDrawItemStruct->itemState & (ODS_SELECTED|ODS_FOCUS)))
	{
		colorBackground = my::lib::SumColorsAlpha(colorBackground, colorHighlight, opacity);
		dc.FillSolidRect(&lpDrawItemStruct->rcItem, colorBackground);
	}
	else
		dc.FillSolidRect(&lpDrawItemStruct->rcItem, colorBackground);

	// If this item has the focus, draw a red frame around the item's rect.
	if ((lpDrawItemStruct->itemAction | ODA_FOCUS) && (lpDrawItemStruct->itemState & ODS_FOCUS))
	{
		CBrush br;
		br.CreateSolidBrush(colorHighlight);
		dc.FrameRect(&lpDrawItemStruct->rcItem, br);
	}

	dc.SetTextColor(colorText);
	dc.DrawText(lpszText, (int)_tcslen(lpszText), &itemRect, DT_LEFT|DT_SINGLELINE|DT_VCENTER);

	//dc0.SetStretchBltMode(COLORONCOLOR);
	//dc0.BitBlt(rect.left, rect.top, rect.Width(), rect.Height(), &dc, rect.left, rect.top, SRCCOPY);

	// Reset the backgrnd color and the text color back to their
	// original values.
	dc.SetTextColor(crOldTextColor);
	dc.SetBkColor(crOldBkColor);

	dc.Detach();
}

// CPropertyPageListOfDeals dialog

CPropertyPageListOfDeals::CPropertyPageListOfDeals(const Settings::Presentation::ListOfDeals & settings)
	: m_settings(settings)
{
	m_initialized = 0;
	m_updating = 0;	
}

CPropertyPageListOfDeals::~CPropertyPageListOfDeals()
{
}

void CPropertyPageListOfDeals::InitListColumns()
{
	m_listColumns = GetDlgItem(IDC_LIST_COLUMNS);

	CRect rect;
	m_listColumns.GetClientRect(rect);

	LPCTSTR name;
	int width;
	int format = LVCFMT_LEFT;

	width = 25;
	m_listColumns.InsertColumn(0, TEXT(""), format, width);
	rect.left += width;
	name = TEXT("Название"); 
	width = rect.Width();
	m_listColumns.InsertColumn(1, name, format, width);

	m_listColumns.SetExtendedListViewStyle(m_listColumns.GetExtendedListViewStyle()|
		LVS_EX_FULLROWSELECT|0|LVS_EX_DOUBLEBUFFER|LVS_EX_CHECKBOXES|
		0  ); // LVS_EX_BORDERSELECT LVS_EX_GRIDLINES LVS_EX_FULLROWSELECT


	LPCTSTR names[] = {
		TEXT(""),
		TEXT("Индексация"),
		TEXT("Значок"),
		TEXT("Номер"),
		TEXT("Дата"),
		TEXT("Время"),
		TEXT("Название"),
		TEXT("Код"),
		TEXT("Класс"),
		TEXT("Операция"),
		TEXT("Цена"),
		TEXT("Цена2"),
		TEXT("Количество"),
		TEXT("Объём"),
		TEXT("Состояние"),
		TEXT("Клиент"),
		TEXT("Счёт"),
	};
	int i, iItem = 0;
	for (i = COL_FIRST; i < SIZEOF_ARRAY(names); i++)
	{		
		const Settings::Presentation::ListOfDeals::Column & column = m_settings.columns.item[i];
		if (column.index >= 0)
		{
			name = names[column.index];
			int iItem = i - COL_FIRST;
			m_listColumns.InsertItem(iItem, TEXT(""));
			m_listColumns.SetItemText(iItem, 1, name);
			m_listColumns.SetItemData(iItem, static_cast<DWORD_PTR>(column.index));
			m_listColumns.SetCheckState(iItem, column.visible);
		}
		else
			break;
	} // for (i)

#if defined _DEBUG && 0
	my::ctrl::SetFocusedItem(m_listColumns, 0, LVIS_SELECTED);
#endif
	::SetWindowTheme(m_listColumns, TEXT("explorer"), NULL);
}

void CPropertyPageListOfDeals::InitComboAlignment()
{
	LPCTSTR names[] = {
		TEXT("Лево"),
		TEXT("Право"),
		TEXT("Центр"),
	};
	CComboBox combo = GetDlgItem(IDC_COMBO1);
	for (int i = 0; i < SIZEOF_ARRAY(names); i++)
		combo.AddString(names[i]);

	CWindow wnd = GetDlgItem(IDC_STATIC_ALIGNMENT);
	wnd.EnableWindow(FALSE);
	combo.EnableWindow(FALSE);
}

LPCTSTR CPropertyPageListOfDeals::GetPropertyName(unsigned int index)
{
	LPCTSTR names[] = {
		TEXT("Общий"),
		TEXT("Покупка"),
		TEXT("Продажа"),
		TEXT("Активная заявка на покупку"),
		TEXT("Активная заявка на продажу"),
		TEXT("Активная стоп-заявка на покупку"),
		TEXT("Активная стоп-заявка на продажу"),
		TEXT("Исполненная стоп-заявка на покупку"),
		TEXT("Исполненная стоп-заявка на продажу"),
		TEXT("Снятая заявка на покупку"),
		TEXT("Снятая заявка на продажу"),
		TEXT("Снятая стоп-заявка на покупку"),
		TEXT("Снятая стоп-заявка на продажу"),
		TEXT("Проблемы с покупкой"),
		TEXT("Проблемы с продажей"),
		TEXT("Рамка"),
		TEXT("Индексация"),
		NULL,
	};
	return names[index];
}

void CPropertyPageListOfDeals::InitListColors()
{
	LPCTSTR name;

	CRect rect;
	m_listColors.GetClientRect(&rect); 
	int cx = GetSystemMetrics(SM_CXHSCROLL);
	m_listColors.InsertColumn(0, TEXT("Шаблоны"), LVCFMT_LEFT, rect.Width() - cx);

	for (int i = 0; ; i++)
	{
		name = GetPropertyName(i);
		if (name != NULL)
		{
			m_listColors.InsertItem(i, name);
			m_listColors.SetItemData(i, (DWORD_PTR)&m_settings.colors.item[i]);
		}
		else
			break;
	} // for (i)
	while (name != NULL);

	m_listColors.SetColors(m_settings.colors);
	//m_listColors.SetItemState(0, LVIS_SELECTED, LVIS_SELECTED);
	m_listColors.SendMessage(WM_KEYDOWN, VK_HOME, 0);
}

void CPropertyPageListOfDeals::SetModified(BOOL bChanged)
{
	if (bChanged && m_initialized)
	{
		CWindow parentWnd = theApp.GetMainWnd();
		if (parentWnd)
			parentWnd.SendMessage(UM_APPLY_SETTINGS, APPLY_SETTINGS_PRESENTATION_LIST, (LPARAM)&m_settings);
		Invalidate2(GetFocus());
	}
	CPropertyPageListOfDealsBase::SetModified(bChanged);
}

void CPropertyPageListOfDeals::UpdateSettings(int set)
{
	UpdateColumns(set);
}

void CPropertyPageListOfDeals::UpdateColumns(int set)
{
	int i, iItem = 0;
	int count = m_listColumns.GetItemCount();
	for (i = 0; i < count; i++)
	{		
		if (set)
		{
		}
		else
		{
			int index = static_cast<int>(m_listColumns.GetItemData(i));
			int j = i + COL_FIRST;
			m_settings.columns.item[j].index = (char)index;
			m_settings.columns.item[j].visible = (char)m_listColumns.GetCheckState(i);
		}
	} // for (i)
}

void CPropertyPageListOfDeals::UpdateColumnsInv(int set, int iItem)
{
	for (int i = 0; i < m_listColumns.GetItemCount(); i++)
	{		
		if (set)
		{
		}
		else
		{
			int index = (int)m_listColumns.GetItemData(i);
			int state = m_listColumns.GetCheckState(i);
			m_settings.columns.item[i].index = (char)index;
			m_settings.columns.item[i].visible = (char)((i == iItem) ? !state : state);
		}
	} // for (i)
}


void CPropertyPageListOfDeals::EnableButtonUp(BOOL enable)
{
	CWindow wnd = GetDlgItem(IDC_BUTTON_UP);
	wnd.EnableWindow(enable);
}

void CPropertyPageListOfDeals::EnableButtonDown(BOOL enable)
{
	CWindow wnd = GetDlgItem(IDC_BUTTON_DOWN);
	wnd.EnableWindow(enable);
}

void CPropertyPageListOfDeals::EnableButtonUpDown(int iItem)
{
	if (iItem < 0)
		iItem = my::ctrl::GetFocusedItem(m_listColumns);
	if (iItem >= 0)
	{
		EnableButtonUp((iItem == 0) ? FALSE : TRUE);
		EnableButtonDown((iItem == m_listColumns.GetItemCount() - 1) ? FALSE : TRUE);
	}
}

void CPropertyPageListOfDeals::EnableBackground(BOOL enable)
{
	CWindow wnd;
	wnd = GetDlgItem(IDC_STATIC_BACKGROUND);
	wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_COLOR_BACKGROUND);
	wnd.EnableWindow(enable);
}

void CPropertyPageListOfDeals::EnableColorText(BOOL enable)
{
	CWindow wnd;
	wnd = GetDlgItem(IDC_STATIC_TEXT);
	wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_COLOR_TEXT);
	wnd.EnableWindow(enable);
}

void CPropertyPageListOfDeals::SwapItems(int i1, int i2, int iSel)
{
	TSTRING_SMALL(str1);
	TSTRING_SMALL(str2);
	m_listColumns.GetItemText(i1, 1, str1, SIZEOF_ARRAY(str1));
	m_listColumns.GetItemText(i2, 1, str2, SIZEOF_ARRAY(str2));
	int check1 = m_listColumns.GetCheckState(i1);
	int check2 = m_listColumns.GetCheckState(i2);
	DWORD_PTR data1 = m_listColumns.GetItemData(i1);
	DWORD_PTR data2 = m_listColumns.GetItemData(i2);
	UINT mask = LVIS_SELECTED|LVIS_FOCUSED;
	UINT state1 = m_listColumns.GetItemState(i1, mask);
	UINT state2 = m_listColumns.GetItemState(i2, mask);
#if 1
	Settings::Presentation::ListOfDeals::Column & column1 = m_settings.columns.item[i1 + COL_FIRST];
	Settings::Presentation::ListOfDeals::Column & column2 = m_settings.columns.item[i2 + COL_FIRST];
	Settings::Presentation::ListOfDeals::Column column;
#endif
	m_updating = 1;
	m_listColumns.SetItemText(i1, 1, str2);
	m_listColumns.SetCheckState(i1, check2);
	m_listColumns.SetItemData(i1, data2);
	m_listColumns.SetItemState(i1, state2, mask);
	m_listColumns.SetItemText(i2, 1, str1);
	m_listColumns.SetCheckState(i2, check1);
	m_listColumns.SetItemData(i2, data1);
	m_listColumns.SetItemState(i2, state1, mask);
#if 1
	column = column1;
	column1.format = column2.format; column2.format = column.format;
#endif
	m_updating = 0;
}

void CPropertyPageListOfDeals::ApplyColors(int index)
{
	const Settings::Presentation::ListOfDeals::Color color = m_settings.colors.item[index];
	int selected = 0;
	for (int i = 0; i < m_listColors.GetItemCount(); i++)
	{
		if (m_listColors.GetItemState(i, LVIS_SELECTED) == LVIS_SELECTED)
		{
			Settings::Presentation::ListOfDeals::Color & _color = m_settings.colors.item[i];
			if (_color != color)
			{
				if (i != I_COLOR_FRAME)
					_color = color;
				else
					_color.text = color.text;
				selected++;
			}
		}
	} // for (i)
	if (selected)
	{
		SetModified();
		CWindow wnd = GetFocus();
		wnd.Invalidate();
		Invalidate2(wnd);
	}
}

void CPropertyPageListOfDeals::ResetColors(int index)
{
	if (index == -1)
		index = my::ctrl::GetFocusedItem(m_listColors);
	if (index >= 0)
	{
		Settings::Presentation::ListOfDeals::Color & color = m_settings.colors.item[index];
		Settings::Presentation::ListOfDeals::Color defaultColor;
		MakeColorDefault(defaultColor, index);
		if (color != defaultColor)
		{
			color = defaultColor;
			SetModified();
			CWindow wnd = GetFocus();
			wnd.Invalidate();
			Invalidate2(wnd);
		}
	}
}

// CPropertyPageListOfDeals message handlers

BOOL CPropertyPageListOfDeals::OnInitDialog(CWindow wndFocus, LPARAM lInitParam)
{
	DoDataExchange(FALSE);

	m_hints.Create(*this);
	m_painter.Create(*this);

	BOOL modern = TRUE;
#if AUTOSELECT_MODERN_THEME
	modern = IsModernOS();
#endif
	CButton btn;
	btn = GetDlgItem(IDC_BUTTON_UP);
	if (modern)
		btn.SetIcon(theApp.LoadSmallIcon(IDI_MOVE_UP));
	m_hints.AddTool (CToolInfo (TTF_SUBCLASS, btn, 0, NULL, TEXT("Переместить выше")));
	btn = GetDlgItem(IDC_BUTTON_DOWN);
	if (modern)
		btn.SetIcon(theApp.LoadSmallIcon(IDI_MOVE_DOWN));
	m_hints.AddTool (CToolInfo (TTF_SUBCLASS, btn, 0, NULL, TEXT("Переместить ниже")));

	InitListColumns();
	InitListColors();

	InitComboAlignment();

	m_listColors.SetFocus();

	btn = GetDlgItem(IDC_CHECK_USE_DIFFERENT_COLORS);
	btn.SetCheck(m_settings.colors.useDifferentColors ? BST_CHECKED : BST_UNCHECKED);
	EnableBackground(m_settings.colors.useDifferentColors);

	btn = GetDlgItem(IDC_CHECK_USE_DIFFERENT_COLORS_TEXT);
	btn.SetCheck(m_settings.colors.useDifferentColorsText ? BST_CHECKED : BST_UNCHECKED);
	EnableColorText(m_settings.colors.useDifferentColorsText);

#if 1
	SetDlgCursorHand(this, IDC_COLOR_BACKGROUND);
	SetDlgCursorHand(this, IDC_COLOR_TEXT);
#endif

	EnableButtonUp(FALSE);
	EnableButtonDown(FALSE);

	m_initialized = TRUE;

	//Invalidate2();

	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX Property Pages should return FALSE
}

void CPropertyPageListOfDeals::OnBnClickedButtonUp(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	m_listColumns.SetFocus();
	int iSel = my::ctrl::GetFocusedItem(m_listColumns);
	if (iSel >= 0)
	{
		int i1 = iSel;
		int i2 = i1 - 1;
		if (i2 >= 0)
		{
			SwapItems(i1, i2, iSel);
			UpdateColumns(0);
			SetModified();

			EnableButtonUpDown(-1);
		}
	}
}

void CPropertyPageListOfDeals::OnBnClickedButtonDown(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	m_listColumns.SetFocus();
	int iSel = my::ctrl::GetFocusedItem(m_listColumns);
	if (iSel >= 0)
	{
		int i1 = iSel;
		int i2 = i1 + 1;
		if (i2 < m_listColumns.GetItemCount())
		{
			SwapItems(i1, i2, iSel);
			UpdateColumns(0);
			SetModified();

			EnableButtonUpDown(-1);
		}
	}
}

BOOL CPropertyPageListOfDeals::OnApply()
{
	UpdateSettings(GET);

	SetModified(FALSE);

	CWindow wnd = GetParent();
	if (wnd)
		wnd.SendMessage(UM_APPLY, this->IDD, (LPARAM)&m_settings);

	return CPropertyPageListOfDealsBase::OnApply();
}

BOOL CPropertyPageListOfDeals::OnQueryCancel()
{
	UpdateSettings(GET);

	CWindow wnd = GetParent();
	if (wnd)
		wnd.SendMessage(UM_CANCEL, this->IDD, (LPARAM)&m_settings);

	return CPropertyPageListOfDealsBase::OnQueryCancel();
}

LRESULT CPropertyPageListOfDeals::OnNMDblclkListColumns(NMHDR *pNMHDR)
{
	LPNMITEMACTIVATE pNMItemActivate = reinterpret_cast<LPNMITEMACTIVATE>(pNMHDR);
	
	int iItem = pNMItemActivate->iItem;
	int check = m_listColumns.GetCheckState(iItem);
	check = !check;
	m_listColumns.SetCheckState(iItem, check);
	
	return 0;
}

LRESULT CPropertyPageListOfDeals::OnNMClickListColumns(NMHDR *pNMHDR)
{
	LPNMITEMACTIVATE pNMItemActivate = reinterpret_cast<LPNMITEMACTIVATE>(pNMHDR);
#if 0
	if (pNMItemActivate->iSubItem == 0)
	{
		if (m_initialized)
		{
#if 1
			if (pNMItemActivate->iItem == 0)
				m_listColumns.SetCheck(pNMItemActivate->iItem, TRUE);
			else
#endif
			{
				UpdateColumnsInv(0, pNMItemActivate->iItem);
				SetModified();
			}
		}
	}
#endif
	return 0;
}

LRESULT CPropertyPageListOfDeals::OnLvnItemchangedListColumns(NMHDR *pNMHDR)
{
	LPNMLISTVIEW pNMLV = reinterpret_cast<LPNMLISTVIEW>(pNMHDR);

	if (!m_updating && m_initialized)
	{//LVIS_SELECTED
		BOOL updateButtonsUpDown = FALSE;
		int iItem = pNMLV->iItem;
		if ((pNMLV->uNewState == 4096 && pNMLV->uOldState == 8192) || (pNMLV->uNewState == 8192 && pNMLV->uOldState == 4096))// || 
			//(pNMLV->uNewState == 3 && pNMLV->uOldState == 0))// pNMLV->iSubItem == 0) // pNMLV->uOldState == 0 && 
		{// Изменение checkbox
#if 0
			if (pNMLV->iItem == 0)
				m_listColumns.SetCheck(pNMLV->iItem, TRUE);
			else
#endif
			{
				UpdateColumns(0);
				SetModified();
				//updateButtonsUpDown = TRUE;
			}
		}
		else if (pNMLV->uNewState == 3 && pNMLV->uOldState == 0)
		{
			CComboBox combo = GetDlgItem(IDC_COMBO1);
			CWindow wnd = GetDlgItem(IDC_STATIC_ALIGNMENT);
			wnd.EnableWindow();
			combo.EnableWindow();
			combo.SetCurSel(m_settings.columns.item[iItem + COL_FIRST].format);			
			updateButtonsUpDown = TRUE;
		}
		if (updateButtonsUpDown)
		{
			EnableButtonUpDown(iItem);
		}
	} //if (! m_updating)

	return 0;
}

void CPropertyPageListOfDeals::OnPaint(CDCHandle )
{
	CPaintDC dc(*this); // device context for painting

	CWindow wnd;
	int iItem = my::ctrl::GetFocusedItem(m_listColors);
	if (iItem < 0)
		return;
	
	const Settings::Presentation::ListOfDeals::Color * pColor = &m_settings.colors.item[iItem];

	// Цвет фона:
	wnd = GetDlgItem(IDC_COLOR_BACKGROUND);
	my::Painter::DrawColorPicker(wnd, pColor->backgrnd);
	// Цвет текста:
	wnd = GetDlgItem(IDC_COLOR_TEXT);
	my::Painter::DrawColorPicker(wnd, pColor->text);
}

void CPropertyPageListOfDeals::OnColor(DWORD & color)
{
	CColorDialog dlg(color, CC_ANYCOLOR|CC_FULLOPEN, *this);
	if (IDOK == dlg.DoModal())
	{
		if (color != dlg.GetColor())
		{
			color = dlg.GetColor();

			SetModified();
#if 1
			m_listColors.SetFocus();
#endif
			CWindow wndFocus = GetFocus();
			Invalidate2(wndFocus);
		}
	}
}

void CPropertyPageListOfDeals::OnStnClickedColorBackground(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	int iItem = my::ctrl::GetFocusedItem(m_listColors);
	if (iItem >= 0)
	{
		Settings::Presentation::ListOfDeals::Color * pColor = &m_settings.colors.item[iItem];
		OnColor(pColor->backgrnd);
	}
}

void CPropertyPageListOfDeals::OnStnClickedColorText(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	int iItem = my::ctrl::GetFocusedItem(m_listColors);
	if (iItem >= 0)
	{
		Settings::Presentation::ListOfDeals::Color * pColor = &m_settings.colors.item[iItem];
		OnColor(pColor->text);
	}
}

void CPropertyPageListOfDeals::OnBnClickedCheckUseDifferentColors(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	CButton btn(GetDlgItem(IDC_CHECK_USE_DIFFERENT_COLORS));
	int checked = (btn.GetCheck() == BST_CHECKED) ? TRUE : FALSE;
	EnableBackground(checked);
	m_settings.colors.useDifferentColors = checked;
	SetModified();
	Invalidate2(m_listColors);
	m_listColors.Invalidate();
}

void CPropertyPageListOfDeals::OnBnClickedCheckUseDifferentColorsText(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	CButton btn(GetDlgItem(IDC_CHECK_USE_DIFFERENT_COLORS_TEXT));
	int checked = (btn.GetCheck() == BST_CHECKED) ? TRUE : FALSE;
	EnableColorText(checked);
	m_settings.colors.useDifferentColorsText = checked;
	SetModified();
	Invalidate2(m_listColors);
	m_listColors.Invalidate();
}

void CPropertyPageListOfDeals::OnCbnSelchangeComboAlignment(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	int iItem = my::ctrl::GetFocusedItem(m_listColumns);
	if (iItem >= 0)
	{
		CComboBox combo = GetDlgItem(IDC_COMBO1);
		int i = combo.GetCurSel();
		if (i >= 0)
		{
			int index = static_cast<int>(m_listColumns.GetItemData(iItem));
			m_settings.columns.item[iItem + COL_FIRST].format = i;
			SetModified();
		}
	}
}

LRESULT CPropertyPageListOfDeals::OnCmdMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = FALSE;

	int nCode = HIWORD(wParam);
	if (nCode == 0)
	{
		UINT nID = LOWORD(wParam);
		if (nID >= ID_LISTOFDEALS_COLORS_00 && nID <= ID_LISTOFDEALS_COLORS_00 + 17)
		{
			ApplyColors(nID - ID_LISTOFDEALS_COLORS_00);
			bHandled = TRUE;
		}
		else if (nID == ID_LISTOFDELAS_DEFAULT_COLOR)
		{
			ResetColors();
			bHandled = TRUE;
		}		
	}
	return 0;
}

LRESULT CPropertyPageListOfDeals::OnLvnItemchangedListColors(NMHDR *pNMHDR)
{
	LPNMLISTVIEW pNMLV = reinterpret_cast<LPNMLISTVIEW>(pNMHDR);
	
	if (pNMLV->uOldState == 0)
	{
		int iItem = my::ctrl::GetFocusedItem(m_listColors);
		if (iItem >= 0)
		{
			CWindow wndText = GetDlgItem(IDC_STATIC_BACKGROUND);
			CWindow wndRect = GetDlgItem(IDC_COLOR_BACKGROUND);
			if (iItem == I_COLOR_FRAME)
			{
				wndText.EnableWindow(FALSE);
				wndRect.EnableWindow(FALSE);
			}
			else
			{
				wndText.EnableWindow();
				wndRect.EnableWindow();
			}
			Invalidate2(GetDlgItem(IDC_COLOR_TEXT));
		}
	}

	return 0;
}

LRESULT CPropertyPageListOfDeals::OnNMRClickListColors(NMHDR *pNMHDR)
{
	LPNMITEMACTIVATE pNMItemActivate = reinterpret_cast<LPNMITEMACTIVATE>(pNMHDR);

	if (m_listColors.GetItemCount() > 0)
	{
		CMenu root;
		root.LoadMenu(IDR_MENU_SETTINGS_LISTOFDEALS_COLORS);
		CMenu menu = root.GetSubMenu(0);
		CMenu subMenu = menu.GetSubMenu(0);
		if (subMenu)
		{// "Исправляем" названия:
			UINT flags = MF_BYPOSITION;
			// Удаляем все элементы меню:
			while (TRUE == subMenu.DeleteMenu(0, flags));
			// Вставляем элементы:
			LPCTSTR name;
			UINT id = ID_LISTOFDEALS_COLORS_00;
			int i;
			for (i = 0; (name = CPropertyPageListOfDeals::GetPropertyName(i)) != NULL; i++)
			{
				subMenu.InsertMenu(i, flags, id++, name);
				if (m_listColors.GetItemState(i, LVIS_SELECTED) == LVIS_SELECTED)
					subMenu.CheckMenuItem(i, MF_CHECKED|MF_BYPOSITION);
			} // for (i)

			subMenu.InsertMenu(i, flags|MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));
			subMenu.InsertMenu(i + 1, flags, ID_LISTOFDELAS_DEFAULT_COLOR, TEXT("По умолчанию"));
		}
		CPoint point;
		GetCursorPos(&point);
		menu.TrackPopupMenu(TPM_LEFTALIGN |TPM_RIGHTBUTTON, point.x, point.y, *this);
	}

	return 0;
}

BOOL CPropertyPageListOfDeals::OnSetActive()
{
	m_painter.SetPaint(F_DRAW_ALL);

	return CPropertyPageListOfDealsBase::OnSetActive();
}
