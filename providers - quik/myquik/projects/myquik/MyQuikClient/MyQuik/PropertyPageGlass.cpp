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
 *  PropertyPageGlass.cpp : implementation file
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "PropertyPageGlass.h"
#include <color.h>


CColumnSizer::CColumnSizer()
{
	m_length = 180;
	m_pParams = NULL;
	m_sizing = FALSE;
	m_markers.active = -1;
}

void CColumnSizer::Init(Settings::Presentation::Glass::Columns * pParams)
{
	m_pParams = pParams;
}

BOOL CColumnSizer::IsEnabled() const
{
#if 1
	return TRUE;
#endif
}

BOOL CColumnSizer::IsSizing() const
{
	return m_sizing;
}

int CColumnSizer::GetPerc(int & w1, int & w2, int & w3) const
{
	DWORD dwWidth = m_pParams->width;
	Settings::Presentation::Glass::GetColumnWidth(dwWidth, w1, w2, w3);
	return S_OK;
}

int CColumnSizer::Get(int & cx1, int & cx2, int & cx3) const
{
	int w1, w2, w3;
	GetPerc(w1, w2, w3);
	int length = m_length;
	cx1 = w1 * length / 100; cx2 = w2 * length / 100; cx3 = w3 * length / 100;
	return S_OK;
}

int CColumnSizer::Get(CPoint * pLeft, CPoint * pRight, CRect * pMarker0, CRect * pMarker1, CRect * pMarker2, 
					  CRect * pActive1, CRect * pActive2) const
{
	CRect rect;
	this->GetClientRect(&rect);

	int x1, x2;
	int yMiddle = rect.top + rect.Height() / 2;
	const int length = m_length;

	// Средняя линия:
	x1 = rect.left + (rect.Width() - length) / 2; x2 = x1 + length;

	// Маркеры:
	CSize markerSize(5, 5);
	CRect rct(CPoint(x1 - markerSize.cx/2, yMiddle - markerSize.cy/2), markerSize);
	CRect rect1, rect2, rect0; 
	rect1 = rct; 
	rect2 = rct; rect2.left += length; rect2.right += length;
	
	if (pMarker0)
		*pMarker0 = rct;

	// Подвижные маркеры:
	int w1, w2, w3;
	int cx1, cx2, cx3;
	GetPerc(w1, w2, w3);
	Get(cx1, cx2, cx3);

	rect1 = rct; rect1.left += cx1; rect1.right += cx1;
	rect2 = rct; rect2.left = x2 - cx3 - markerSize.cx/2; rect2.right = rect2.left + rct.Width();

	// Активный маркер:
	int dd = 2;
	rect0 = rect1;
	rect0.InflateRect(dd, dd);

	CRect rct02;
	rct02 = rect2;
	rct02.InflateRect(dd, dd);

	if (pLeft)
	{
		pLeft->x = x1;
		pLeft->y = yMiddle;
	}
	if (pRight)
	{
		pRight->x = x2;
		pRight->y = yMiddle;
	}
	
	if (pMarker1)
		*pMarker1 = rect1;
	if (pMarker2)
		*pMarker2 = rect2;

	if (pActive1)
		*pActive1 = rect0;
	if (pActive2)
		*pActive2 = rct02;

	return S_OK;
}

int CColumnSizer::SetMarker(const CPoint * pPoint)
{
	int i = GetActiveMarker();
	if (i >= 0)
	{
		CPoint point, left, right;
		CRect marker1, marker2;
		int cx1, cx2;
		int w, w1, w2, w3;
		GetPerc(w1, w2, w3);
		int length = m_length;
		point = *pPoint;
		this->Get(&left, &right, NULL, &marker1, &marker2, NULL, NULL);
		if (i == 0)
		{
			if (point.x < left.x)
				point.x = left.x;
			if (point.x > marker2.left)
				point.x = marker2.left;
			cx1 = point.x - left.x;
			w = cx1 * 100 / length;
			w2 += w1 - w;
			w1 = w;
		}
		else
		{
			if (point.x < marker1.right)
				point.x = marker1.right;
			if (point.x > right.x)
				point.x = right.x;
			cx2 = point.x - (marker1.left + marker1.Width()/2);
			w = cx2 * 100 / length;
			w3 += w2 - w;
			w2 = w;
		}
		m_pParams->width = Settings::Presentation::Glass::MakeColumnWidth(w1, w2, w3);
		
		return S_OK;
	}
	else
		return -1;
}

//
// CPropertyPageGlass dialog
//
CPropertyPageGlass::CPropertyPageGlass(const Settings::Presentation::Glass & settings)
{
	m_settings = settings;
	m_initialized = 0;
	m_canModify = FALSE;
}

CPropertyPageGlass::~CPropertyPageGlass()
{
}

void CPropertyPageGlass::UpdateSettings(int set)
{
	UpdateItemSettings(set);
}

void CPropertyPageGlass::UpdateItemSettings(int set, int index)
{
	if (index < 0)
		index = GetCurrentIndex();
	switch (index)
	{
	case GLASS_ITEM_VIEW:
		UpdateSettingsView(set);
		break;	
	case GLASS_ITEM_COLUMNS:
		break;
	case GLASS_ITEM_USER:
		UpdateSettingsUser(set);
		break;
	case GLASS_ITEM_MARKER:
		UpdateSettingsMarker(set);
		break;
	case GLASS_ITEM_GRID:
		UpdateSettingsGrid(set);
		break;
	case GLASS_ITEM_BORDER:
		UpdateSettingsBorder(set);
		break;
	case GLASS_ITEM_COMMON:
	default:
		{
			Settings::GlassItem & item = m_settings.items[index];
			UpdateSettingsText(set, item.text);
			UpdateSettingsBackgrnd(set, item.backgrnd);
			if (index == GLASS_ITEM_COMMON)
				UpdateSettingsGroupDigits(set, item.text);
			else if (index == GLASS_ITEM_BUY || index == GLASS_ITEM_SELL)
				UpdateSettingsAlignment(set, item.u.alignment.value, item.u.alignment.enable);
			else if (index == GLASS_ITEM_BUY_VOLUME || index == GLASS_ITEM_SELL_VOLUME)
				UpdateSettingsVolumeIndication(set, item);
		}
		break;
	} // switch (index)
}

void CPropertyPageGlass::UpdateSettingsText(int set, Settings::Presentation::GlassItem::Text & text)
{
	CComboBox combo;
	TSTRING_SMALL2(name, size);
	int i;

	// Название шрифта:
	combo = GetDlgItem(IDC_COMBO_FONT_NAME);
	if (set)
	{
		i = combo.FindStringExact(-1, text.cf.szFaceName);
		if (i != CB_ERR)
			combo.SetCurSel(i);
	}
	else
	{
		combo.GetWindowText(name, size);
		SAFE_TSTRCPY(text.cf.szFaceName, name);	
	}	

	// Начертание:
	combo = GetDlgItem(IDC_COMBO_FONT_STYLE);
	UpdateComboBoxOutline(combo, text.cf.dwEffects, set);

	// Размер:
	combo = GetDlgItem(IDC_COMBO_FONT_SIZE);
	if (set)
	{
		int pts = text.cf.yHeight / 20;
		_stprintf_s(name, size, TEXT("%d"), pts);
		i = combo.FindStringExact(-1, name);
		if (i != CB_ERR)
			combo.SetCurSel(i);
		else
			combo.SetWindowText(name);
	}
	else
	{
		i = combo.GetCurSel();
		if (i >= 0)
		{
			combo.GetLBText(i, name);
			//combo.GetWindowText(name);
		
			int pts = ::StrToInt(name);
			text.cf.yHeight = pts * 20;
		}
	}

	CButton btn = GetDlgItem(IDC_CHECKBOX_TEXT_1);
	if (set)
		btn.SetCheck(text.val1 ? BST_CHECKED : BST_UNCHECKED);
	else
		text.val1 = (btn.GetCheck() == BST_CHECKED);	

	if (set)
	{
		UINT id = IDC_CHECKBOX_TEXT;
		CButton btn(GetDlgItem(id));
		btn.SetCheck(text.enable ? BST_CHECKED : BST_UNCHECKED);
		EnableGroup(&CPropertyPageGlass::GetIdGroupText, text.enable, id, IDC_GLASS_FRAME_TEXT);
		Invalidate2(GetDlgItem(IDC_FONT_COLOR), F_DRAW_TEXT_COLOR);
	}
}

void CPropertyPageGlass::UpdateSettingsGroupDigits(int set, Settings::Presentation::GlassItem::Text & text)
{
	CButton btn = GetDlgItem(IDC_CHECK_GROUP_DIGITS);
	if (set)
		btn.SetCheck(text.val1 ? BST_CHECKED : BST_UNCHECKED);
	else
		text.val1 = (btn.GetCheck() == BST_CHECKED);
}

void CPropertyPageGlass::UpdateSettingsBackgrnd(int set, Settings::Presentation::GlassItem::Background & backgrnd)
{
	TSTRING_SMALL2(text, size);

	CWindow wnd = GetDlgItem(IDC_EDIT_GLASS_BACKGRND);
	if (set)
	{
		_stprintf_s(text, size, TEXT("%d%%"), backgrnd.opacity);
		EnableModify(FALSE);
		wnd.SetWindowText(text);
		EnableModify();
	}
	else
	{
		wnd.GetWindowText(text, size);
		backgrnd.opacity = StrToInt(text);
		if (backgrnd.opacity > 100)
			backgrnd.opacity = 100;
	}

	CButton btn = GetDlgItem(IDC_CHECKBOX_BACKGRND_1);
	if (set)
		btn.SetCheck(backgrnd.val1 ? BST_CHECKED : BST_UNCHECKED);
	else
		backgrnd.val1 = (btn.GetCheck() == BST_CHECKED);

	if (set)
	{
		UINT id = IDC_CHECKBOX_BACKGRND;
		CButton btn(GetDlgItem(id));
		btn.SetCheck(backgrnd.enable ? BST_CHECKED : BST_UNCHECKED);
		EnableGroup(&CPropertyPageGlass::GetIdGroupBackgrnd, backgrnd.enable, id, IDC_GLASS_FRAME_BACKGRND);
		Invalidate2(GetDlgItem(IDC_GLASS_BACKGRND_COLOR), F_DRAW_GLASS_BACKGRND_COLOR);
	}
}

void CPropertyPageGlass::UpdateSettingsText1(int set, int i)
{
	CButton btn = GetDlgItem(IDC_CHECKBOX_TEXT_1);
	if (i < 0)
		i = GetCurrentIndex();
	DWORD & val = m_settings.items[i].text.val1;
	if (set)
		btn.SetCheck(val ? BST_CHECKED : BST_UNCHECKED);
	else
		val = (btn.GetCheck() == BST_CHECKED);
}

void CPropertyPageGlass::UpdateSettingsBackgrnd1(int set, int i)
{
	CButton btn = GetDlgItem(IDC_CHECKBOX_BACKGRND_1);
	if (i < 0)
		i = GetCurrentIndex();
	DWORD & val = m_settings.items[i].backgrnd.val1;
	if (set)
		btn.SetCheck(val ? BST_CHECKED : BST_UNCHECKED);
	else
		val = (btn.GetCheck() == BST_CHECKED);
}

void CPropertyPageGlass::UpdateSettingsView(int set)
{
	UpdateSettingsStyle(set);
	int style = m_settings.view.style;
	DWORD & flags = m_settings.view.flags;

	CButton btn;

	if (! set)
		flags = 0;
	
	btn = GetDlgItem(IDC_CHECK_BUY_ABOVE);
	if (set)
		btn.SetCheck((flags & F_GLASS_VIEW_BUY_UP) ? BST_CHECKED : BST_UNCHECKED);
	else if (btn.GetCheck() == BST_CHECKED)
		flags |= F_GLASS_VIEW_BUY_UP;

	btn = GetDlgItem(IDC_CHECK_FLIP_HORIZ);
	if (set)
	{
		btn.SetButtonStyle((style == GLASS_STYLE_3) ? BS_AUTO3STATE : BS_AUTOCHECKBOX);
		btn.SetCheck(m_settings.view.flipHorizontal);
	}
	else
	{
		int state = btn.GetCheck();
		m_settings.view.flipHorizontal = state;
	}

	btn = GetDlgItem(IDC_CHECK_RAREFY);
	if (set)
		btn.SetCheck((flags & F_GLASS_VIEW_RAREFIED) ? BST_CHECKED : BST_UNCHECKED);
	else if(btn.GetCheck() == BST_CHECKED)
		flags |= F_GLASS_VIEW_RAREFIED;

	btn = GetDlgItem(IDC_CHECK_NEUTRAL_ZONE);
	if (set)
		btn.SetCheck((flags & F_GLASS_VIEW_NEUTRAL_ZONE) ? BST_CHECKED : BST_UNCHECKED);
	else if(btn.GetCheck() == BST_CHECKED)
		flags |= F_GLASS_VIEW_NEUTRAL_ZONE;
}

void CPropertyPageGlass::UpdateSettingsStyle(int set, int index)
{
	UINT ids[] = {IDC_RADIO_STYLE1, IDC_RADIO_STYLE2, IDC_RADIO_STYLE3};
	int & style = m_settings.view.style;
	if (index < 0)
		index = style;
	CButton radio = GetDlgItem(ids[index]);
	if (set)
	{
		radio.SetCheck(BST_CHECKED);
		UpdateTextBuyAbove(style);
	}
	else
		style = index;
}

void CPropertyPageGlass::UpdateSettingsGrid(int set)
{
	UpdateSettingsGridLines(set);
	UpdateSettingsMargins(set);
}

void CPropertyPageGlass::UpdateSettingsGridLines(int set)
{
	Settings::Presentation::Glass::Grid & grid = m_settings.grid;
	CButton btnV = GetDlgItem(IDC_CHECKBOX_GRID_LINES_V);
	CButton btnH = GetDlgItem(IDC_CHECKBOX_GRID_LINES_H);
	if (set)
	{
		btnV.SetCheck((grid.lines & GRID_LINES_VERTICAL) ? BST_CHECKED : BST_UNCHECKED);
		btnH.SetCheck((grid.lines & GRID_LINES_HORIZONTAL) ? BST_CHECKED : BST_UNCHECKED);
	}
	else
	{
		grid.lines = 0;
		if (btnV.GetCheck() == BST_CHECKED)
			grid.lines |= GRID_LINES_VERTICAL;
		if (btnH.GetCheck() == BST_CHECKED)
			grid.lines |= GRID_LINES_HORIZONTAL;
	}
	TSTRING_SMALL2(text, size);
	CWindow wnd = GetDlgItem(IDC_EDIT_GLASS_BACKGRND);
	if (set)
	{
		_stprintf_s(text, size, TEXT("%d"), grid.width);
		EnableModify(FALSE);
		wnd.SetWindowText(text);
		EnableModify(TRUE);
	}
	else
	{
		wnd.GetWindowText(text, size);
		grid.width = StrToInt(text);
	}

	if (set)
	{
		CButton btn(GetDlgItem(IDC_CHECKBOX_GRID));
		btn.SetCheck(grid.show ? BST_CHECKED : BST_UNCHECKED);
		EnableGroup(&CPropertyPageGlass::GetIdGroupBackgrnd, grid.show, IDC_CHECKBOX_BACKGRND, IDC_GLASS_FRAME_BACKGRND);

		my::wnd::Enable(btnV, grid.show);
		my::wnd::Enable(btnH, grid.show);
	}
}

void CPropertyPageGlass::UpdateSettingsMargins(int set)
{
	Settings::Presentation::Glass::Margins & margins = m_settings.margins;
	CComboBox combo = GetDlgItem(IDC_COMBO_GLASS1);
	CWindow wnd = GetDlgItem(IDC_EDIT_GLASS);
	TSTRING_SMALL2(text, size);
	int vals[4];
	Settings::Presentation::Glass::GetMargins(margins.value, vals[0], vals[1], vals[2], vals[3]);
	int i = combo.GetCurSel();
	if (set)
	{
		_stprintf_s(text, size, TEXT("%d"), vals[i]);
		EnableModify(FALSE);
		wnd.SetWindowText(text);
		EnableModify(TRUE);
	}
	else
	{
		wnd.GetWindowText(text, size);
		vals[i] = StrToInt(text);
		margins.value = Settings::Presentation::Glass::MakeMargins(vals[0], vals[1], vals[2], vals[3]);
	}

	if (set)
	{
		CButton btn(GetDlgItem(IDC_CHECKBOX_MARGINS));
		btn.SetCheck(margins.enable ? BST_CHECKED : BST_UNCHECKED);
		EnableGroup(&CPropertyPageGlass::GetIdGroupGlass, margins.enable, IDC_CHECKBOX_GLASS, IDC_GLASS_FRAME);
	}
}

void CPropertyPageGlass::UpdateSettingsBorder(int set)
{
	CWindow wnd;
	TSTRING_SMALL2(text, size);
	// Граница заявок:
	Settings::Presentation::Glass::Border & border = m_settings.border;
	wnd = GetDlgItem(IDC_EDIT_GLASS);

	CButton btn0 = GetDlgItem(IDC_CHECKBOX_GRID_LINES_V);
	CButton btn1 = GetDlgItem(IDC_CHECKBOX_GRID_LINES_H);

	if (set)
	{
		_stprintf_s(text, size, TEXT("%d"), border.width);
		EnableModify(FALSE);
		wnd.SetWindowText(text);
		EnableModify();

		btn0.SetCheck((border.lines & BORDER_LINES_BORDERS) ? BST_CHECKED : BST_UNCHECKED);
		btn1.SetCheck((border.lines & BORDER_LINES_MEDIAN) ? BST_CHECKED : BST_UNCHECKED);
	}
	else
	{		
		wnd.GetWindowText(text, size);
		border.width = StrToInt(text);

		border.lines = 0;
		if (btn0.GetCheck() == BST_CHECKED)
			border.lines |= BORDER_LINES_BORDERS;
		if (btn1.GetCheck() == BST_CHECKED)
			border.lines |= BORDER_LINES_MEDIAN;
	}

	if (set)
	{
		CButton btn(GetDlgItem(IDC_CHECKBOX_BORDER));
		btn.SetCheck(border.show ? BST_CHECKED : BST_UNCHECKED);
		EnableGroup(&CPropertyPageGlass::GetIdGroupGlass, border.show, IDC_CHECKBOX_GLASS, IDC_GLASS_FRAME);

		my::wnd::Enable(btn0, border.show); my::wnd::Enable(btn1, border.show);
	}
}

void CPropertyPageGlass::UpdateSettingsMarker(int set)
{
	CWindow wnd;
	TSTRING_SMALL2(text, size);
	// Маркер:
	Settings::Presentation::Glass::Marker & marker = m_settings.marker;
	wnd = GetDlgItem(IDC_EDIT_GLASS);

	CButton btn1 = GetDlgItem(IDC_CHECKBOX_GRID_LINES_V);
	CButton btn2 = GetDlgItem(IDC_CHECKBOX_GRID_LINES_H);
	CButton btn3 = GetDlgItem(IDC_CHECKBOX3);

	if (set)
	{
		_stprintf_s(text, size, TEXT("%d"), marker.width);
		EnableModify(FALSE);
		wnd.SetWindowText(text);
		EnableModify();

		btn1.SetCheck((marker.follow & MARKER_FOLLOW_PRICE) ? BST_CHECKED : BST_UNCHECKED);
		btn2.SetCheck((marker.follow & MARKER_FOLLOW_CURSOR) ? BST_CHECKED : BST_UNCHECKED);
		btn3.SetCheck((marker.follow & MARKER_FOLLOW_MENU) ? BST_CHECKED : BST_UNCHECKED);
	}
	else
	{		
		wnd.GetWindowText(text, size);
		marker.width = StrToInt(text);

		marker.follow = 0;
		if (btn1.GetCheck() == BST_CHECKED)
			marker.follow |= MARKER_FOLLOW_PRICE;
		if (btn2.GetCheck() == BST_CHECKED)
			marker.follow |= MARKER_FOLLOW_CURSOR;
		if (btn3.GetCheck() == BST_CHECKED)
			marker.follow |= MARKER_FOLLOW_MENU;
	}

	if (set)
	{
		CButton btn(GetDlgItem(IDC_CHECKBOX_MARKER));
		btn.SetCheck(marker.show ? BST_CHECKED : BST_UNCHECKED);
		EnableGroup(&CPropertyPageGlass::GetIdGroupGlass, marker.show, IDC_CHECKBOX_GLASS, IDC_GLASS_FRAME);

		my::wnd::Enable(btn1, marker.show); my::wnd::Enable(btn2, marker.show); my::wnd::Enable(btn3, marker.show);
	}
}

void CPropertyPageGlass::UpdateSettingsUser(int set)
{
	DWORD & flags = m_settings.user.flags;
	if (! set)
		flags = 0;

	CButton btn;
	// Показывать активные заявки:
	btn = GetDlgItem(IDC_CHECK_SHOW_USER_BIDS);
	if (set)
		btn.SetCheck((flags & F_USER_SHOW_ACTIVE_BIDS) ? BST_CHECKED : BST_UNCHECKED);
	else if (btn.GetCheck() == BST_CHECKED)
		flags |= F_USER_SHOW_ACTIVE_BIDS;
	// Стоп-заявки:
	btn = GetDlgItem(IDC_CHECK_SHOW_USER_STOPS);
	if (set)
		btn.SetCheck((flags & F_USER_SHOW_ACTIVE_STOPS) ? BST_CHECKED : BST_UNCHECKED);
	else if (btn.GetCheck() == BST_CHECKED)
		flags |= F_USER_SHOW_ACTIVE_STOPS;
	// Активные сделки:
	btn = GetDlgItem(IDC_CHECK_SHOW_USER_DEALS);
	if (set)
		btn.SetCheck((flags & F_USER_SHOW_ACTIVE_DEALS) ? BST_CHECKED : BST_UNCHECKED);
	else if (btn.GetCheck() == BST_CHECKED)
		flags |= F_USER_SHOW_ACTIVE_DEALS;
	// Показывать значки:
	btn = GetDlgItem(IDC_CHECK_SHOW_USER_ICONS);
	if (set)
		btn.SetCheck((flags & F_USER_SHOW_ICONS) ? BST_CHECKED : BST_UNCHECKED);
	else if (btn.GetCheck() == BST_CHECKED)
		flags |= F_USER_SHOW_ICONS;
	// Отдельный столбец:
	btn = GetDlgItem(IDC_CHECK_SHOW_USER_COL);
	if (set)
		btn.SetCheck((flags & F_USER_SHOW_USER_COLUMN) ? BST_CHECKED : BST_UNCHECKED);
	else if (btn.GetCheck() == BST_CHECKED)
		flags |= F_USER_SHOW_USER_COLUMN;
	// Группироват одинаковые цены:
	btn = GetDlgItem(IDC_CHECKBOX_USER_GROUP_BY_PRICE);
	if (set)
		btn.SetCheck((flags & F_USER_GROUP_BY_PRICE) ? BST_CHECKED : BST_UNCHECKED);
	else if (btn.GetCheck() == BST_CHECKED)
		flags |= F_USER_GROUP_BY_PRICE;
}

void CPropertyPageGlass::UpdateSettingsAlignment(int set, int i)
{
	Settings::Presentation::GlassItem & item = m_settings.items[i];
	UpdateSettingsAlignment(set, item.u.alignment.value, item.u.alignment.enable);
}

void CPropertyPageGlass::UpdateSettingsAlignment(int set, DWORD & alignment, BOOL enable)
{
	CComboBox comboName = GetDlgItem(IDC_COMBO_GLASS1);
	CComboBox comboAlignment = GetDlgItem(IDC_COMBO_GLASS2);
	int cols[3];
	Settings::Presentation::Glass::GetColumnAlignment(alignment, cols[0], cols[1], cols[2]);
	int i = comboName.GetCurSel();
	if (set)
	{
		comboAlignment.SetCurSel(cols[i]);
	}
	else
	{
		cols[i] = comboAlignment.GetCurSel();
		alignment = Settings::Presentation::Glass::MakeColumnAlignment(cols[0], cols[1], cols[2]);
	}

	if (set)
	{
		CButton btn(GetDlgItem(IDC_CHECKBOX_ALIGNMENT));
		btn.SetCheck(enable ? BST_CHECKED : BST_UNCHECKED);
		EnableGroup(&CPropertyPageGlass::GetIdGroupGlass, enable, IDC_CHECKBOX_GLASS, IDC_GLASS_FRAME);
	}
}

void CPropertyPageGlass::UpdateSettingsVolumeIndication(int set, int i)
{
	Settings::Presentation::GlassItem & item = m_settings.items[i];
	UpdateSettingsVolumeIndication(set, item);
}

void CPropertyPageGlass::UpdateSettingsVolumeIndication(int set, Settings::Presentation::GlassItem & item)
{
	CComboBox comboMargins = GetDlgItem(IDC_COMBO_GLASS1);
	CComboBox comboAlignment = GetDlgItem(IDC_COMBO_GLASS2);
	CWindow wnd = GetDlgItem(IDC_EDIT_GLASS);
	TSTRING_SMALL2(text, size);
	int vals[4];

	BOOL enable = item.other.enable;

	DWORD & alignment = item.u.values.value1;
	DWORD & margins = item.u.values.value2;

	Settings::Presentation::Glass::GetMargins(margins, vals[0], vals[1], vals[2], vals[3]);

	int i = comboMargins.GetCurSel();

	if (set)
	{
		// Выравнивание:
		comboAlignment.SetCurSel(alignment);
		// Отступы:
		_stprintf_s(text, size, TEXT("%d"), vals[i]);
		EnableModify(FALSE);
		wnd.SetWindowText(text);
		EnableModify(TRUE);
	}
	else
	{
		alignment = comboAlignment.GetCurSel();

		wnd.GetWindowText(text, size);
		vals[i] = StrToInt(text);
		margins = Settings::Presentation::Glass::MakeMargins(vals[0], vals[1], vals[2], vals[3]);
	}

	if (set)
	{
		CButton btn(GetDlgItem(IDC_CHECKBOX_INDICATION));
		btn.SetCheck(enable ? BST_CHECKED : BST_UNCHECKED);
		EnableGroup(&CPropertyPageGlass::GetIdGroupGlass, enable, IDC_CHECKBOX_GLASS, IDC_GLASS_FRAME);

		CWindow wnd = GetDlgItem(IDC_STATIC_GLASS_MARGINS);
		my::wnd::Enable(wnd, enable);
	}
}

void CPropertyPageGlass::UpdateTextBuyAbove(int style)
{
	CWindow wnd = GetDlgItem(IDC_CHECK_BUY_ABOVE);
	wnd.SetWindowText((style == GLASS_STYLE_3) ? TEXT("Покупки показывать слева") : TEXT("Покупки показывать сверху"));
}

void CPropertyPageGlass::EnableModify(BOOL enable)
{
	m_canModify = enable;
}

static int CALLBACK EnumFontFamExProc(ENUMLOGFONTEX *lpelfe, NEWTEXTMETRICEX *lpntme, DWORD FontType, LPARAM lParam)
{
	CComboBox * pCombo = (CComboBox*)lParam;
	if (pCombo)
	{
		LPCTSTR name = lpelfe->elfLogFont.lfFaceName;
		int i = pCombo->GetCount();
		if (CB_ERR == pCombo->FindStringExact(i, name))
			pCombo->AddString(name);
	}	
	return 1;
}

void CPropertyPageGlass::EnumerateSystemFonts()
{
	CComboBox combo (GetDlgItem(IDC_COMBO_FONT_NAME));
	combo.Clear();

	LOGFONT lf;
	lf.lfFaceName[0] = '\0';
	lf.lfCharSet = DEFAULT_CHARSET;

	::EnumFontFamiliesEx(GetWindowDC(), &lf, (FONTENUMPROC)EnumFontFamExProc, (LPARAM)&combo, 0);

	combo.SetCurSel(0);

	// Начертание шрифта:
	InitComboBoxOutline(IDC_COMBO_FONT_STYLE);

	// Размер шрифта:
	combo = GetDlgItem(IDC_COMBO_FONT_SIZE);
	combo.Clear();
	TCHAR str[MAX_PATH];
	for (int i = 6; i <= 24; i++)
	{
		wsprintf(str, TEXT("%d"), i);
		combo.AddString(str);
	} // for (i)
}

BOOL CPropertyPageGlass::InitComboBoxOutline(UINT id)
{
	CComboBox combo(GetDlgItem(id));
	combo.ResetContent();
	combo.AddString(TEXT("обычный"));
	combo.AddString(TEXT("курсив"));
	combo.AddString(TEXT("жирный"));
	combo.AddString(TEXT("жирный курсив"));
	//combo.SetCurSel(0);
	return TRUE;
}

BOOL CPropertyPageGlass::InitComboBoxMargins(UINT id)
{
	CComboBox combo (GetDlgItem(id));

	LPCTSTR names[] = {
		TEXT("Верхний"),
		TEXT("Нижний"),
		TEXT("Левый"),
		TEXT("Правый"),
	};
	combo.ResetContent();
	for (int i = 0; i < SIZEOF_ARRAY(names); i++)
		combo.AddString(names[i]);
	combo.SetCurSel(0);

	return TRUE;
}

BOOL CPropertyPageGlass::InitComboBoxColumns(UINT id)
{
	CComboBox combo (GetDlgItem(id));

	LPCTSTR names[] = {
		TEXT("Цена"),
		TEXT("Объём"),
		TEXT("Свои заявки"),
	};
	combo.ResetContent();
	for (int i = 0; i < SIZEOF_ARRAY(names); i++)
		combo.AddString(names[i]);
	combo.SetCurSel(0);
	return TRUE;
}

BOOL CPropertyPageGlass::InitComboBoxAlignment(UINT id)
{
	CComboBox combo (GetDlgItem(id));

	LPCTSTR names[] = {
		TEXT("Лево"),
		TEXT("Право"),
		TEXT("Центр"),
	};
	combo.ResetContent();
	for (int i = 0; i < SIZEOF_ARRAY(names); i++)
		combo.AddString(names[i]);
	return TRUE;
}

void CPropertyPageGlass::SetModified(BOOL bChanged)
{
#if 0
	if (m_canModify)
#endif
	{
		if (bChanged && m_initialized)
		{
			::SendMessage(theApp.GetMainWnd(), UM_APPLY_SETTINGS, APPLY_SETTINGS_PRESENTATION_GLASS, (LPARAM)&m_settings);
		}
		CPropertyPageGlassBase::SetModified(bChanged);
	}
}

LPCTSTR CPropertyPageGlass::GetItemName(int i) const
{
	LPCTSTR names[] = {
		TEXT("Вид окна"),
		TEXT("Базовые"),
		TEXT("Покупки"),
		TEXT("Продажи"),
		TEXT("Покупки: цена"),
		TEXT("Покупки: объём"),
		TEXT("Покупки: свои"),
		TEXT("Продажи: цена"),
		TEXT("Продажи: объём"),
		TEXT("Продажи: свои"),
		TEXT("Нейтральная зона"),
		TEXT("Нейтральная зона: цена"),
		TEXT("Нейтральная зона: объём"),
		TEXT("Нейтральная зона: свои"),
		TEXT("Свои заявки и сделки"),
		TEXT("Свои заявки (покупка)"),
		TEXT("Свои заявки (продажа)"),
		TEXT("Свои стоп-заявки (покупка)"),
		TEXT("Свои стоп-заявки (продажа)"),
		TEXT("Свои сделки (покупка)"),
		TEXT("Свои сделки (продажа)"),	
		TEXT("Минимальная цена сделки"),
		TEXT("Максимальная цена сделки"),
		TEXT("Цена последней сделки"),
		TEXT("Лучшая цена спроса"),
		TEXT("Лучшая цена предложения"),
		TEXT("Ширина столбцов (%)"),
		TEXT("Сетка и отступы"),
		TEXT("Граница заявок"),
		TEXT("Маркер"),
		NULL,
	};
	return names[i];
}

int CPropertyPageGlass::GetCurrentIndex() const
{
	CListBox listBox = GetDlgItem(IDC_LIST1);
	ASSERT(listBox);
	int i = listBox.GetCurSel();
	if (i == 0)
		i = GLASS_ITEM_VIEW;
	else
		i -= 1;
	return i;
}

Settings::Presentation::GlassItem::Text * CPropertyPageGlass::GetCurrentText()
{
	Settings::Presentation::GlassItem::Text * pText = NULL;
	int i = GetCurrentIndex();
	if (i >= GLASS_ITEM_COMMON && i < GLASS_ITEM_COLUMNS)
		if (i != GLASS_ITEM_USER)
			pText = &m_settings.items[i].text;
	return pText;
}

Settings::Presentation::GlassItem::Background * CPropertyPageGlass::GetCurrentBackgrnd(int i)
{
	Settings::Presentation::GlassItem::Background * pBackgrnd = NULL;
	if (i == -1)
		i = GetCurrentIndex();
	if (i >= GLASS_ITEM_COMMON && i < GLASS_ITEM_COLUMNS)
		if (i != GLASS_ITEM_USER)
			pBackgrnd = &m_settings.items[i].backgrnd;
	return pBackgrnd;
}

COLORREF * CPropertyPageGlass::GetCurrentColor(int backgrnd, int i)
{
	COLORREF * pColor = NULL;
	if (backgrnd)
	{
		Settings::Presentation::GlassItem::Background * pBackgrnd = GetCurrentBackgrnd(i);
		if (pBackgrnd)
			return &pBackgrnd->color;
	}
	if (i == -1)
		i = GetCurrentIndex();
	if (! backgrnd)
	{
		if (i == GLASS_ITEM_BUY_VOLUME || i == GLASS_ITEM_SELL_VOLUME)
		{
			return &m_settings.items[i].other.val1;
		}
	}
	if (i == GLASS_ITEM_GRID)
		pColor = &m_settings.grid.color;
	else if (i == GLASS_ITEM_BORDER)
		pColor = &m_settings.border.color;
	else if (i == GLASS_ITEM_MARKER)
		pColor = &m_settings.marker.color;

	return pColor;
}

UINT CPropertyPageGlass::GetIdGroupView(int i)
{
	UINT ids[] = {
		IDC_GLASS_FRAME_VIEW,
		IDC_RADIO_STYLE1, IDC_STATIC_PICT1, IDC_RADIO_STYLE2, IDC_STATIC_PICT2, IDC_RADIO_STYLE3, IDC_STATIC_PICT3, 
		IDC_CHECK_BUY_ABOVE, IDC_CHECK_FLIP_HORIZ, IDC_CHECK_RAREFY, IDC_CHECK_NEUTRAL_ZONE, 
		//IDC_CHECK_GROUP_DIGITS,
	};
	return (i < SIZEOF_ARRAY(ids)) ? ids[i] : 0;
}

UINT CPropertyPageGlass::GetIdGroupText(int i)
{
	UINT ids[] = {
		IDC_GLASS_FRAME_TEXT,
		IDC_CHECKBOX_TEXT, 
		IDC_STATIC_FONT_NAME, IDC_COMBO_FONT_NAME, IDC_STATIC_FONT_STYLE, IDC_COMBO_FONT_STYLE, 
		IDC_STATIC_FONT_SIZE, IDC_COMBO_FONT_SIZE, IDC_STATIC_FONT_COLOR, IDC_FONT_COLOR,
		IDC_CHECKBOX_TEXT_1,
		IDC_CHECK_GROUP_DIGITS,
	};
	return (i < SIZEOF_ARRAY(ids)) ? ids[i] : 0;
}

UINT CPropertyPageGlass::GetIdGroupBackgrnd(int i)
{
	UINT ids[] = {
		IDC_GLASS_FRAME_BACKGRND,
		IDC_CHECKBOX_BACKGRND, 
		IDC_STATIC_GLASS_BACKGRND_COLOR, IDC_GLASS_BACKGRND_COLOR, 
		IDC_STATIC_GLASS_OPACITY, IDC_EDIT_GLASS_BACKGRND, IDC_SPIN_GLASS_BACKGRND,
		IDC_CHECKBOX_BACKGRND_1,
	};
	return (i < SIZEOF_ARRAY(ids)) ? ids[i] : 0;
}

UINT CPropertyPageGlass::GetIdGroupUser(int i)
{
	UINT ids[] = {
		IDC_GLASS_FRAME_USER,
		IDC_CHECK_SHOW_USER_BIDS, IDC_CHECK_SHOW_USER_STOPS, IDC_CHECK_SHOW_USER_DEALS, 
		IDC_CHECK_SHOW_USER_ICONS,
		IDC_CHECK_SHOW_USER_COL,
		IDC_CHECKBOX_USER_GROUP_BY_PRICE,
	};
	return (i < SIZEOF_ARRAY(ids)) ? ids[i] : 0;
}

UINT CPropertyPageGlass::GetIdGroupGlass(int i)
{
	UINT ids[] = {
		IDC_GLASS_FRAME, IDC_CHECKBOX_GLASS,
		IDC_STATIC_GLASS_COLOR, IDC_STATIC_GLASS_WIDTH, 
		IDC_GLASS_COLOR, IDC_EDIT_GLASS, IDC_SPIN_GLASS, IDC_COMBO_GLASS1, IDC_COMBO_GLASS2, 
	};
	return (i < SIZEOF_ARRAY(ids)) ? ids[i] : 0;
}

void CPropertyPageGlass::ShowGroup(PFuncGetIdGroup func, int show, UINT except, int flags)
{
	for (int i = 0; ; i++)
	{
		UINT id = (this->*func)(i);
		if (id)
		{
			if (id != except)
			{
				CWindow wnd(GetDlgItem(id));
				my::wnd::Show(wnd, show, flags);
			}
		}
		else
			break;
	} // for (i)
}

void CPropertyPageGlass::EnableGroup(PFuncGetIdGroup func, BOOL enable, UINT except1, UINT except2)
{
	for (int i = 0; ; i++)
	{
		UINT id = (this->*func)(i);
		if (id)
		{
			if (id != except1 && id != except2)
			{
				CWindow wnd(GetDlgItem(id));
				my::wnd::Enable(wnd, enable);
			}
		}
		else
			break;
	} // for (i)
}

void CPropertyPageGlass::MoveGroup(PFuncGetIdGroup func, UINT src, UINT dst, int show)
{
	CWindow wnd;
	CRect rct, rctSrc, rctDst;
	wnd = GetDlgItem(src);
	wnd.GetWindowRect(&rctSrc);
	wnd = GetDlgItem(dst);
	wnd.GetWindowRect(&rctDst);
	ScreenToClient(&rctDst);

	for (int i = 0; ; i++)
	{
		UINT id = (this->*func)(i);
		if (id)
		{
			wnd = GetDlgItem(id);
			wnd.GetWindowRect(&rct);
			rct.OffsetRect(-rctSrc.left, -rctSrc.top);			
			int x = rctDst.left + rct.left; int y = rctDst.top + rct.top;
#if 0
			wnd.MoveWindow(x, y, rct.Width(), rct.Height(), TRUE);
#else		
			my::wnd::SetPos(wnd, x, y);
#endif
			my::wnd::Show(wnd, show);
		}
		else
			break;
	} // for (i)
}

void CPropertyPageGlass::MoveGroup(PFuncGetIdGroup func, UINT src, CRect dst, int show)
{
	CWindow wnd;
	CRect rct, rctSrc, rctDst;
	wnd = GetDlgItem(src);
	wnd.GetWindowRect(&rctSrc);
	rctDst = dst;

	for (int i = 0; ; i++)
	{
		UINT id = (this->*func)(i);
		if (id)
		{
			wnd = GetDlgItem(id);
			wnd.GetWindowRect(&rct);
			rct.OffsetRect(-rctSrc.left, -rctSrc.top);			
			int x = rctDst.left + rct.left; int y = rctDst.top + rct.top;
#if 0
			wnd.MoveWindow(x, y, rct.Width(), rct.Height(), TRUE);
#else		
			my::wnd::SetPos(wnd, x, y);
#endif
			my::wnd::Show(wnd, show);
		}
		else
			break;
	} // for (i)
}

int CPropertyPageGlass::DrawColumnSizer(CWindow wnd)
{
	int result;
	if (wnd)
	{
		CPen pen;
		CBrush brush;
		COLORREF color;
		CRect rect;

		BOOL enabled = m_columnSizer.IsEnabled();

		wnd.GetClientRect(&rect);

		wnd.InvalidateRect(&rect);
		wnd.UpdateWindow();

		CDC dc = wnd.GetDC();

		dc.SetBkMode(TRANSPARENT);

		CBrush background;
		COLORREF colorBackground;
#if 0
		colorBackground = ::GetSysColor(enabled ? COLOR_BTNFACE : COLOR_WINDOW)); // COLOR_WINDOW COLOR_BTNFACE
#else
		colorBackground = ::GetSysColor(COLOR_WINDOW);
#endif
		background.CreateSolidBrush(colorBackground);
		dc.SelectBrush(background);

#if 0
		color = ::GetSysColor(enabled ? COLOR_3DDKSHADOW : COLOR_ACTIVEBORDER);
#else
		color = ::GetSysColor(enabled ? COLOR_WINDOWTEXT : COLOR_3DDKSHADOW);
		//color = al::lib::AddColor(color, RGB(80, 80, 80));
#endif
		brush.CreateSolidBrush(color);

		//CPen penNull(PS_NULL, 1, (COLORREF)0);
		//dc.SelectObject(&penNull);
		dc.FillRect(&rect, background);
		
		pen.CreatePen(PS_SOLID, 1, color);
		dc.SelectPen(pen);
		
		int w1, w2, w3;
		int cx1, cx2, cx3;
		CPoint left, right;
		CRect rct, rect1, rect2;
		CRect rctActive1, rctActive2;

		// Получаем параметры:
		m_columnSizer.GetPerc(w1, w2, w3);
		m_columnSizer.Get(cx1, cx2, cx3);
		m_columnSizer.Get(&left, &right, &rct, &rect1, &rect2, &rctActive1, &rctActive2);
		
		// Проводим среднюю линию:
		dc.MoveTo(left.x, left.y);
		dc.LineTo(right.x, right.y);

		// Рисуем маркеры:
		CRect rctLeft(rct);
		CRect rctRight(rct);
		rctRight.left += right.x - left.x; rctRight.right += right.x - left.x; 
		dc.FillRect(&rctLeft, brush);
		dc.FillRect(&rctRight, brush);

		// Подвижные маркеры:
		CPen penHighlight;
		penHighlight.CreatePen(PS_SOLID, 1, ::GetSysColor(COLOR_HIGHLIGHT));
		dc.SelectPen(penHighlight);
		if (enabled)
		{
			// Активный маркер:
			if (m_columnSizer.GetActiveMarker() == 0)
				dc.Rectangle(&rctActive1);
		}
		dc.FillRect(&rect1, brush);
		if (enabled)
		{
			rect1.DeflateRect(1, 1);
			dc.FillRect(&rect1, background);
		}

		if (enabled)
		{
			// Активный маркер:
			if (m_columnSizer.GetActiveMarker() == 1)
				dc.Rectangle(&rctActive2);
		}
		dc.FillRect(&rect2, brush);
		if (enabled)
		{
			rect2.DeflateRect(1, 1);
			dc.FillRect(&rect2, background);
		}

		// Подписи:
		TSTRING_SMALL2(text, textSize);
		CFont font;
		CFont * pFont = &font;
		BYTE bold = FALSE;
		BYTE italic = FALSE;
		int pts = 6;
		int lfHeight = -MulDiv(pts, dc.GetDeviceCaps(LOGPIXELSY), 72);
		pFont->CreateFont(lfHeight, 0, 0, 0, bold ? FW_BOLD : FW_NORMAL, italic, FALSE, 0, DEFAULT_CHARSET, 
			OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH|FF_SWISS,
			TEXT("Tahoma"));		
	
		dc.SelectFont(*pFont);
		dc.SetTextColor(color);

		int n;
		CSize size;
		n = wsprintf(text, TEXT("%s"), TEXT("$"));
		dc.GetTextExtent(text, n, &size);
		dc.ExtTextOut(left.x + (cx1 - size.cx)/2, rct.top - 2 - size.cy - 2, ETO_CLIPPED, &rect, text, n, NULL);
		n = wsprintf(text, TEXT("%d"), w1);
		dc.GetTextExtent(text, n, &size);
		dc.ExtTextOut(left.x + (cx1 - size.cx)/2, rct.bottom + 2, ETO_CLIPPED, &rect, text, n, NULL);
		n = wsprintf(text, TEXT("%s"), TEXT("V"));
		dc.GetTextExtent(text, n, &size);
		dc.ExtTextOut(left.x + cx1 + (cx2 - size.cx)/2, rct.top - 2 - size.cy - 2, ETO_CLIPPED, &rect, text, n, NULL);
		n = wsprintf(text, TEXT("%d"), w2);
		dc.GetTextExtent(text, n, &size);
		dc.ExtTextOut(left.x + cx1 + (cx2 - size.cx)/2, rct.bottom + 2, ETO_CLIPPED, &rect, text, n, NULL);
		n = wsprintf(text, TEXT("%d"), w3);
		dc.GetTextExtent(text, n, &size);
		dc.ExtTextOut(right.x - cx3 + (cx3 - size.cx)/2, rct.bottom + 2, ETO_CLIPPED, &rect, text, n, NULL);


		wnd.ReleaseDC(dc);

		result = S_OK;
	}
	else
		result = E_INVALIDARG;
	return result;
}

int CPropertyPageGlass::DrawMiniPicture(CWindow wnd, int style)
{
	int result;
	if (wnd)
	{
		CPen pen;
		CBrush brush;
		COLORREF color;
		COLORREF penColor;
		CRect rect, rect1, rect2;

		COLORREF colorBuy = DEFAULT_COLOR_BUY;
		COLORREF colorSell = DEFAULT_COLOR_SELL;
		COLORREF colorFill = DEFAULT_COLOR_FILL;
		COLORREF colorGrid = DEFAULT_COLOR_GRID;
		COLORREF colorText = DEFAULT_COLOR_TEXT;

		wnd.GetClientRect(&rect);

		wnd.InvalidateRect(&rect);
		wnd.UpdateWindow();

		CDC dc = wnd.GetDC();

		dc.SetBkMode(TRANSPARENT);

		penColor = ::GetSysColor(COLOR_3DDKSHADOW);

		color = m_settings.items[GLASS_ITEM_COMMON].backgrnd.color;

		// Знак $ и стрелочки:
		LPCTSTR text = TEXT("$");
		CFont font;
		CFont * pFont = &font;
		BYTE bold = FALSE;
		BYTE italic = FALSE;
		int pts = 8;
		int lfHeight = -MulDiv(pts, dc.GetDeviceCaps(LOGPIXELSY), 72);
		pFont->CreateFont(lfHeight, 0, 0, 0, bold ? FW_BOLD : FW_NORMAL, italic, FALSE, 0, DEFAULT_CHARSET, 
			OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH|FF_SWISS,
			TEXT("Tahoma"));		
	
		dc.SelectFont(*pFont);
		dc.SetTextColor(colorText);		

		CSize size;
		dc.GetTextExtent(text, lstrlen(text), &size);

		int x1, y1, x2, y2;
		int xMiddle, yMiddle;
		int xSign1, xSign2, ySign1, ySign2;
		int xIcon1, xIcon2, yIcon1, yIcon2;
		x1 = y1 = x2 = y2 = xSign1 = xSign2 = xIcon1 = xIcon2 = 0;
		rect.left += 1; rect.top += 1;
		rect1 = rect; rect2 = rect;
		if (style == GLASS_STYLE_1 || style == GLASS_STYLE_2)
		{
			yMiddle = rect.top + rect.Height() / 2;
			rect1.bottom = yMiddle; rect2.top = rect1.bottom + 0;
			x1 = rect1.left; x2 = rect2.right - 1;
			y1 = y2 = yMiddle - 1;						
			if (style == GLASS_STYLE_2)
			{
				xSign1 = xSign2 = rect1.left + (rect1.Width() - size.cx) / 2;
				xIcon2 = rect1.left + 1; xIcon1 = rect2.right - 10 - 1;
			}
			else
			{
				xSign1 = xSign2 = rect1.left + (rect1.Width() - size.cx - 12) / 2;
				xIcon1 = xSign1 + size.cx + 4; xIcon2 = xIcon1;
			}
			
		}
		else if (style == GLASS_STYLE_3)
		{
			xMiddle = rect.left + rect.Width() / 2;
			rect1.right = xMiddle; rect2.left = rect1.right + 0;
			x1 = x2 = xMiddle - 1;
			y1 = rect1.top; y2 = rect2.bottom - 1;			
			xSign1 = rect1.right - size.cx - 2; xSign2 = rect2.left + 1;
			xIcon1 = rect1.left; xIcon2 = rect2.left + size.cx;
		}
		ySign1 = rect1.top; yIcon2 = ySign2 = rect2.top;
		yIcon1 = ySign1 + 3; yIcon2 = ySign2 + 3;

		CPen penNull;
		penNull.CreatePen(PS_NULL, 1, RGB(0, 0, 0));
		dc.SelectPen(penNull);

		CBrush brush1;
		brush1.CreateSolidBrush(colorSell);
		dc.SelectBrush(brush1);
		dc.Rectangle(&rect1);

		CBrush brush2;
		brush2.CreateSolidBrush(colorBuy);
		dc.SelectBrush(brush2);
		dc.Rectangle(&rect2);

		if (style == GLASS_STYLE_2)
		{
			CRect rect3, rect4;
			int cxThird = rect.Width() / 3;
			rect3 = rect1; rect4 = rect2;
			rect3.right = rect3.left + cxThird; rect4.left = rect4.right - cxThird;

			CBrush brush3;
			brush3.CreateSolidBrush(colorFill);
			dc.SelectBrush(brush3);		
			dc.Rectangle(&rect3); dc.Rectangle(&rect4);
		}

		// Граница:
		CPen penGrid;
		penGrid.CreatePen(PS_SOLID, 1, colorGrid);
		dc.SelectPen(penGrid);
		dc.MoveTo(x1, y1);
		dc.LineTo(x2, y2);

		int len = lstrlen(text);
		dc.ExtTextOut(xSign1, ySign1, ETO_CLIPPED, &rect, text, len, NULL);
		dc.ExtTextOut(xSign2, ySign2, ETO_CLIPPED, &rect, text, len, NULL);

		int cxIcon = 16; int cyIcon = 16;
		::DrawIconEx(dc.m_hDC, xIcon1, yIcon1, m_icons.hIcon[0], cxIcon, cyIcon, 0, NULL, DI_NORMAL);
		::DrawIconEx(dc.m_hDC, xIcon2, yIcon2, m_icons.hIcon[1], cxIcon, cyIcon, 0, NULL, DI_NORMAL);

		wnd.ReleaseDC(dc);

		result = S_OK;
	}
	else
		result = E_INVALIDARG;
	return result;
}

// CPropertyPageGlass message handlers

BOOL CPropertyPageGlass::OnInitDialog(CWindow wndFocus, LPARAM lInitParam)
{
	DoDataExchange(FALSE);

	m_painter.Create(*this);

	HINSTANCE hInst = theApp.GetModuleInstance();
	int icons[] = {
		IDI_BUY_SMALL,
		IDI_SELL_SMALL,
	};
	for (int i = 0; i < SIZEOF_ARRAY(icons); i++)
		m_icons.hIcon[i] = CMyQuikApp::LoadIconEx(hInst, icons[i], 16);

	// Перемещаем группы элементов:
	MoveGroup(&CPropertyPageGlass::GetIdGroupView, IDC_GLASS_FRAME_VIEW, IDC_GLASS_FRAME_TEXT, SW_HIDE);
	MoveGroup(&CPropertyPageGlass::GetIdGroupUser, IDC_GLASS_FRAME_USER, IDC_GLASS_FRAME_TEXT, SW_HIDE);

	::SetWindowText(GetDlgItem(IDC_GLASS_FRAME_TEXT), TEXT(""));
	::SetWindowText(GetDlgItem(IDC_GLASS_FRAME_BACKGRND), TEXT(""));

	// Перебираем установленные в системе шрифты:
	EnumerateSystemFonts();	

	SetModified(FALSE);
#if 1
	SetDlgCursorHand(this, IDC_GLASS_BACKGRND_COLOR);
	SetDlgCursorHand(this, IDC_GLASS_COLOR);
	SetDlgCursorHand(this, IDC_FONT_COLOR);
#endif

	CListBox listBox = GetDlgItem(IDC_LIST1);
	LPCTSTR name;
	for (int i = 0; (name = GetItemName(i)) != NULL; i++)
		listBox.AddString(name);
	listBox.SetCurSel(0);

	m_columnSizer.Init(&m_settings.columns);

	OnLbnSelchangeList1(0, 0, NULL);

	UpdateSettings();

	EnableModify();

	m_initialized = TRUE;

	Invalidate2();

	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX Property Pages should return FALSE
}

BOOL CPropertyPageGlass::OnApply()
{
	UpdateSettings(GET);

	SetModified(FALSE);

	CWindow wnd = GetParent();
	if (wnd)
		wnd.SendMessage(UM_APPLY, this->IDD, (LPARAM)&m_settings);

	return CPropertyPageGlassBase::OnApply();
}

BOOL CPropertyPageGlass::OnQueryCancel()
{
	CWindow wnd = GetParent();
	if (wnd)
		wnd.SendMessage(UM_CANCEL, this->IDD, (LPARAM)&m_settings);

	return CPropertyPageGlassBase::OnQueryCancel();
}

void CPropertyPageGlass::OnPaint(CDCHandle )
{
	CPaintDC dc(*this);

	CWindow wnd;

	int flags = m_painter.GetPaint();

	if (flags & F_DRAW_WINDOW_STYLE)
	{
		wnd = GetDlgItem(IDC_STATIC_PICT1);
		DrawMiniPicture(wnd, GLASS_STYLE_1);
		wnd = GetDlgItem(IDC_STATIC_PICT2);
		DrawMiniPicture(wnd, GLASS_STYLE_2);
		wnd = GetDlgItem(IDC_STATIC_PICT3);
		DrawMiniPicture(wnd, GLASS_STYLE_3);
	}

	if (flags & F_DRAW_TEXT_COLOR)
	{
		// Цвет шрифта:
		Settings::Presentation::GlassItem::Text * pText = GetCurrentText();
		if (pText)
		{
			wnd = GetDlgItem(IDC_FONT_COLOR);
			my::Painter::DrawColorPicker(wnd, pText->cf.crTextColor);
		}
	}

	if (flags & F_DRAW_GLASS_BACKGRND_COLOR)
	{		
		COLORREF * pColor = GetCurrentColor();
		if (pColor)
		{
			wnd = GetDlgItem(IDC_GLASS_BACKGRND_COLOR);
			my::Painter::DrawColorPicker(wnd, *pColor);
		}
	}

	if (flags & F_DRAW_GLASS_COLOR)
	{		
		COLORREF * pColor = GetCurrentColor(0);
		if (pColor)
		{
			wnd = GetDlgItem(IDC_GLASS_COLOR);
			my::Painter::DrawColorPicker(wnd, *pColor);
		}
	}

	if (flags & F_DRAW_COLUMN_SIZER)
	{
		wnd = GetDlgItem(IDC_COLUMN_SIZER);
		DrawColumnSizer(wnd);
	}
}

void CPropertyPageGlass::OnColor(DWORD & color)
{
	CColorDialog dlg(color, CC_ANYCOLOR|CC_FULLOPEN, *this);
	if (IDOK == dlg.DoModal())
	{
		if (color != dlg.GetColor())
		{
			color = dlg.GetColor();

			SetModified();

			CWindow wnd = GetFocus();
			this->Invalidate2(wnd);
		}
	}
}

void CPropertyPageGlass::OnStnClickedFontColor(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	Settings::Presentation::GlassItem::Text * pText = GetCurrentText();
	if (pText)
		OnColor(pText->cf.crTextColor);
}

void CPropertyPageGlass::OnCbnSelchangeComboFontName(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	Settings::Presentation::GlassItem::Text * pText = GetCurrentText();
	if (pText)
	{
		CComboBox combo (GetDlgItem(IDC_COMBO_FONT_NAME));
		ASSERT (combo);

		TSTRING_SMALL2(name, size);
		combo.GetWindowText(name, size);
		SAFE_TSTRCPY(pText->cf.szFaceName, name);

		SetModified();
	}
}

void CPropertyPageGlass::OnCbnSelchangeComboFontStyle(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	Settings::Presentation::GlassItem::Text * pText = GetCurrentText();
	if (pText)
	{
		CComboBox combo (GetDlgItem(IDC_COMBO_FONT_STYLE));
		UpdateComboBoxOutline(combo, pText->cf.dwEffects, GET);
		SetModified();
	}
}

void CPropertyPageGlass::OnCbnSelchangeComboFontSize(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	Settings::Presentation::GlassItem::Text * pText = GetCurrentText();
	if (pText)
	{
		CComboBox combo (GetDlgItem(IDC_COMBO_FONT_SIZE));
		ASSERT (combo);

		TSTRING_SMALL2(name, size);
		int iSel = combo.GetCurSel();
		if (iSel >= 0)
		{
			combo.GetLBText(iSel, name);
			//combo.GetWindowText(name);
		
			int pts = ::StrToInt(name);
			pText->cf.yHeight = pts * 20;

			SetModified();
		}
	}
}

void CPropertyPageGlass::OnCbnEditchangeComboFontSize(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	Settings::Presentation::GlassItem::Text * pText = GetCurrentText();
	if (pText)
	{
		CComboBox combo (GetDlgItem(IDC_COMBO_FONT_SIZE));
		ASSERT (combo);

		TSTRING_SMALL2(name, size);
		combo.GetWindowText(name, size);
		
		int pts = ::StrToInt(name);
		if (pts > 0)
		{
			pText->cf.yHeight = pts * 20;
			SetModified();
		}
	}
}

void CPropertyPageGlass::OnBnClickedCheckShowUserBids(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsUser(GET);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedCheckShowUserStopbids(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsUser(GET);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedCheckShowUserDeals(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsUser(GET);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedCheckShowUserCol(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsUser(GET);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedCheckShowUserIcons(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsUser(GET);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedCheckUserGroupByPrice(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsUser(GET);
	SetModified();
}

void CPropertyPageGlass::OnStnClickedGlassBackgrndColor(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	COLORREF * pColor = GetCurrentColor();
	if (pColor)
		OnColor(*pColor);
}

void CPropertyPageGlass::OnEnChangeEditGlassBackgrnd(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	if (m_canModify)
	{
		UpdateItemSettings(GET);
		SetModified();
	}
}

LRESULT CPropertyPageGlass::OnDeltaposSpinGlassBackgrnd(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);
	
	UINT id = IDC_EDIT_GLASS_BACKGRND;
	CWindow wnd = GetDlgItem(id);
	ASSERT (wnd);

	TSTRING_SMALL2(str, size);
	wnd.GetWindowText(str, size);
	int val = StrToInt(str);
	if (pNMUpDown->iDelta < 0)
		val++;
	else
	{
		if (val > 0)
			val--;
	}
	_stprintf_s(str, size, TEXT("%d"), val);
	wnd.SetWindowText(str);

	return 0;
}

void CPropertyPageGlass::OnStnClickedGlassColor(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	COLORREF * pColor = GetCurrentColor(0);
	if (pColor)
		OnColor(*pColor);
}

void CPropertyPageGlass::OnEnChangeEditGlass(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	if (m_canModify)
	{
		UpdateItemSettings(GET);
		SetModified();
	}
}

LRESULT CPropertyPageGlass::OnDeltaposSpinGlass(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);
	
	UINT id = IDC_EDIT_GLASS;
	CWindow wnd = GetDlgItem(id);
	ASSERT (wnd);

	TSTRING_SMALL2(str, size);
	wnd.GetWindowText(str, size);
	int val = StrToInt(str);
	if (pNMUpDown->iDelta < 0)
		val++;
	else
	{
		int minimum = 0;
		int i = GetCurrentIndex();
#if 0
		if (i == GLASS_ITEM_GRID)
			minimum = -1;
#endif
		if (val > minimum)
			val--;
	}
	_stprintf_s(str, size, TEXT("%d"), val);
	wnd.SetWindowText(str);

	return 0;
}

void CPropertyPageGlass::OnLbnSelchangeList1(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	const int show = SW_SHOWNA;
	const int hide = SW_HIDE;

	int showFrame, showEdit, showValue1, showValue2, showOutline, showGrids;
	int showColumnSizer, showColumns, showAlignment, showAlignment2;
	int showGroupView, showGroupCommon, showGroupText, showGroupBackgrnd;
	int showGroupUser;
	int showGroupGrid, showGroupBorder, showGroupMarker, showGroupGlass;
	int showGroupMargins, showGroupIndication;
	int showApplyText, showApplyBackgrnd;
	int showFillArea;
	int showGroupDigits;

	showFrame = showEdit = showValue1 = showValue2 = showOutline = showGrids = hide;
	showColumnSizer = showColumns = showAlignment = showAlignment2 = hide;
	showGroupView = showGroupCommon = showGroupText = showGroupBackgrnd = hide;
	showGroupUser = hide;
	showGroupGrid = showGroupBorder = showGroupMarker = hide;
	showGroupGlass = hide;
	showGroupMargins = showGroupIndication = hide;
	showApplyText = showApplyBackgrnd = hide;
	showFillArea = hide;
	showGroupDigits = hide;

	LPCTSTR str1, str2, str3;
	LPCTSTR strFrame, strOpacity, strWidth;
	str1 = str2 = str3 = NULL;
	strFrame = strOpacity = NULL;
	strWidth = TEXT("Толщина");

	int flags = 0x0;

	int i = GetCurrentIndex();
	Settings::GlassItem & item = m_settings.items[i];
	BOOL enableText, enableBackgrnd, enableEdit;

	enableText = enableBackgrnd = enableEdit = FALSE;

	CRect rect, rect0, rect1, rect2, rectUse;
	int spinOffsetX = 0;
	CWindow wnd, wnd1, wnd2, wnd3;
	CWindow wndDraw = GetDlgItem(IDC_GLASS_COLOR);
	
	if (i == GLASS_ITEM_VIEW)
	{
		showGroupView = show;
		flags = F_DRAW_WINDOW_STYLE;
	}
	else
	{
		if ((i >= GLASS_ITEM_COMMON && i < GLASS_ITEM_COLUMNS) && (i != GLASS_ITEM_USER))
		{
			showGroupText = show;
			showGroupBackgrnd = show;
			flags = F_DRAW_TEXT_COLOR|F_DRAW_GLASS_BACKGRND_COLOR;

			if (i == GLASS_ITEM_COMMON)
				showGroupCommon = showGroupDigits = show;
				
			if ((i >= GLASS_ITEM_USER_BIDS_BUY && i <= GLASS_ITEM_USER_DEALS_SELL) || 
				(i >= GLASS_ITEM_PRICE_MIN && i <= GLASS_ITEM_PRICE_OFFER))
			{
				showApplyText = show;
				showApplyBackgrnd = show;
				showEdit = show;
			}
			if (i == GLASS_ITEM_BUY || i == GLASS_ITEM_SELL || i == GLASS_ITEM_NEUTRAL_ZONE)
			{
				if (i != GLASS_ITEM_NEUTRAL_ZONE)
					showAlignment = show;
				else
					showEdit = show;
				showFillArea = show;
			}
			if (i == GLASS_ITEM_BUY_VOLUME || i == GLASS_ITEM_SELL_VOLUME)
			{
				showGroupIndication = show;
				flags |= F_DRAW_GLASS_COLOR;
			}
		}
		else
		{
			if (i == GLASS_ITEM_USER)
				showGroupUser = show;
			else if (i == GLASS_ITEM_GRID)
				showGroupGrid = showGroupMargins = show;
			else if (i == GLASS_ITEM_BORDER)
			{
				showGroupBorder = show;
				flags |= F_DRAW_GLASS_COLOR;
			}
			else if (i == GLASS_ITEM_MARKER)
			{
				showGroupMarker = show;
				flags |= F_DRAW_GLASS_COLOR;
			}
			else if (i == GLASS_ITEM_COLUMNS)
				showColumns = show;
		}		
	}

	TSTRING_SMALL(strCaption);
	CListBox listBox = GetDlgItem(IDC_LIST1);
	listBox.GetText(listBox.GetCurSel(), strCaption);

	ShowGroup(&CPropertyPageGlass::GetIdGroupView, showGroupView);
	ShowGroup(&CPropertyPageGlass::GetIdGroupText, showGroupText);
	ShowGroup(&CPropertyPageGlass::GetIdGroupBackgrnd, showGroupBackgrnd);
	ShowGroup(&CPropertyPageGlass::GetIdGroupUser, showGroupUser);

	CRect rectView;
	wnd1 = GetDlgItem(IDC_GLASS_FRAME_TEXT);
	wnd2 = GetDlgItem(IDC_GLASS_FRAME_BACKGRND);
	wnd1.GetWindowRect(&rect1); ScreenToClient(&rect1);
	wnd2.GetWindowRect(&rect2); ScreenToClient(&rect2);

	wnd = GetDlgItem(IDC_GLASS_FRAME_LIST);
	wnd.GetWindowRect(&rect); ScreenToClient(&rect);

	rectView.top = rect.top; rectView.bottom = rect.bottom;
	rectView.left = rect1.left; rectView.right = rect1.right;

	CRect rectList, rectFrame, rectCombo, rectCheck, rectColor, rectEdit, rectSpin;
	wnd = GetDlgItem(IDC_GLASS_FRAME_0);
	wnd.GetWindowRect(&rectFrame); ScreenToClient(&rectFrame);
	rectFrame.top = rectView.top;
	wnd = GetDlgItem(IDC_COMBO_FONT_STYLE);
	wnd.GetWindowRect(&rectCombo); ScreenToClient(&rectCombo);
	wnd = GetDlgItem(IDC_CHECKBOX_TEXT_1);
	wnd.GetWindowRect(&rectCheck); ScreenToClient(&rectCheck);
	wnd = GetDlgItem(IDC_STATIC_FONT_NAME);
	wnd.GetWindowRect(&rect0); ScreenToClient(&rect0);
	wnd = GetDlgItem(IDC_LIST1);
	wnd.GetWindowRect(&rectList); ScreenToClient(&rectList);
	rect0.top = rectList.top;

	int top, left, bottom;
	int dy = rectCheck.Height();
	int dy2;

	CWindow checkbox, check0, check1, check2;
	checkbox = GetDlgItem(IDC_CHECKBOX_TEXT);
	CRect rectCheckBox;
	checkbox.GetWindowRect(&rectCheckBox); ScreenToClient(&rectCheckBox);

	check0 = GetDlgItem(IDC_CHECKBOX_GLASS);	

	CComboBox combo1 = GetDlgItem(IDC_COMBO_GLASS1);
	CComboBox combo2 = GetDlgItem(IDC_COMBO_GLASS2);

	if (showGroupView)
	{
		CRect rectView;
		wnd = GetDlgItem(IDC_CHECK_NEUTRAL_ZONE);
		wnd.GetWindowRect(&rect); ScreenToClient(&rect);
		wnd = GetDlgItem(IDC_GLASS_FRAME_VIEW);
		wnd.GetWindowRect(&rectView); ScreenToClient(&rectView);
		rectView.bottom = rect.bottom + dy;
		wnd.MoveWindow(rectView, TRUE);
	}

	if (showGroupText)
	{
		rect1.bottom = rectCombo.bottom + dy;

		wnd = GetDlgItem(IDC_CHECK_GROUP_DIGITS);
		if (showGroupDigits)
		{
			my::wnd::SetPos(wnd, rectCheck.left, rect1.bottom);
			rect1.bottom += dy + dy;			
		}		
		wnd = GetDlgItem(IDC_CHECKBOX_TEXT_1);
		if (showApplyText)
		{
			my::wnd::SetPos(wnd, rectCheck.left, rect1.bottom);
			rect1.bottom += dy + dy;			
		}
		my::wnd::Show(wnd, showApplyText);
		wnd1.MoveWindow(rect1, TRUE);		
	} // if (showGroupText)
	wnd = GetDlgItem(IDC_CHECK_GROUP_DIGITS);
	my::wnd::Show(wnd, showGroupDigits);

	if (showGroupBackgrnd)
	{
		rect2.top = rect1.bottom + dy;
		MoveGroup(&CPropertyPageGlass::GetIdGroupBackgrnd, IDC_GLASS_FRAME_BACKGRND, rect2, showGroupBackgrnd);
		
		wnd = GetDlgItem(IDC_STATIC_GLASS_BACKGRND_COLOR);
		my::wnd::Show(wnd, showGroupBackgrnd);
		wnd.Invalidate();

		wnd = GetDlgItem(IDC_GLASS_BACKGRND_COLOR);
		wnd.GetWindowRect(&rectColor); ScreenToClient(&rectColor);		
		rect2.bottom = rectColor.bottom + dy;	
		wnd = GetDlgItem(IDC_EDIT_GLASS_BACKGRND);
		wnd.GetWindowRect(&rectEdit); ScreenToClient(&rectEdit);
		wnd3 = GetDlgItem(IDC_SPIN_GLASS_BACKGRND);
		if (showEdit)
		{
			my::wnd::Enable(wnd, enableEdit);
			my::wnd::Enable(wnd3, enableEdit);
			my::wnd::SetPos(wnd, rectEdit.left, rectColor.top);
		}
		my::wnd::Show(wnd, showEdit);
		my::wnd::Show(wnd3, showEdit);
		wnd = GetDlgItem(IDC_STATIC_GLASS_OPACITY);
		my::wnd::Show(wnd, showEdit);
		wnd = GetDlgItem(IDC_CHECKBOX_BACKGRND_1);
		if (showApplyBackgrnd || showFillArea)
		{
			my::wnd::SetPos(wnd, rectCheck.left, rect2.bottom);
			rect2.bottom += dy + dy;
		}	
		wnd.SetWindowText((showFillArea) ? TEXT("Заполнять всю область") : TEXT("Применить ко всей строке"));
		my::wnd::Show(wnd, showApplyBackgrnd || showFillArea);

		wnd2.MoveWindow(rect2, TRUE);		
		strOpacity = TEXT("Непрозр:");
	} // if (showGroupBackgrnd)

	showGroupGlass = showAlignment | showGroupMargins | showGroupBorder | showGroupMarker;
	ShowGroup(&CPropertyPageGlass::GetIdGroupGlass, showGroupGlass);	

	check1 = GetDlgItem(IDC_CHECKBOX_ALIGNMENT);
	if (showAlignment)
	{
		rectFrame.top = rect2.bottom + dy;
		MoveGroup(&CPropertyPageGlass::GetIdGroupGlass, IDC_GLASS_FRAME, rectFrame, show);
		wnd = GetDlgItem(IDC_STATIC_GLASS_COLOR);
		my::wnd::Show(wnd, hide);
		wnd.GetWindowRect(&rect); ScreenToClient(&rect);
		left = rect.left;
		top = rect.top + 2;
		wnd = GetDlgItem(IDC_STATIC_GLASS_COLOR);
		my::wnd::Show(wnd, hide);
		wnd = GetDlgItem(IDC_STATIC_GLASS_WIDTH);
		my::wnd::Show(wnd, hide);
		wnd = GetDlgItem(IDC_GLASS_COLOR);
		my::wnd::Show(wnd, hide);
		wnd = GetDlgItem(IDC_EDIT_GLASS);
		my::wnd::Show(wnd, hide);
		wnd = GetDlgItem(IDC_SPIN_GLASS);
		my::wnd::Show(wnd, hide);

		InitComboBoxColumns(combo1.GetDlgCtrlID());
		InitComboBoxAlignment(combo2.GetDlgCtrlID());
		my::wnd::Show(combo1, show); my::wnd::Show(combo2, show);
		my::wnd::SetPos(combo1, left, top);
		my::wnd::SetPos(combo2, rectEdit.left, top);

		check0.GetWindowRect(&rect); ScreenToClient(&rect);
		my::wnd::SetPos(check1, rectCheckBox.left, rect.top);
	} // if (showAlignment)
	my::wnd::Show(check1, showAlignment); 

	check1 = GetDlgItem(IDC_CHECKBOX_GRID);	
	wnd1 = GetDlgItem(IDC_CHECKBOX_GRID_LINES_V);
	wnd2 = GetDlgItem(IDC_CHECKBOX_GRID_LINES_H);

	if (showGroupGrid)
	{
		MoveGroup(&CPropertyPageGlass::GetIdGroupBackgrnd, IDC_GLASS_FRAME_BACKGRND, rectFrame, show);
		//EnableGroup(&CPropertyPageGlass::GetIdGroupBackgrnd, showGroupGrid, IDC_CHECKBOX_BACKGRND);
		wnd = GetDlgItem(IDC_CHECKBOX_BACKGRND_1);
		my::wnd::Show(wnd, hide);

		wnd = GetDlgItem(IDC_CHECKBOX_BACKGRND);
		my::wnd::Show(wnd, hide);
		my::wnd::SetPos(check1, rectCheckBox.left, rectCheckBox.top);

		wnd = GetDlgItem(IDC_GLASS_BACKGRND_COLOR);
		wnd.GetWindowRect(&rectColor); ScreenToClient(&rectColor);
		wndDraw = wnd;
		flags = F_DRAW_GLASS_BACKGRND_COLOR;		

		str1 = TEXT("Вертикальные линии");
		str2 = TEXT("Горизонтальные линии");

		wnd1.GetWindowRect(&rect1);
		wnd2.GetWindowRect(&rect2);
		my::wnd::SetPos(wnd1, rect0.left, rectColor.bottom + dy);
		my::wnd::SetPos(wnd2, rect0.left, rectColor.bottom + dy + rect2.top - rect1.top);
		wnd2.GetWindowRect(&rect2); ScreenToClient(&rect2);
		rect2.bottom += dy;
		wnd = GetDlgItem(IDC_GLASS_FRAME_BACKGRND);
		rectFrame.bottom = rect2.bottom;
		wnd.MoveWindow(rectFrame, TRUE);
	} // if (showGroupGrid)	
	my::wnd::Show(check1, showGroupGrid);

	check2 = GetDlgItem(IDC_CHECKBOX_MARGINS);
	if (showGroupMargins)
	{
		// Отступы:
		rectFrame.top = rect2.bottom + dy;
		MoveGroup(&CPropertyPageGlass::GetIdGroupGlass, IDC_GLASS_FRAME, rectFrame, show);
		wnd = GetDlgItem(IDC_STATIC_GLASS_COLOR);
		my::wnd::Show(wnd, hide);
		wnd.GetWindowRect(&rect); ScreenToClient(&rect);
		left = rect.left;
		top = rect.top + 2;
		wnd = GetDlgItem(IDC_STATIC_GLASS_WIDTH);
		my::wnd::Show(wnd, hide);

		wnd = GetDlgItem(IDC_GLASS_COLOR);
		my::wnd::Show(wnd, hide);

		InitComboBoxMargins(combo1.GetDlgCtrlID());
		my::wnd::Show(combo1, show); my::wnd::Show(combo2, hide);
		my::wnd::SetPos(combo1, left, top);

		check0.GetWindowRect(&rect); ScreenToClient(&rect);
		my::wnd::SetPos(check2, rectCheckBox.left, rect.top);
	} // if (showGroupMargins)
	my::wnd::Show(check2, showGroupMargins);

	check1 = GetDlgItem(IDC_CHECKBOX_BORDER);
	if (showGroupBorder)
	{
		MoveGroup(&CPropertyPageGlass::GetIdGroupGlass, IDC_GLASS_FRAME, rectFrame, show);
		my::wnd::SetPos(check1, rectCheckBox.left, rectCheckBox.top);
		
		wnd = GetDlgItem(IDC_STATIC_GLASS_WIDTH);
		wnd.GetWindowRect(&rect); ScreenToClient(&rect);
		top = rect.bottom + 3;

		my::wnd::Show(combo1, hide); my::wnd::Show(combo2, hide);
	}
	my::wnd::Show(check1, showGroupBorder);
	
	check1 = GetDlgItem(IDC_CHECKBOX_MARKER);
	wnd3 = GetDlgItem(IDC_CHECKBOX3);
	if (showGroupMarker)
	{
		MoveGroup(&CPropertyPageGlass::GetIdGroupGlass, IDC_GLASS_FRAME, rectFrame, show);
		my::wnd::SetPos(check1, rectCheckBox.left, rectCheckBox.top);
		
		wnd = GetDlgItem(IDC_STATIC_GLASS_WIDTH);
		wnd.GetWindowRect(&rect); ScreenToClient(&rect);
		top = rect.bottom + 3;

		my::wnd::Show(combo1, hide); my::wnd::Show(combo2, hide);

		wnd = GetDlgItem(IDC_GLASS_COLOR);
		wnd.GetWindowRect(&rectColor); ScreenToClient(&rectColor);

		str1 = TEXT("Следовать за ценой");
		str2 = TEXT("Следовать за курсором");
		str3 = TEXT("Следовать за контекстным меню");

		wnd1.GetWindowRect(&rect1);
		wnd2.GetWindowRect(&rect2);
		dy2 = rect2.top - rect1.top;
		my::wnd::SetPos(wnd1, rect0.left, rectColor.bottom + dy);
		my::wnd::SetPos(wnd2, rect0.left, rectColor.bottom + dy + dy2);
		wnd2.GetWindowRect(&rect2); ScreenToClient(&rect2);
		my::wnd::SetPos(wnd3, rect0.left, rect2.top + dy2);
		wnd3.GetWindowRect(&rect2); ScreenToClient(&rect2);
		rect2.bottom += dy;
		showFrame = show;
		wnd = GetDlgItem(IDC_GLASS_FRAME);
		my::wnd::Show(wnd, hide);
	}
	my::wnd::Show(check1, showGroupMarker);

	wnd = GetDlgItem(IDC_COLUMN_SIZER);
	if (showColumns)
	{			
		my::wnd::SetPos(wnd, rect0.left, rect0.top);
		wnd.GetWindowRect(&rect); ScreenToClient(&rect);
		wndDraw = wnd; flags = F_DRAW_COLUMN_SIZER;
		rect2.bottom = rect.bottom + dy;
		showFrame = show;
		strFrame = strCaption;
	}
	my::wnd::Show(wnd, showColumns);

	if (showGroupGlass)
	{
		wnd = GetDlgItem(IDC_GLASS_COLOR);
		wnd.GetWindowRect(&rect); ScreenToClient(&rect);
		my::wnd::SetPos(wnd, rect.left, top);
		wnd = GetDlgItem(IDC_SPIN_GLASS);
		wnd.GetWindowRect(&rect); ScreenToClient(&rect);
		my::wnd::SetPos(wnd, rect.left, top);
		wnd = GetDlgItem(IDC_EDIT_GLASS);
		wnd.GetWindowRect(&rect); ScreenToClient(&rect);
		my::wnd::SetPos(wnd, rect.left, top);
		bottom = top + rect.Height() + dy;
		wnd = GetDlgItem(IDC_GLASS_FRAME);
		wnd.GetWindowRect(&rect); ScreenToClient(&rect);
		rect.bottom = bottom;
		wnd.MoveWindow(rect, TRUE);

		strOpacity = TEXT("Толщина:");
	}

	if (showGroupBorder)
	{
		wnd = GetDlgItem(IDC_GLASS_COLOR);
		wnd.GetWindowRect(&rectColor); ScreenToClient(&rectColor);

		str1 = TEXT("Границы нейтральной зоны");
		str2 = TEXT("Середина нейтральной зоны");

		wnd1.GetWindowRect(&rect1);
		wnd2.GetWindowRect(&rect2);
		my::wnd::SetPos(wnd1, rect0.left, rectColor.bottom + dy);
		my::wnd::SetPos(wnd2, rect0.left, rectColor.bottom + dy + rect2.top - rect1.top);
		wnd2.GetWindowRect(&rect2); ScreenToClient(&rect2);
		rect2.bottom += dy;

		wnd = GetDlgItem(IDC_GLASS_FRAME);
		rectFrame.bottom = rect2.bottom;
		wnd.MoveWindow(rectFrame, TRUE);
		wnd.ShowWindow(show);
	}

	check1 = GetDlgItem(IDC_CHECKBOX_INDICATION);
	if (showGroupIndication)
	{
		int rectHeight;
		rectFrame.top = rect2.bottom + dy;
		MoveGroup(&CPropertyPageGlass::GetIdGroupGlass, IDC_GLASS_FRAME, rectFrame, show);		

		wnd = GetDlgItem(IDC_STATIC_GLASS_COLOR);
		wnd.GetWindowRect(&rect); ScreenToClient(&rect);
		top = rect.bottom + 3;
		wnd = GetDlgItem(IDC_GLASS_COLOR);
		wnd.GetWindowRect(&rect); ScreenToClient(&rect);
		my::wnd::SetPos(wnd, rect.left, top);

		left = rect.left;
		rectHeight = rect.Height();

		InitComboBoxAlignment(combo2.GetDlgCtrlID());
		combo2.GetWindowRect(&rect); ScreenToClient(&rect);
		my::wnd::SetPos(combo2, rect.left, top);

		top += rectHeight;

		strWidth = TEXT("Выравн:");
		top += 3 + 3;
		wnd = GetDlgItem(IDC_STATIC_GLASS_MARGINS);
		my::wnd::SetPos(wnd, left, top);

		top += dy + 3;
		InitComboBoxMargins(combo1.GetDlgCtrlID());
		my::wnd::SetPos(combo1, left, top);

		wnd = GetDlgItem(IDC_EDIT_GLASS);
		wnd.GetWindowRect(&rectEdit); ScreenToClient(&rectEdit);
		my::wnd::SetPos(wnd, rectEdit.left, top);
		wnd = GetDlgItem(IDC_SPIN_GLASS);
		wnd.GetWindowRect(&rectSpin); ScreenToClient(&rectSpin);		
		my::wnd::SetPos(wnd, rectSpin.left, top + rectSpin.top - rectEdit.top);

		check0.GetWindowRect(&rect); ScreenToClient(&rect);
		my::wnd::SetPos(check1, rect.left, rect.top);

		combo1.GetWindowRect(&rect); ScreenToClient(&rect);
		wnd = GetDlgItem(IDC_GLASS_FRAME);
		rectFrame.bottom = rect.bottom + dy;
		wnd.MoveWindow(rectFrame, TRUE);
	} // if (showGroupIndication)
	my::wnd::Show(check1, showGroupIndication);
	wnd = GetDlgItem(IDC_STATIC_GLASS_MARGINS);
	my::wnd::Show(wnd, showGroupIndication);

	my::wnd::Show(check0, hide);

	if (str1) 
	{
		wnd1.SetWindowText(str1); my::wnd::Show(wnd1, show);
	}
	else
		my::wnd::Show(wnd1, hide);
	if (str2) 
	{
		wnd2.SetWindowText(str2); my::wnd::Show(wnd2, show);
	}
	else
		my::wnd::Show(wnd2, hide);
	if (str3) 
	{
		wnd3.SetWindowText(str3); my::wnd::Show(wnd3, show);
	}
	else
		my::wnd::Show(wnd3, hide);


	if (showFrame)
	{
		wnd = GetDlgItem(IDC_GLASS_FRAME_0);		
		rectFrame.bottom = rect2.bottom;
		wnd.MoveWindow(rectFrame, TRUE);		
	}

	wnd = GetDlgItem(IDC_GLASS_FRAME_0);
	if (strFrame)
		wnd.SetWindowText(strFrame);
	my::wnd::Show(wnd, showFrame);

	if (strOpacity)
	{
		wnd = GetDlgItem(IDC_STATIC_GLASS_OPACITY);
		wnd.SetWindowText(strOpacity);
	}

	if (strWidth)
	{
		wnd = GetDlgItem(IDC_STATIC_GLASS_WIDTH);
		wnd.SetWindowText(strWidth);
	}
	
	UpdateItemSettings(SET);
	Invalidate2(wndDraw, flags);
}

void CPropertyPageGlass::OnBnClickedCheckBuyAbove(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsView(GET);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedCheckFlipHoriz(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsView(GET);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedCheckRarefy(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsView(GET);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedCheckNeutralZone(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsView(GET);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedCheckGroupDigits(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsGroupDigits(GET, m_settings.items[GLASS_ITEM_COMMON].text);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedRadioStyle1(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	int style = GLASS_STYLE_1;
	UpdateSettingsStyle(GET, style);
	UpdateSettingsStyle(GET, GLASS_STYLE_1);
	OnLbnSelchangeList1(0, 0, NULL);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedRadioStyle2(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	int style = GLASS_STYLE_2;
	UpdateSettingsStyle(GET, style);	
	OnLbnSelchangeList1(0, 0, NULL);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedRadioStyle3(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	int style = GLASS_STYLE_3;
	UpdateSettingsStyle(GET, style);
	OnLbnSelchangeList1(0, 0, NULL);
	SetModified();
}

void CPropertyPageGlass::OnMouseMove(UINT nFlags, CPoint point)
{
	CRect rect;
	CWindow wnd = GetDlgItem(IDC_COLUMN_SIZER);
	if (wnd.IsWindowVisible())
	{
		wnd.GetWindowRect(&rect);
		ScreenToClient(&rect);
		if (rect.PtInRect(point))
		{
			BOOL update = FALSE;
			CRect marker1, marker2;
			point.Offset(-rect.left, -rect.top);
			m_columnSizer.Get(NULL, NULL, NULL, NULL, NULL, &marker1, &marker2);
			if (marker1.PtInRect(point))
				m_columnSizer.SetActiveMarker(0);
			else if (marker2.PtInRect(point))
				m_columnSizer.SetActiveMarker(1);
			else if (m_columnSizer.GetActiveMarker() >= 0)
			{
				if (! m_columnSizer.IsSizing())
				{
					m_columnSizer.SetActiveMarker(-1);
					update = TRUE;
				}
			}
			if (m_columnSizer.GetActiveMarker() >= 0)
			{
				if (m_columnSizer.IsSizing())
				{
					m_columnSizer.SetMarker(&point);
					SetModified();
				}
				update = TRUE;
			}

			if (update)
				Invalidate2(wnd, F_DRAW_COLUMN_SIZER);
		}
	}
}

void CPropertyPageGlass::OnLButtonDown(UINT nFlags, CPoint point)
{
	if (m_columnSizer.GetActiveMarker() >= 0)
	{
		if (! m_columnSizer.IsSizing())
		{

			m_columnSizer.SetSizing();
		}
	}
}

void CPropertyPageGlass::OnLButtonUp(UINT nFlags, CPoint point)
{
	if (m_columnSizer.IsSizing())
	{

		m_columnSizer.SetSizing(FALSE);
	}
}

void CPropertyPageGlass::OnBnClickedCheckText(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	Settings::Presentation::GlassItem::Text * pText = GetCurrentText();
	if (pText)
	{
		CButton btn = GetDlgItem(IDC_CHECKBOX_TEXT);
		if (GLASS_ITEM_COMMON == GetCurrentIndex())
			btn.SetCheck(BST_CHECKED);
		else
		{
			pText->enable = (btn.GetCheck() == BST_CHECKED);
			UpdateSettingsText(SET, *pText);			
			SetModified();
		}
	}
}

void CPropertyPageGlass::OnBnClickedCheckBackgrnd(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	Settings::Presentation::GlassItem::Background * pBackgrnd = GetCurrentBackgrnd();
	if (pBackgrnd)
	{
		CButton btn = GetDlgItem(IDC_CHECKBOX_BACKGRND);
		if (GLASS_ITEM_COMMON == GetCurrentIndex())
			btn.SetCheck(BST_CHECKED);
		else
		{
			pBackgrnd->enable = (btn.GetCheck() == BST_CHECKED);
			UpdateSettingsBackgrnd(SET, *pBackgrnd);
			SetModified();
		}
	}
}

void CPropertyPageGlass::OnBnClickedCheckText1(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsText1(GET, -1);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedCheckBackgrnd1(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsBackgrnd1(GET, -1);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedCheckAlignment(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	int i = GetCurrentIndex();
	if (i == GLASS_ITEM_BUY || i == GLASS_ITEM_SELL)
	{
		Settings::Presentation::GlassItem & item = m_settings.items[i];
		CButton btn = GetDlgItem(nID);
		item.u.alignment.enable = (btn.GetCheck() == BST_CHECKED);

		UpdateSettingsAlignment(SET, item.u.alignment.value, item.u.alignment.enable);
		SetModified();
	}
}

void CPropertyPageGlass::OnBnClickedCheckboxIndication(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	int i = GetCurrentIndex();
	if (i == GLASS_ITEM_BUY_VOLUME || i == GLASS_ITEM_SELL_VOLUME)
	{
		Settings::Presentation::GlassItem & item = m_settings.items[i];
		CButton btn = GetDlgItem(nID);
		item.other.enable = (btn.GetCheck() == BST_CHECKED);

		UpdateSettingsVolumeIndication(SET, item);
		SetModified();
	}
}

void CPropertyPageGlass::OnBnClickedCheckGrid(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	CButton btn = GetDlgItem(nID);
	m_settings.grid.show = (btn.GetCheck() == BST_CHECKED);

	UpdateSettingsGridLines(SET);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedCheckMargins(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	CButton btn = GetDlgItem(nID);
	m_settings.margins.enable = (btn.GetCheck() == BST_CHECKED);

	UpdateSettingsMargins(SET);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedCheckBorder(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	CButton btn = GetDlgItem(nID);
	m_settings.border.show = (btn.GetCheck() == BST_CHECKED);

	UpdateSettingsBorder(SET);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedCheckMarker(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	CButton btn = GetDlgItem(nID);
	m_settings.marker.show = (btn.GetCheck() == BST_CHECKED);

	UpdateSettingsMarker(SET);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedCheckGridLinesV(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	int i = GetCurrentIndex();
	if (i == GLASS_ITEM_GRID)
		UpdateSettingsGridLines(GET);
	else if (i == GLASS_ITEM_BORDER)
		UpdateSettingsBorder(GET);
	else if (i == GLASS_ITEM_MARKER)
		UpdateSettingsMarker(GET);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedCheckGridLinesH(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	int i = GetCurrentIndex();
	if (i == GLASS_ITEM_GRID)
		UpdateSettingsGridLines(GET);
	else if (i == GLASS_ITEM_BORDER)
		UpdateSettingsBorder(GET);
	else if (i == GLASS_ITEM_MARKER)
		UpdateSettingsMarker(GET);
	SetModified();
}

void CPropertyPageGlass::OnBnClickedCheckbox3(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	int i = GetCurrentIndex();
	if (i == GLASS_ITEM_MARKER)
		UpdateSettingsMarker(GET);
	SetModified();
}

void CPropertyPageGlass::OnCbnSelchangeComboGlass1(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	int i = GetCurrentIndex();
	if (i == GLASS_ITEM_GRID)
		UpdateSettingsMargins(SET);
	else if (i == GLASS_ITEM_BUY || i == GLASS_ITEM_SELL)
		UpdateSettingsAlignment(SET, i);
	else if (i == GLASS_ITEM_BUY_VOLUME || i == GLASS_ITEM_SELL_VOLUME)
		UpdateSettingsVolumeIndication(SET, i);
}

void CPropertyPageGlass::OnCbnSelchangeComboGlass2(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	int i = GetCurrentIndex();
	if (i == GLASS_ITEM_BUY || i == GLASS_ITEM_SELL)
	{
		UpdateSettingsAlignment(GET, i);
		SetModified();
	}
	else if (i == GLASS_ITEM_BUY_VOLUME || i == GLASS_ITEM_SELL_VOLUME)
	{
		UpdateSettingsVolumeIndication(GET, i);
		SetModified();
	}
}

BOOL CPropertyPageGlass::OnSetActive()
{
	int flags = F_DRAW_ALL;
	if (GLASS_ITEM_COLUMNS == GetCurrentIndex())
		flags |= F_DRAW_COLUMN_SIZER;
	m_painter.SetPaint(flags);

	return CPropertyPageGlassBase::OnSetActive();
}
