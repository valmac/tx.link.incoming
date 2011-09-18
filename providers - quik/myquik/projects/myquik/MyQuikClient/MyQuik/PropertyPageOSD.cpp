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
 *  PropertyPageOSD.cpp : implementation file
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "PropertyPageOSD.h"

//
// CPropertyPageOSD dialog
//

CPropertyPageOSD::CPropertyPageOSD(const Settings::Presentation::OSD & settings)
	: m_settings(settings)
{
	m_initialized = 0;
}

CPropertyPageOSD::~CPropertyPageOSD()
{
}

void CPropertyPageOSD::InitList()
{
	struct InternalData {
		LPCTSTR name;
		TxtFormat * pFormat;
	};
	InternalData datas[] = {
		{ TEXT("Обычный текст"), &m_settings.foreground },
		{ TEXT("Цена последней сделки"), &m_settings.prices.current },
		{ TEXT("Максимальная цена"), &m_settings.prices.maximum },
		{ TEXT("Минимальная цена"), &m_settings.prices.minimum },
		{ TEXT("Процент изменения"), &m_settings.percent },
		{ TEXT("Индикатор цены"), &m_settings.indicators.price },
		{ TEXT("Индик. спроса/предложения"), &m_settings.indicators.demandSupply },
		{ TEXT("Денежный лимит"), &m_settings.account.limit },
		{ TEXT("Накопленный доход"), &m_settings.account.profit },
		{ TEXT("Средняя цена трэйда"), &m_settings.trade.basic },
		{ TEXT("Доход трэйда"), &m_settings.trade.profit },
		{ TEXT("Предыдущий трэйд"), &m_settings.trade.last },
		{ TEXT("Информация о записях"), &m_settings.transaction },
		{ TEXT("Выбор инструмента"), &m_settings.instrument },
		{ TEXT("Выбор инструмента (код)"), &m_settings.seccode },
		{ TEXT("Рамка"), &m_settings.frame },
	};
	for (int i = 0; i < SIZEOF_ARRAY(datas); i++)
	{
		InternalData * pData = &datas[i];
		int iItem = m_listBox.AddString(pData->name);
		if (iItem >= 0)
			m_listBox.SetItemData(iItem, (DWORD_PTR)pData->pFormat);
	} // for (i)
	m_listBox.SetCurSel(0);
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

void CPropertyPageOSD::EnumerateSystemFonts()
{
	CComboBox combo;
	combo = GetDlgItem(IDC_COMBO_FONT_NAME);
	combo.Clear();

	LOGFONT lf;
	lf.lfFaceName[0] = '\0';
	lf.lfCharSet = DEFAULT_CHARSET;

	::EnumFontFamiliesEx(GetWindowDC(), &lf, (FONTENUMPROC)EnumFontFamExProc, (LPARAM)&combo, 0);

	combo.SetCurSel(0);

	// Начертание шрифта:
	combo = GetDlgItem(IDC_COMBO_FONT_STYLE);
	combo.Clear();
	combo.AddString(TEXT("обычный"));
	combo.AddString(TEXT("курсив"));
	combo.AddString(TEXT("жирный"));
	combo.AddString(TEXT("жирный курсив"));

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

void CPropertyPageOSD::SetModified(BOOL bChanged)
{
	if (bChanged && m_initialized)
	{
		CWindow parentWnd = theApp.GetMainWnd();
		if (parentWnd)
			parentWnd.SendMessage(UM_APPLY_SETTINGS, APPLY_SETTINGS_PRESENTATION_OSD, (LPARAM)&m_settings);
		Invalidate2(GetFocus());
	}
	CPropertyPageOSDBase::SetModified(bChanged);
}

TxtFormat * CPropertyPageOSD::GetCurrentFormat()
{
	TxtFormat * pFormat = NULL;
#if 0
	pFormat = &m_settings.foreground;
#else
	int iItem = m_listBox.GetCurSel();
	if (iItem >= 0)
	{
		pFormat = (TxtFormat *)m_listBox.GetItemData(iItem);
	}
#endif
	return pFormat;
}

void CPropertyPageOSD::UpdateSettings(int set)
{
	UpdateFont(set);
	UpdateOffsetX(set); UpdateOffsetY(set);
	UpdateVertical(set);
}

void CPropertyPageOSD::UpdateFont(int set)
{
	CComboBox combo;
	int i;
	// Текст:
	TxtFormat * pFormat = GetCurrentFormat();
	// Название шрифта:
	combo = GetDlgItem(IDC_COMBO_FONT_NAME);
	if (set)
	{
		i = combo.FindStringExact(-1, pFormat->cf.szFaceName);
		if (i != CB_ERR)
			combo.SetCurSel(i);
	}
	else
		OnCbnSelchangeComboFontName(0, 0, NULL);

	// Начертание:
	combo = GetDlgItem(IDC_COMBO_FONT_STYLE);
	UpdateComboBoxOutline(combo, pFormat->cf.dwEffects, set);

	// Размер:
	combo = GetDlgItem(IDC_COMBO_FONT_SIZE);
	if (set)
	{
		TCHAR name[MAX_PATH];
		int pts = pFormat->cf.yHeight / 20;
		wsprintf(name, TEXT("%d"), pts);
		int i = combo.FindStringExact(-1, name);
		if (i != CB_ERR)
			combo.SetCurSel(i);
		else
			combo.SetWindowText(name);
	}
	else
		OnCbnSelchangeComboFontSize(0, 0, NULL);

	CButton btn = GetDlgItem(IDC_CHECK_GROUP_DIGITS);
	if (set)
		btn.SetCheck(pFormat->groupDigits ? BST_CHECKED : BST_UNCHECKED);
	else
		pFormat->groupDigits = (BST_CHECKED == btn.GetCheck()) ? TRUE : FALSE;
}

void CPropertyPageOSD::UpdateOffset(int id, int set)
{
	CWindow wnd = GetDlgItem(id);
	TSTRING_SMALL2(str, size);
	TxtFormat * pFormat = GetCurrentFormat();
	LONG & offset = (id == IDC_EDIT_OFFSET_X) ? pFormat->offset.cx : pFormat->offset.cy;
	if (set)
	{			
		_stprintf_s(str, size, TEXT("%d"), offset);
		wnd.SetWindowText(str);
	}
	else
	{
		wnd.GetWindowText(str, size);
		int val = StrToInt(str);
		offset = val;
	}
}

void CPropertyPageOSD::UpdateOffsetX(int set)
{
	return UpdateOffset(IDC_EDIT_OFFSET_X, set);
}

void CPropertyPageOSD::UpdateOffsetY(int set)
{
	return UpdateOffset(IDC_EDIT_OFFSET_Y, set);
}

void CPropertyPageOSD::UpdateVertical(int set)
{
	CButton check (GetDlgItem(IDC_CHECK_VERTICAL));
	if (check)
	{
		CString str;
		TxtFormat * pFormat = GetCurrentFormat();
		if (set)
			check.SetCheck(pFormat->placement.vertical ? BST_CHECKED : BST_UNCHECKED);
		else
			pFormat->placement.vertical = (BST_CHECKED == check.GetCheck()) ? 1 : 0;
	}
}

void CPropertyPageOSD::EnableSelectFont(BOOL enable, BOOL enableColor, BOOL enableGroupDigits)
{
	CWindow wnd;
	wnd = GetDlgItem(IDC_STATIC_FONT_NAME); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_COMBO_FONT_NAME); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_STATIC_FONT_STYLE); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_COMBO_FONT_STYLE); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_STATIC_FONT_SIZE); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_COMBO_FONT_SIZE); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_STATIC_FONT_COLOR); wnd.EnableWindow(enableColor);
	wnd = GetDlgItem(IDC_FONT_COLOR); wnd.EnableWindow(enableColor);
	wnd = GetDlgItem(IDC_CHECK_GROUP_DIGITS); wnd.EnableWindow(enableGroupDigits);
}

void CPropertyPageOSD::EnableOffset(BOOL enable)
{
	CWindow wnd;
	wnd = GetDlgItem(IDC_STATIC_X); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_EDIT_OFFSET_X); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_SPIN_OFFSET_X); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_STATIC_Y); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_EDIT_OFFSET_Y); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_SPIN_OFFSET_Y); wnd.EnableWindow(enable);
}

void CPropertyPageOSD::EnableVertical(BOOL enable)
{
	CWindow wnd = GetDlgItem(IDC_CHECK_VERTICAL);
	wnd.EnableWindow(enable);
	wnd.ShowWindow(enable ? SW_SHOW : SW_HIDE);
}

// CPropertyPageOSD message handlers
BOOL CPropertyPageOSD::OnInitDialog(CWindow wndFocus, LPARAM lInitParam)
{
	m_listBox.Attach(GetDlgItem(IDC_LIST1));
	::SetWindowTheme(m_listBox, TEXT("explorer"), NULL);

	m_painter.Create(*this);

	InitList();

	EnumerateSystemFonts();

	UpdateSettings(SET);

	OnLbnSelchangeList1(0, 0, NULL);

#if 1
	SetDlgCursorHand(this, IDC_FONT_COLOR);
	SetDlgCursorHand(this, IDC_EXAMPLE);
#endif

	m_initialized = 1;

	Invalidate2();

	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX Property Pages should return FALSE
}

void CPropertyPageOSD::OnPaint(CDCHandle )
{
	CPaintDC dc(*this);

	CWindow wnd;
	CDC * pDC;
	CRect rect;

	TxtFormat * pFormat = GetCurrentFormat();

	int flags = m_painter.GetPaint();
	if (flags & F_DRAW_TEXT_COLOR)
	{
		// Цвет шрифта:
		wnd = GetDlgItem(IDC_FONT_COLOR);
		my::Painter::DrawColorPicker(wnd, pFormat->cf.crTextColor);
	}

	if (flags & F_DRAW_EXAMPLE)
	{
		// Образец текста:
		wnd = GetDlgItem(IDC_EXAMPLE);

		wnd.GetClientRect(&rect);
		wnd.InvalidateRect(&rect);
		wnd.UpdateWindow();

		CDC _dc = wnd.GetDC();
		pDC = &_dc;
			
		// Закрашиваем область:
		CBrush brush;
		brush.CreateSolidBrush(m_settings.backgrnd.color);
		pDC->SelectBrush(brush);
		CPen pen;
		pen.CreatePen(PS_SOLID, 1, ::GetSysColor(COLOR_3DDKSHADOW));
		pDC->SelectPen(pen);
		pDC->Rectangle(&rect);

		CSize size;
		int x, y;

		// Текст:
		TSTRING_SMALL(str);
		TSTRING_SMALL(strValue);
		int n = _stprintf_s(strValue, SIZEOF_ARRAY(strValue), TEXT("%s"), TEXT("12345.67890"));
		if (pFormat->groupDigits)
			n = my::str::GroupDigits(strValue, n, strValue, SIZEOF_ARRAY(strValue));
		n = _stprintf_s(str, SIZEOF_ARRAY(str), TEXT("AaBb %s ЮюЯя"), strValue);

		TEXTMETRIC tm;
		CFont font;

		BOOL bold = (pFormat->cf.dwEffects & CFE_BOLD) ? TRUE : FALSE;
		BOOL italic = (pFormat->cf.dwEffects & CFE_ITALIC) ? TRUE : FALSE;
		int pts = pFormat->cf.yHeight / 20;
		int lfHeight = -MulDiv(pts, pDC->GetDeviceCaps(LOGPIXELSY), 72);
		font.CreateFont(lfHeight, 0, 0, 0, bold ? FW_BOLD : FW_NORMAL, italic, FALSE, 0, DEFAULT_CHARSET, 
			OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH|FF_SWISS,
			pFormat->cf.szFaceName);		
		pDC->SetBkMode(TRANSPARENT);
		pDC->SelectFont(font);
		pDC->GetTextMetrics(&tm);
		pDC->GetTextExtent(str, n, &size);
		x = (rect.Width() - size.cx) / 2;
		y = (rect.Height() - size.cy) / 2;
		COLORREF color;
		if (pFormat == &m_settings.frame)
		{
			CRect frame(x, y, x + size.cx, y + size.cy);
			CPen penFrame;
			penFrame.CreatePen(PS_SOLID, 1, pFormat->cf.crTextColor);
			pDC->SelectPen(penFrame);
			pDC->Rectangle(x - 2, y, x + 1 + size.cx, y + 1 + size.cy);
			color = m_settings.foreground.cf.crTextColor;
		}
		else
			color = pFormat->cf.crTextColor;

		pDC->SetTextColor(color);
		pDC->ExtTextOut(x, y, ETO_CLIPPED, &rect, str, n, NULL);

		wnd.ReleaseDC(*pDC);
	}
}

void CPropertyPageOSD::OnColor(DWORD & color)
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

BOOL CPropertyPageOSD::OnApply()
{
	UpdateSettings(GET);

	SetModified(FALSE);

	CWindow wnd = GetParent();
	if (wnd)
		wnd.SendMessage(UM_APPLY, this->IDD, (LPARAM)&m_settings);

	return CPropertyPageOSDBase::OnApply();
}

BOOL CPropertyPageOSD::OnQueryCancel()
{
	UpdateSettings(GET);

	CWindow wnd = GetParent();
	if (wnd)
		wnd.SendMessage(UM_CANCEL, this->IDD, (LPARAM)&m_settings);
	wnd = theApp.GetMainWnd();
	if (wnd)
		wnd.SendMessage(UM_OSD, 0, 0x0);

	return CPropertyPageOSDBase::OnQueryCancel();
}

void CPropertyPageOSD::OnCbnSelchangeComboFontName(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	if (! m_initialized)
		return;

	CComboBox combo (GetDlgItem(IDC_COMBO_FONT_NAME));
	TxtFormat * pFormat = GetCurrentFormat();
	TSTRING_STD2(name, size);
	combo.GetWindowText(name, size);
	SAFE_TSTRCPY(pFormat->cf.szFaceName, name);

	SetModified();
}

void CPropertyPageOSD::OnCbnSelchangeComboFontStyle(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	if (! m_initialized)
		return;

	CComboBox combo (GetDlgItem(IDC_COMBO_FONT_STYLE));
	TxtFormat * pFormat = GetCurrentFormat();
	UpdateComboBoxOutline(combo, pFormat->cf.dwEffects, GET);
	SetModified();
}

void CPropertyPageOSD::OnCbnSelchangeComboFontSize(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	if (! m_initialized)
		return;

	CComboBox combo (GetDlgItem(IDC_COMBO_FONT_SIZE));
	int iSel = combo.GetCurSel();
	if (iSel >= 0)
	{
		TSTRING_STD(name);
		combo.GetLBText(iSel, name);
		
		int pts = ::StrToInt(name);
		GetCurrentFormat()->cf.yHeight = pts * 20;

		SetModified();
	}
}

void CPropertyPageOSD::OnStnClickedFontColor(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnColor(GetCurrentFormat()->cf.crTextColor);
}

void CPropertyPageOSD::OnStnClickedExample(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnColor(m_settings.backgrnd.color);
}

void CPropertyPageOSD::OnCbnEditchangeComboFontSize(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	CComboBox combo (GetDlgItem(IDC_COMBO_FONT_SIZE));
	TSTRING_SMALL2(name, size);
	combo.GetWindowText(name, size);
		
	int pts = ::StrToInt(name);
	if (pts > 0)
	{
		GetCurrentFormat()->cf.yHeight = pts * 20;
		SetModified();
	}
}

void CPropertyPageOSD::OnBnClickedButtonBackgroundColor(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnColor(m_settings.backgrnd.color);
}

void CPropertyPageOSD::OnLbnSelchangeList1(UINT uNotifyCode, int nID, CWindow wndCtl)
{
#if 1
	int iItem = m_listBox.GetCurSel();
	TSTRING_SMALL(text);
	m_listBox.GetText(iItem, text);
	CWindow wnd = GetDlgItem(IDC_STATIC_FRAME_GLASS_ITEM);
	wnd.SetWindowText(text);
#endif

	UpdateFont(SET);
	UpdateOffsetX(SET); UpdateOffsetY(SET);
	UpdateVertical(SET);

	BOOL enable, enableColor, enableGroupDigits;
	enable = enableColor = enableGroupDigits = FALSE;
	TxtFormat * pFormat = GetCurrentFormat();
	if (pFormat != &m_settings.indicators.price && pFormat != &m_settings.indicators.demandSupply && pFormat != &m_settings.frame)
		enable = TRUE;
	if (pFormat != &m_settings.frame)
		enableColor = enable;
	else
		enableColor = TRUE;
	if (pFormat != &m_settings.transaction && pFormat != &m_settings.instrument && pFormat != &m_settings.seccode)
		enableGroupDigits = enable;
	EnableSelectFont(enable, enableColor, enableGroupDigits);
	
	if (pFormat != &m_settings.foreground && pFormat != &m_settings.frame)
		enable = TRUE;
	else
		enable = FALSE;
	EnableOffset(enable);

#if 0
	enable = (pFormat == &m_settings.indicators.price) ? TRUE : FALSE; //  || pFormat == &m_settings.indicators.demandSupply
#else
	enable = FALSE;
#endif
	EnableVertical(enable);

	Invalidate2(GetDlgItem(IDC_EXAMPLE));

	//if (enable)
	{
		CWindow wnd = theApp.GetMainWnd();
		if (wnd)
		{
			DWORD flags;
			int iSel = m_listBox.GetCurSel();
			if (iSel >= 0)
				flags = 1 << iSel;
			else
				flags = 0x0;
			wnd.SendMessage(UM_OSD, 0, flags);
		}
	}
}

void CPropertyPageOSD::OnEnChangeEditOffsetX(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateOffsetX(0);	
	CWindow wnd = GetDlgItem(IDC_EDIT_OFFSET_X);
	if (GetFocus() == wnd)
		SetModified();
}

void CPropertyPageOSD::OnEnChangeEditOffsetY(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateOffsetY(0);
	CWindow wnd = GetDlgItem(IDC_EDIT_OFFSET_Y);
	if (GetFocus() == wnd)
		SetModified();
}

LRESULT CPropertyPageOSD::OnDeltaposSpinOffsetX(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);

	CWindow wnd = GetDlgItem(IDC_EDIT_OFFSET_X);

	TSTRING_SMALL2(str, size);
	wnd.GetWindowText(str, size);
	int val = StrToInt(str);
	if (pNMUpDown->iDelta < 0)
		val++;
	else
		val--;
	_stprintf_s(str, size, TEXT("%d"), val);
	wnd.SetWindowText(str);

	OnEnChangeEditOffsetX(0, 0, NULL);
	SetModified();

	return 0;
}

LRESULT CPropertyPageOSD::OnDeltaposSpinOffsetY(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);

	CWindow wnd = GetDlgItem(IDC_EDIT_OFFSET_Y);

	TSTRING_SMALL2(str, size);
	wnd.GetWindowText(str, size);
	int val = StrToInt(str);
	if (pNMUpDown->iDelta < 0)
		val++;
	else
		val--;
	_stprintf_s(str, size, TEXT("%d"), val);
	wnd.SetWindowText(str);

	OnEnChangeEditOffsetY(0, 0, NULL);
	SetModified();

	return 0;
}

BOOL CPropertyPageOSD::OnKillActive()
{
	CWindow wnd = theApp.GetMainWnd();
	if (wnd)
		wnd.SendMessage(UM_OSD, 0, 0x0);	

	return CPropertyPageOSDBase::OnKillActive();
}

BOOL CPropertyPageOSD::OnSetActive()
{
	m_painter.SetPaint(F_DRAW_ALL);

	OnLbnSelchangeList1(0, 0, NULL);

	return CPropertyPageOSDBase::OnSetActive();
}


void CPropertyPageOSD::OnBnClickedCheckVertical(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	SetModified();
}

void CPropertyPageOSD::OnBnClickedCheckGroupDigits(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateFont(GET);
	Invalidate2(GetDlgItem(IDC_EXAMPLE));
	SetModified();
}


