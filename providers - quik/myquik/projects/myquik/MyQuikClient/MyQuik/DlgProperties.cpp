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
 *  DlgProperties.cpp : implementation file
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "DlgProperties.h"


// CDlgProperties dialog

CDlgProperties::CDlgProperties()
{
	m_initialized = FALSE;
	m_changing = FALSE;
}

CDlgProperties::~CDlgProperties()
{
}

void CDlgProperties::Init(LPCTSTR name, const QuoteTable::Properties * pProperties, const ListOfTableProperties * pListOfProperties)
{
	m_properties = *pProperties;
	if (name != NULL)
		SAFE_TSTRCPY(m_properties.name, name);
	m_pListOfProperties = pListOfProperties;
}

void CDlgProperties::SavePosition()
{
	theApp.SaveWindowPosition(*this, GetName());
}

void CDlgProperties::InsertStringInCombo(LPCTSTR str, CComboBox combo)
{
	if (lstrlen(str) > 0)
		if (CB_ERR == combo.FindStringExact(0, str))
			combo.InsertString(0, str);
}

void CDlgProperties::UpdateProperties(QuoteTable::Properties * pProperties, int set)
{
	ListOfTableProperties::const_iterator it;

	CWindow wnd = GetDlgItem(IDC_EDIT_NAME);
	if (set)
		wnd.SetWindowText(pProperties->name);
	else
		wnd.GetWindowText(pProperties->name, SIZEOF_ARRAY(pProperties->name));

	CComboBox combo;
	combo = GetDlgItem(IDC_COMBO_SECCODE);
	if (set)
	{
		for (it = m_pListOfProperties->begin(); it != m_pListOfProperties->end(); it++)
			InsertStringInCombo((*it).seccode, combo);
		combo.SetWindowText(pProperties->seccode);
	}
	else
		combo.GetWindowText(pProperties->seccode, SIZEOF_ARRAY(pProperties->seccode));

	combo = GetDlgItem(IDC_COMBO_CLASSCODE);
	if (set)
	{
		for (it = m_pListOfProperties->begin(); it != m_pListOfProperties->end(); it++)
			InsertStringInCombo((*it).classcode, combo);
		combo.SetWindowText(pProperties->classcode);
	}
	else
		combo.GetWindowText(pProperties->classcode, SIZEOF_ARRAY(pProperties->classcode));

	combo = GetDlgItem(IDC_COMBO_CLIENTCODE);
	if (set)
	{
		for (it = m_pListOfProperties->begin(); it != m_pListOfProperties->end(); it++)
			InsertStringInCombo((*it).clientcode, combo);
		combo.SetWindowText(pProperties->clientcode);
	}
	else
		combo.GetWindowText(pProperties->clientcode, SIZEOF_ARRAY(pProperties->clientcode));

	combo = GetDlgItem(IDC_COMBO_ACCOUNT);
	if (set)
	{
		for (it = m_pListOfProperties->begin(); it != m_pListOfProperties->end(); it++)
			InsertStringInCombo((*it).account, combo);
		combo.SetWindowText(pProperties->account);
	}
	else
		combo.GetWindowText(pProperties->account, SIZEOF_ARRAY(pProperties->account));

	wnd = GetDlgItem(IDC_EDIT_DPRICE);
	TSTRING_STD2(str, size);
	if (set)
	{
		FormatString(str, size, pProperties->price.step, pProperties->price.nd);
		wnd.SetWindowText(str);
	}
	else
	{
		int len = wnd.GetWindowText(str, size);
		pProperties->price.nd = CalculateLengthAfterPoint(str, len);
		TCHAR * pEnd;
		pProperties->price.step = _tcstod(str, &pEnd);
	}

	wnd = GetDlgItem(IDC_EDIT_QUANTITY);
	if (set)
	{			
		_stprintf_s(str, size, TEXT("%d"), static_cast<int>(pProperties->trading.quantity));
		wnd.SetWindowText(str);
	}
	else
	{
		wnd.GetWindowText(str, size);
		int val = StrToInt(str);
		if (val > 0)
			pProperties->trading.quantity = val;
	}

	wnd = GetDlgItem(IDC_EDIT_SPREAD);
	if (set)
	{
		FormatString(str, size, pProperties->trading.spread, pProperties->price.nd);
		wnd.SetWindowText(str);
	}
	else
	{
		wnd.GetWindowText(str, size);
		TCHAR * pEnd;
		pProperties->trading.spread = _tcstod(str, &pEnd);
	}

	UpdatePropertiesTradingAutos(pProperties, set);
}


void CDlgProperties::UpdatePropertiesTradingAutos(QuoteTable::Properties * pProperties, int set)
{
	TSTRING_STD2(str, size);
	TCHAR * pEnd;

	CWindow wnd;
	CButton btn;

	CComboBox comboBox = GetDlgItem(IDC_COMBO_TRADING_AUTOS);
	int i = comboBox.GetCurSel();
	QuoteTable::Properties::Trading::Autos::Item & autoTrading = 
		(i == TRADING_AUTO_STOP) ? pProperties->trading.autos.stop : pProperties->trading.autos.profit;

	m_changing = TRUE;

	wnd = GetDlgItem(IDC_EDIT_SLIPPAGE);
	if (set)
	{
		FormatString(str, size, autoTrading.slippage, pProperties->price.nd);
		wnd.SetWindowText(str);
	}
	else
	{
		wnd.GetWindowText(str, size);
		autoTrading.slippage = _tcstod(str, &pEnd);
	}

	wnd = GetDlgItem(IDC_EDIT_STOP_RELATIVE);
	if (set)
	{
		FormatString(str, size, autoTrading.relative, 2);
		wnd.SetWindowText(str);
	}
	else
	{
		wnd.GetWindowText(str, size);
		TCHAR * pEnd;
		autoTrading.relative = _tcstod(str, &pEnd);
	}

	wnd = GetDlgItem(IDC_EDIT_STOP_ABSOLUT);
	if (set)
	{
		FormatString(str, size, autoTrading.absolute, pProperties->price.nd);
		wnd.SetWindowText(str);
	}
	else
	{
		wnd.GetWindowText(str, size);		
		autoTrading.absolute = _tcstod(str, &pEnd);
	}

	if (set)
	{
		CButton radio;
		if (autoTrading.isRelative)
		{
			radio = GetDlgItem(IDC_RADIO_STOP_RELATIVE);
			radio.SetCheck(BST_CHECKED);
			UpdateStopRelative();
		}
		else
		{
			radio = GetDlgItem(IDC_RADIO_STOP_ABSOLUT);
			radio.SetCheck(BST_CHECKED);
			UpdateStopAbsolut();
		}
	}
	else
	{
		btn = GetDlgItem(IDC_RADIO_STOP_RELATIVE);
		autoTrading.isRelative = (btn.GetCheck() == BST_CHECKED) ? TRUE : FALSE;
	}

	m_changing = FALSE;
}

void CDlgProperties::UpdateStopRelative()
{
	CWindow wnd;
	
	wnd = GetDlgItem(IDC_EDIT_STOP_RELATIVE);
	wnd.EnableWindow(TRUE);
	wnd = GetDlgItem(IDC_SPIN_STOP_RELATIVE);
	wnd.EnableWindow(TRUE);

	wnd = GetDlgItem(IDC_EDIT_STOP_ABSOLUT);
	wnd.EnableWindow(FALSE);
	wnd = GetDlgItem(IDC_SPIN_STOP_ABSOLUT);
	wnd.EnableWindow(FALSE);

	Update_xQ(&m_properties);
}

void CDlgProperties::UpdateStopAbsolut()
{
	CWindow wnd;
	
	wnd = GetDlgItem(IDC_EDIT_STOP_RELATIVE);
	wnd.EnableWindow(FALSE);
	wnd = GetDlgItem(IDC_SPIN_STOP_RELATIVE);
	wnd.EnableWindow(FALSE);

	wnd = GetDlgItem(IDC_EDIT_STOP_ABSOLUT);
	wnd.EnableWindow(TRUE);
	wnd = GetDlgItem(IDC_SPIN_STOP_ABSOLUT);
	wnd.EnableWindow(TRUE);

	Update_xQ(&m_properties);
}

void CDlgProperties::Update_xQ(QuoteTable::Properties * pProperties, int set)
{
	CWindow wnd;
	TSTRING_STD2(str, size);
	CButton btn = GetDlgItem(IDC_RADIO_STOP_RELATIVE);
	BOOL isRelative = (btn.GetCheck() == BST_CHECKED) ? TRUE : FALSE;
	CWindow wndQ = GetDlgItem(IDC_EDIT_QUANTITY);
	wndQ.GetWindowText(str, size);
	int Q = StrToInt(str);
	int n;
	const int nd = pProperties->price.nd;

	TCHAR * pEnd;
	TSTRING_SMALL2(strRelative, sizeRelative);
	TSTRING_SMALL2(strAbsolute, sizeAbsolute);
	TSTRING_SMALL2(strSlippage, sizeSlippage);
	wnd = GetDlgItem(IDC_EDIT_STOP_RELATIVE);
	wnd.GetWindowText(strRelative, sizeRelative);
	double relative = _tcstod(strRelative, &pEnd);
	wnd = GetDlgItem(IDC_EDIT_STOP_ABSOLUT);
	wnd.GetWindowText(strAbsolute, sizeAbsolute);
	double absolute = _tcstod(strAbsolute, &pEnd);

	wnd = GetDlgItem(IDC_EDIT_SLIPPAGE);
	wnd.GetWindowText(strSlippage, sizeSlippage);
	double slippage = _tcstod(strSlippage, &pEnd);
#if 1
	n = _stprintf_s(str, size, TEXT("%d*("), Q);
	if (isRelative)
		n += _stprintf_s(str + n, size - n, TEXT("%s%%"), strRelative);
	else
		n += _stprintf_s(str + n, size - n, TEXT("%s"), strAbsolute);
	n += _stprintf_s(str + n, size - n, TEXT("+%s"), strSlippage);
	n += _stprintf_s(str + n, size - n, TEXT(") = "));
#else
	n = _stprintf_s(str, size, TEXT("= "));
#endif
	if (isRelative)
	{
		n += _stprintf_s(str + n, size - n, TEXT("%s%%"), strRelative);
		n += _stprintf_s(str + n, size - n, TEXT(" + "));
		n += FormatString(str + n, size - n, Q*slippage, nd);
	}
	else
		n += FormatString(str + n, size - n, Q*(absolute + slippage), nd);

	CEdit edit = GetDlgItem(IDC_EDIT_STOP_xQ);
	edit.SetWindowText(str);
#if 1
	// Выравниваем по праваому краю:
	n = edit.LineLength();
	edit.SetSel(n, n);
#endif
}

// CDlgProperties message handlers

BOOL CDlgProperties::OnInitDialog(CWindow wndFocus, LPARAM lInitParam)
{
	CREATESTRUCT cs;
	if (S_OK == theApp.LoadWindowPosition(&cs, GetName()))
		MoveWindow(cs.x, cs.y, cs.cx, cs.cy);

	HICON hIcon = theApp.LoadSmallIcon(IDI_EDIT);
	SetIcon(hIcon, FALSE);

	CComboBox comboBox(GetDlgItem(IDC_COMBO_TRADING_AUTOS));
	comboBox.AddString(TEXT("Параметры стоп-ордера"));
#if USE_AUTO_PROFIT
	comboBox.AddString(TEXT("Автоматический тэйк-профит"));
#endif
	comboBox.SetCurSel(0);

	UpdateProperties(&m_properties, SET);

	m_initialized = TRUE;

	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX Property Pages should return FALSE
}

void CDlgProperties::OnClose()
{
	SavePosition();

	EndDialog(IDCANCEL);
}

void CDlgProperties::OnBnClickedOk(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	SavePosition();
	UpdateProperties(&m_properties, 0);
	EndDialog(IDOK);
}

void CDlgProperties::OnBnClickedCancel(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	SavePosition();
	EndDialog(IDCANCEL);
}

LRESULT CDlgProperties::OnDeltaSpinPrice(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);

	CWindow wnd = GetDlgItem(IDC_EDIT_DPRICE);
	TSTRING_SMALL2(str, size);
	int len = wnd.GetWindowText(str, size);
	int n = CalculateLengthAfterPoint(str, len);
	TCHAR * pEnd;
	double d = _tcstod(str, &pEnd);
	double dd;
	if (n == 0)
		dd = 1;
	else
	{
		dd = 0.1;
		for (int i = 0; i < n-1; i++)
			dd *= 0.1;
	}
	if (pNMUpDown->iDelta < 0)
		d += dd;
	else
	{
		if (d - dd > 0)
			d -= dd;
	}
	FormatString(str, size, d, n);
	wnd.SetWindowText(str);

	return 0;
}

LRESULT CDlgProperties::OnDeltaSpinQuantity(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);

	CWindow wnd = GetDlgItem(IDC_EDIT_QUANTITY);
	TSTRING_SMALL2(str, size);
	wnd.GetWindowText(str, size);
	int val = StrToInt(str);
	if (pNMUpDown->iDelta < 0)
		val++;
	else
	{
		if (val > 1)
			val--;
	}
	_stprintf_s(str, TEXT("%d"), val);
	wnd.SetWindowText(str);

	return 0;
}

LRESULT CDlgProperties::OnDeltaposSpinSpread(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);

	CWindow wndPrice = GetDlgItem(IDC_EDIT_DPRICE);
	CWindow wnd = GetDlgItem(IDC_EDIT_SPREAD);
	TSTRING_SMALL2(str, size);
	TCHAR * pEnd;

	int len = wndPrice.GetWindowText(str, size);
	double dprice = _tcstod(str, &pEnd);
	int n = CalculateLengthAfterPoint(str, len);

	wnd.GetWindowText(str, size);
	double d = _tcstod(str, &pEnd);
	
	if (pNMUpDown->iDelta < 0)
		d += dprice;
	else
	{
		d -= dprice;
		if (d < 0)
			d = 0;
	}

	FormatString(str, size, d, n);
	wnd.SetWindowText(str);

	return 0;
}

void CDlgProperties::OnBnClickedRadioStopRelative(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateStopRelative();
	UpdatePropertiesTradingAutos(&m_properties, GET);
}

void CDlgProperties::OnBnClickedRadioStopAbsolut(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateStopAbsolut();
	UpdatePropertiesTradingAutos(&m_properties, GET);
}

void CDlgProperties::OnEnChangeEditQuantity(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	if (! m_initialized || m_changing)
		return;

	Update_xQ(&m_properties);
}

void CDlgProperties::OnEnChangeEditStopRelative(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	if (! m_initialized || m_changing)
		return;

	CButton btn(GetDlgItem(IDC_RADIO_STOP_RELATIVE));
	BOOL isRelative = (btn.GetCheck() == BST_CHECKED) ? TRUE : FALSE;
	if (isRelative)
		Update_xQ(&m_properties);
	UpdatePropertiesTradingAutos(&m_properties, GET);
}

void CDlgProperties::OnEnChangeEditStopAbsolut(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	if (! m_initialized || m_changing)
		return;

	CButton btn(GetDlgItem(IDC_RADIO_STOP_RELATIVE));
	BOOL isRelative = (btn.GetCheck() == BST_CHECKED) ? TRUE : FALSE;
	if (! isRelative)
		Update_xQ(&m_properties);
	UpdatePropertiesTradingAutos(&m_properties, GET);
}

void CDlgProperties::OnEnChangeEditSlippage(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	if (! m_initialized || m_changing)
		return;

	Update_xQ(&m_properties);
}

LRESULT CDlgProperties::OnDeltaposSpinStopRelative(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);

	CWindow wnd = GetDlgItem(IDC_EDIT_STOP_RELATIVE);
	TSTRING_SMALL2(str, size);
	int len = wnd.GetWindowText(str, size);
	int n = CalculateLengthAfterPoint(str, len);
	TCHAR * pEnd;
	double d = _tcstod(str, &pEnd);
	double dd;
	if (n == 0)
		dd = 1;
	else
	{
		dd = 0.1;
		for (int i = 0; i < n-1; i++)
			dd *= 0.1;
	}
	if (pNMUpDown->iDelta < 0)
		d += dd;
	else
	{
		if (d - dd > 0)
			d -= dd;
	}
	FormatString(str, size, d, n);
	wnd.SetWindowText(str);

	return 0;
}

LRESULT CDlgProperties::OnDeltaposSpinStopAbsolut(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);

	CWindow wndPrice = GetDlgItem(IDC_EDIT_DPRICE);
	CWindow wnd = GetDlgItem(IDC_EDIT_STOP_ABSOLUT);
	TSTRING_SMALL2(str, size);
	TCHAR * pEnd;

	int len = wndPrice.GetWindowText(str, size);
	double dprice = _tcstod(str, &pEnd);
	int n = CalculateLengthAfterPoint(str, len);

	wnd.GetWindowText(str, size);
	double d = _tcstod(str, &pEnd);
	
	if (pNMUpDown->iDelta < 0)
		d += dprice;
	else
	{
		d -= dprice;
		if (d < 0)
			d = 0;
	}

	FormatString(str, size, d, n);
	wnd.SetWindowText(str);

	return 0;
}

LRESULT CDlgProperties::OnDeltaposSpinSlippage(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);

	CWindow wndPrice = GetDlgItem(IDC_EDIT_DPRICE);
	CWindow wnd = GetDlgItem(IDC_EDIT_SLIPPAGE);
	TSTRING_SMALL2(str, size);
	TCHAR * pEnd;

	int len = wndPrice.GetWindowText(str, size);
	double dprice = _tcstod(str, &pEnd);
	int n = CalculateLengthAfterPoint(str, len);

	wnd.GetWindowText(str, size);
	double d = _tcstod(str, &pEnd);
	
	if (pNMUpDown->iDelta < 0)
		d += dprice;
	else
	{
		d -= dprice;
		if (d < 0)
			d = 0;
	}

	FormatString(str, size, d, n);
	wnd.SetWindowText(str);

	return 0;
}

void CDlgProperties::OnCbnSelchangeComboTradingAutos(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	if (! m_initialized)
		return;

	UpdatePropertiesTradingAutos(&m_properties, SET);
}
