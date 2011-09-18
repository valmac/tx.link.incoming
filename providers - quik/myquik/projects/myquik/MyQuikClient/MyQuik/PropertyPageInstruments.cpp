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
 *  PropertyPageInstruments.cpp : implementation file
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "PropertyPageInstruments.h"
#include <color.h>

enum {
	UPDATE_BUTTONS = 1,
	UPDATE_REMOVE = 2,
};

static struct EnableCtrl {
	BOOL add, addCopy, remove, removeAll, modify, select, export_, import_, properties;
} enable;

void OnRButton(CMyListBox & listBox, HWND hWntParent, CPoint & point)
{
	BOOL outside;
	UINT iSel;
#if USE_MYLISTBOX
	iSel = listBox.ItemFromPoint(point, outside);
#else
	UINT uFlags;
	iSel = listBox.HitTest(point, &uFlags);
	outside = (uFlags & LVHT_ONITEM) ? FALSE : TRUE;			
#endif
	if (! outside)
	{
#if USE_MYLISTBOX
		listBox.SetCurSel(iSel);
#else
#endif	
		::SendMessage(hWntParent, UM_UPDATE, UPDATE_BUTTONS, (LPARAM)listBox.m_hWnd);
	}

	CMenu root, menu;
	root.LoadMenu(IDR_MENU_SETTINGS_INSTRUMENT);
	menu = root.GetSubMenu(0);
	CMenu * pMenu = &menu;

	UINT flags = MF_BYCOMMAND;
#if 0
	pMenu->EnableMenuItem(ID_ADD_INSTRUMENT, flags|(enable.add ? MF_ENABLED : MF_DISABLED|MF_GRAYED)); 
	pMenu->EnableMenuItem(ID_REMOVE_INSTRUMENT, flags|(enable.remove ? MF_ENABLED : MF_DISABLED|MF_GRAYED)); 
	pMenu->EnableMenuItem(ID_REMOVE_INSTRUMENTS_ALL, flags|(enable.removeAll ? MF_ENABLED : MF_DISABLED|MF_GRAYED)); 
	pMenu->EnableMenuItem(ID_INSTRUMENT_PROPERTIES, flags|(enable.modify ? MF_ENABLED : MF_DISABLED|MF_GRAYED)); 
	pMenu->EnableMenuItem(ID_SELECT_INSTRUMENT, flags|(enable.select ? MF_ENABLED : MF_DISABLED|MF_GRAYED)); 
	pMenu->EnableMenuItem(ID_EXPORT_INSTRUMENTS, flags|(enable.export_ ? MF_ENABLED : MF_DISABLED|MF_GRAYED)); 
	pMenu->EnableMenuItem(ID_IMPORT_INSTRUMENTS, flags|(enable.import_ ? MF_ENABLED : MF_DISABLED|MF_GRAYED)); 
#else
	int n = 0;
	int removeSeparator = 0;
	if (! enable.addCopy)
	{
		pMenu->RemoveMenu(ID_ADD_INSTRUMENT_COPY, flags); n++;
	}
	if (! enable.modify)
	{
		pMenu->RemoveMenu(ID_INSTRUMENT_PROPERTIES, flags); n++;
	}

	if (! enable.select)
	{
		pMenu->RemoveMenu(ID_SELECT_INSTRUMENT, flags); n++;
		pMenu->RemoveMenu(5 - n, MF_BYPOSITION); n++; // удаление разделителя;
	}
	if (! enable.remove)
	{
		pMenu->RemoveMenu(ID_REMOVE_INSTRUMENT, flags); n++;
		removeSeparator++;
	}
	if (! enable.removeAll)
	{
		pMenu->RemoveMenu(ID_REMOVE_INSTRUMENTS_ALL, flags); n++;
		removeSeparator++;
	}
	if (removeSeparator == 2)
	{
		pMenu->RemoveMenu(8 - n, MF_BYPOSITION); n++;
	}
	if (! enable.export_)
	{
		pMenu->RemoveMenu(ID_EXPORT_INSTRUMENTS, flags); n++;
	}
	if (! enable.import_)
	{
		pMenu->RemoveMenu(ID_IMPORT_INSTRUMENTS, flags); n++;
	}
#endif
	GetCursorPos(&point);
	pMenu->TrackPopupMenu(TPM_LEFTALIGN |TPM_RIGHTBUTTON, point.x, point.y, hWntParent);
}

CMyListBox::CMyListBox()
{
	this->current = -1;
}

LRESULT CMyListBox::OnKeyUp(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	int vkey = static_cast<int>(wParam);
	if (vkey == VK_DELETE)
	{
		::SendMessage(GetParent(), UM_UPDATE, (WPARAM)UPDATE_REMOVE, 0);
		bHandled = TRUE;
	}
	return 0;
}

#if USE_MYLISTBOX
void CMyListBox::OnRButtonUp(UINT nFlags, CPoint point)
{
	OnRButton(*this, GetParent(), point);
}
#endif // #if USE_MYLISTBOX

void CMyListBox::DrawItem(int nIDCtl, LPDRAWITEMSTRUCT lpDrawItemStruct)
{
	int iItem = lpDrawItemStruct->itemID;
	if (iItem < 0)
	   return;

	TSTRING_STD2(str, size);
#if USE_MYLISTBOX
	this->GetText(iItem, str);
#else
	this->GetItemText(iItem, 0, str, size);
#endif
	LPCTSTR lpszText = (LPCTSTR)str;// lpDrawItemStruct->itemData;
	CDC dc;

	dc.Attach(lpDrawItemStruct->hDC);

	// Save these value to restore them when done drawing.
	COLORREF crOldTextColor = dc.GetTextColor();
	COLORREF crOldBkColor = dc.GetBkColor();
	COLORREF colorHighlight, colorInactive;
	COLORREF background, color;
	double opacity;
	
	GetHighlightParams(*this, colorHighlight, colorInactive, colorHighlight, opacity);
	color = crOldTextColor; background = crOldBkColor;

	CRect rect(lpDrawItemStruct->rcItem);

	if (this->current == iItem)
	{
		color = RGB(250, 250, 250);
		background = ::GetSysColor(COLOR_WINDOWFRAME); // RGB(20, 20, 20);
		//rect.DeflateRect(1, 1);
	}

	dc.SetBkMode(TRANSPARENT);	
	// If this item is selected, set the background color and the text color to appropriate values. 
	// Also, erase rect by filling it with the background color.
	if ((lpDrawItemStruct->itemAction | (ODA_SELECT|ODA_FOCUS)) &&
	  (lpDrawItemStruct->itemState & (ODS_SELECTED|ODS_FOCUS)))
	{
		background = my::lib::SumColorsAlpha(background, colorHighlight, opacity);
		dc.FillSolidRect(&rect, background);
	}
	else
		dc.FillSolidRect(&rect, background);

	// If this item has the focus, draw a red frame around the item's rect.
	if ((lpDrawItemStruct->itemAction | ODA_FOCUS) && (lpDrawItemStruct->itemState & ODS_FOCUS))
	{
		CBrush br;
		br.CreateSolidBrush(colorHighlight);
		dc.FrameRect(&lpDrawItemStruct->rcItem, br);
	}

	dc.SetTextColor(color);
	dc.DrawText(lpszText, static_cast<int>(_tcslen(lpszText)), &lpDrawItemStruct->rcItem, DT_CENTER|DT_SINGLELINE|DT_VCENTER);

	dc.SetTextColor(crOldTextColor);
	dc.SetBkColor(crOldBkColor);

	dc.Detach();
}


// CPropertyPageInstruments dialog

CPropertyPageInstruments::CPropertyPageInstruments(LPCTSTR name)
{
	m_initialized = FALSE;
	m_name = CString(name);
}

CPropertyPageInstruments::~CPropertyPageInstruments()
{
}

int CPropertyPageInstruments::AddStringToListBox(LPCTSTR name, LPCTSTR seccode)
{
	if (lstrlen(name) > 0)
	{
#if USE_MYLISTBOX
		if (CB_ERR == m_listBox.FindStringExact(0, name))
		{
			int iItem = m_listBox.AddString(name);
		}
#else
		LVFINDINFO info;
		info.flags = LVFI_STRING;
		info.psz = name;		
		if (-1 == m_listBox.FindItem(&info, 0))
			m_listBox.InsertItem(m_listBox.GetItemCount(), name);
#endif
	}
	return 0;
}

void CPropertyPageInstruments::SelectInstrument(LPCTSTR name, const QuoteTable::Properties * pProperties, int select)
{
	if (select)
	{
#if USE_MYLISTBOX
		int iItem = m_listBox.FindStringExact(0, name);
		if (iItem != CB_ERR)
		{			
			m_listBox.SelectString(-1, name);
			m_listBox.SetCurrent(iItem);
		}
#else
		LVFINDINFO info;
		info.flags = LVFI_STRING;
		info.psz = name;		
		int iItem = m_listBox.FindItem(&info, -1);
		if (iItem >= 0)
		{
			my::ctrl::SetFocusedItem(m_listBox, iItem);
			m_listBox.SetCurrent(iItem);
		}
#endif
	}

	CWindow wnd;
	LPCTSTR msg;
	TSTRING_STD2(str, size);
	QuoteTable::Properties properties;
	if (pProperties != NULL)
	{
#if 1			
		_stprintf_s(str, size, TEXT("%s%s%s"), 
			pProperties->seccode, (0 == lstrlen(pProperties->classcode) ? TEXT("") : TEXT(" / ")), pProperties->classcode);
		msg = str;
#else
		msg = pProperties->seccode;
#endif
	}
	else
	{
		msg = TEXT("");
		pProperties = &properties;
		memset(&properties, 0, sizeof(properties));
	}
	wnd = GetDlgItem(IDC_STATIC_SECCODE);
	wnd.SetWindowText((pProperties->seccode[0]) ? pProperties->seccode : TEXT("-"));
	wnd = GetDlgItem(IDC_STATIC_CLASSCODE);
	wnd.SetWindowText((pProperties->classcode[0]) ? pProperties->classcode : TEXT("-"));
	wnd = GetDlgItem(IDC_STATIC_CLIENTCODE);
	wnd.SetWindowText((pProperties->clientcode[0]) ? pProperties->clientcode : TEXT("-"));
	wnd = GetDlgItem(IDC_STATIC_ACCOUNT);
	wnd.SetWindowText((pProperties->account[0]) ? pProperties->account : TEXT("-"));

	FormatString(str, size, pProperties->price.step, pProperties->price.nd);
	wnd = GetDlgItem(IDC_STATIC_PRICE_STEP);
	wnd.SetWindowText(str);

	_stprintf_s(str, size, TEXT("%.0f"), pProperties->trading.quantity);
	wnd = GetDlgItem(IDC_STATIC_QUANTITY);
	wnd.SetWindowText(str);

	FormatString(str, size, pProperties->trading.spread, pProperties->price.nd);
	wnd = GetDlgItem(IDC_STATIC_SPREAD);
	wnd.SetWindowText(str);

	TSTRING_SMALL(strValue);
	TSTRING_SMALL(strResult);
	TSTRING_SMALL(strRelative);
	TSTRING_SMALL(strAbsolute);
	TSTRING_SMALL(strSlippage);
	BOOL isRelative = pProperties->trading.autos.stop.isRelative;
	double relative = pProperties->trading.autos.stop.relative;
	double absolute = pProperties->trading.autos.stop.absolute;
	double slippage = pProperties->trading.autos.stop.slippage;
	double Q = pProperties->trading.quantity;
	int nd = pProperties->price.nd;
	int n = 0;

	if (isRelative)
	{
		FormatString(strRelative, SIZEOF_ARRAY(strRelative), relative, 2);
	}
	else
	{
		n += _stprintf_s(str, size, TEXT("%.0f*("), Q);
		FormatString(strAbsolute, SIZEOF_ARRAY(strAbsolute), absolute, nd);
		n += _stprintf_s(str + n, size - n, TEXT("%s"), strAbsolute);
		FormatString(strSlippage, SIZEOF_ARRAY(strSlippage), slippage, nd);
		n += _stprintf_s(str + n, size - n, TEXT("+%s"), strSlippage);
		n += _stprintf_s(str + n, size - n, TEXT(") = "));
	}

	if (isRelative)
	{
		n += _stprintf_s(str + n, size - n, TEXT("%s%%"), strRelative);
		n += _stprintf_s(str + n, size - n, TEXT(" + "));
		n += FormatString(str + n, size - n, Q*slippage, nd);
	}
	else
		n += FormatString(str + n, size - n, Q*(absolute + slippage), nd);

	wnd = GetDlgItem(IDC_STATIC_STOP);
	wnd.SetWindowText(str);

	if (pProperties->trading.autos.profit.isRelative)
	{
		FormatString(strResult, SIZEOF_ARRAY(strResult), pProperties->trading.autos.profit.relative, 2);
		FormatString(strValue, SIZEOF_ARRAY(strValue), pProperties->trading.autos.profit.relative, 2);
		_stprintf_s(str, size, TEXT("%s%%"), strResult);
	}
	else
	{
		FormatString(strResult, SIZEOF_ARRAY(strResult), Q * pProperties->trading.autos.profit.absolute, pProperties->price.nd);
		FormatString(strValue, SIZEOF_ARRAY(strValue), pProperties->trading.autos.profit.absolute, pProperties->price.nd);
		_stprintf_s(str, size, TEXT("%.0f x %s = %s"), pProperties->trading.quantity, strValue, strResult);
	}
		
	wnd = GetDlgItem(IDC_STATIC_PROFIT);
	wnd.SetWindowText(str);
#if 0
	wnd = GetDlgItem(IDC_STATIC_TOOL);
	wnd.SetWindowText(msg);
#endif
}

void CPropertyPageInstruments::UpdateListOfInstruments(BOOL clear)
{
	int iSel = GetCurSel();

	TSTRING_STD2(name, size);
	if (0 == m_listBox.GetWindowText(name, size))
		SAFE_TSTRCPY(name, theApp.m_settings.trading.instrument.name);

	if (clear)
	{
#if USE_MYLISTBOX
		m_listBox.ResetContent();
#else
		m_listBox.DeleteAllItems();
#endif
	}

	// Заполняем список известных инструментов:
	const ListOfTableProperties * pListOfProperties = theApp.GetListOfTableProperties();
	if (pListOfProperties != NULL)
	{
		ListOfTableProperties::const_iterator it;
		for (it = pListOfProperties->begin(); it != pListOfProperties->end(); it++)
		{
			const QuoteTable::Properties * pProperties = &*(it);
			if (clear)
				AddStringToListBox(pProperties->name, pProperties->seccode);
			if (0 == lstrcmp(name, pProperties->name))
			{
				SelectInstrument(pProperties->name, pProperties);
			}
		}
	}

	int count = GetItemCount();
	if (iSel >= 0 && count && clear)
	{		
		if (iSel >= count)
			iSel = count - 1;
		SetCurSel(iSel);
	}

	UpdateButtons();
}

void CPropertyPageInstruments::UpdateButtons(int iSel)
{
	enable.add = TRUE;
	enable.addCopy = FALSE;
	enable.remove = FALSE;
	enable.modify = FALSE;
	enable.select = FALSE;
	enable.properties = FALSE;
	enable.import_ = TRUE;
	int count;
#if USE_MYLISTBOX
	count = m_listBox.GetCount();
#else
	count = m_listBox.GetItemCount();
#endif
	if (count > 0)
	{		
		enable.removeAll = TRUE;
		if (iSel == -1)
			iSel = GetCurSel();
		if (iSel >= 0)
		{
			enable.addCopy = TRUE;
			enable.remove = TRUE;
			enable.modify = TRUE;
			TSTRING_STD2(name, size);
			GetItemText(iSel, name, size);
			if (0 != lstrcmp(m_name, name))
				enable.select = TRUE;
			enable.properties = TRUE;
		}
		enable.export_ = TRUE;
	}
	else
	{		
		enable.removeAll = FALSE;
		enable.export_ = FALSE;
	}
	CWindow wnd;
	wnd = GetDlgItem(IDC_BUTTON_ADD); wnd.EnableWindow(enable.add);
	wnd = GetDlgItem(IDC_BUTTON_ADD_COPY); wnd.EnableWindow(enable.addCopy);
	wnd = GetDlgItem(IDC_BUTTON_REMOVE); wnd.EnableWindow(enable.remove);
	wnd = GetDlgItem(IDC_BUTTON_MODIFY); wnd.EnableWindow(enable.modify);
	wnd = GetDlgItem(IDC_BUTTON_SELECT); wnd.EnableWindow(enable.select);
	wnd = GetDlgItem(IDC_BUTTON_EXPORT); wnd.EnableWindow(enable.export_);
	wnd = GetDlgItem(IDC_BUTTON_IMPORT); wnd.EnableWindow(enable.import_);
#if 0
	wnd = GetDlgItem(IDC_STATIC_SECCODE); wnd.EnableWindow(enable.properties);
	wnd = GetDlgItem(IDC_STATIC_CLASSCODE); wnd.EnableWindow(enable.properties);
	wnd = GetDlgItem(IDC_STATIC_CLIENTCODE); wnd.EnableWindow(enable.properties);
	wnd = GetDlgItem(IDC_STATIC_ACCOUNT); wnd.EnableWindow(enable.properties);
	wnd = GetDlgItem(IDC_STATIC_PRICE_STEP); wnd.EnableWindow(enable.properties);
	wnd = GetDlgItem(IDC_STATIC_QUANTITY); wnd.EnableWindow(enable.properties);
	wnd = GetDlgItem(IDC_STATIC_SPREAD); wnd.EnableWindow(enable.properties);
	wnd = GetDlgItem(IDC_STATIC_STOP); wnd.EnableWindow(enable.properties);
	wnd = GetDlgItem(IDC_STATIC_PROFIT); wnd.EnableWindow(enable.properties);
#else
	if (! enable.properties)
	{
		LPCTSTR text = TEXT("-");
		wnd = GetDlgItem(IDC_STATIC_SECCODE); wnd.SetWindowText(text);
		wnd = GetDlgItem(IDC_STATIC_CLASSCODE); wnd.SetWindowText(text);
		wnd = GetDlgItem(IDC_STATIC_CLIENTCODE); wnd.SetWindowText(text);
		wnd = GetDlgItem(IDC_STATIC_ACCOUNT); wnd.SetWindowText(text);
		wnd = GetDlgItem(IDC_STATIC_PRICE_STEP); wnd.SetWindowText(text);
		wnd = GetDlgItem(IDC_STATIC_QUANTITY); wnd.SetWindowText(text);
		wnd = GetDlgItem(IDC_STATIC_SPREAD); wnd.SetWindowText(text);
		wnd = GetDlgItem(IDC_STATIC_STOP); wnd.SetWindowText(text);
		wnd = GetDlgItem(IDC_STATIC_PROFIT); wnd.SetWindowText(text);
	}
#endif
}

// CPropertyPageInstruments message handlers

BOOL CPropertyPageInstruments::OnApply()
{
	CWindow wnd = GetParent();
	if (wnd)
		wnd.SendMessage(UM_APPLY, this->IDD, (LPARAM)(LPCTSTR)m_name);

	SetModified(FALSE);

	return CPropertyPageInstrumentsBase::OnApply();
}

BOOL CPropertyPageInstruments::OnQueryCancel()
{
	CWindow wnd = GetParent();
	if (wnd)
		wnd.SendMessage(UM_CANCEL, this->IDD, (LPARAM)(LPCTSTR)m_name);

	return CPropertyPageInstrumentsBase::OnQueryCancel();
}

BOOL CPropertyPageInstruments::OnInitDialog(CWindow wndFocus, LPARAM lInitParam)
{
	HINSTANCE hInst = theApp.GetModuleInstance();

	DoDataExchange(FALSE);

	BOOL modern = TRUE;
#if AUTOSELECT_MODERN_THEME
	modern = IsModernOS();
#endif
	CButton btn;
	btn = GetDlgItem(IDC_BUTTON_ADD);
	if (modern)
		btn.SetIcon(theApp.LoadSmallIcon(IDI_NEW));
	btn = GetDlgItem(IDC_BUTTON_ADD_COPY);
	if (modern)
		btn.SetIcon(theApp.LoadSmallIcon(IDI_COPY));
	btn = GetDlgItem(IDC_BUTTON_REMOVE);
	if (modern)
		btn.SetIcon(theApp.LoadSmallIcon(IDI_DELETE));
	btn = GetDlgItem(IDC_BUTTON_MODIFY);
	if (modern)
		btn.SetIcon(theApp.LoadSmallIcon(IDI_EDIT));
	btn = GetDlgItem(IDC_BUTTON_SELECT);
	UINT style = btn.GetButtonStyle();
	if (modern)
		btn.SetIcon(theApp.LoadSmallIcon(IDI_SELECT));

	m_hints.Create(*this);
	m_hints.AddTool (CToolInfo (TTF_SUBCLASS, GetDlgItem(IDC_BUTTON_ADD), 0, NULL, TEXT("Добавить новый инструмент")));
	m_hints.AddTool (CToolInfo (TTF_SUBCLASS, GetDlgItem(IDC_BUTTON_ADD_COPY), 0, NULL, TEXT("Добавить копию инструмента")));
	m_hints.AddTool (CToolInfo (TTF_SUBCLASS, GetDlgItem(IDC_BUTTON_REMOVE), 0, NULL, TEXT("Удалить выбранный инструмент")));
	m_hints.AddTool (CToolInfo (TTF_SUBCLASS, GetDlgItem(IDC_BUTTON_MODIFY), 0, NULL, TEXT("Редактировать параметры инструмента")));
	m_hints.AddTool (CToolInfo (TTF_SUBCLASS, GetDlgItem(IDC_BUTTON_SELECT), 0, NULL, TEXT("Выбрать инструмент для работы")));
	m_hints.AddTool (CToolInfo (TTF_SUBCLASS, GetDlgItem(IDC_BUTTON_EXPORT), 0, NULL, TEXT("Сохранить список инструментов в файл")));
	m_hints.AddTool (CToolInfo (TTF_SUBCLASS, GetDlgItem(IDC_BUTTON_IMPORT), 0, NULL, TEXT("Загрузить список инструментов из файла")));


	CWindow listBox = GetDlgItem(IDC_LIST1);
	CWindow listCtrl = GetDlgItem(IDC_LIST2);
#if USE_MYLISTBOX
	::ShowWindow(listBox, SW_SHOW);
	::ShowWindow(listCtrl, SW_HIDE);
#else
	CRect rect;
	listBox.GetWindowRect(&rect);
	this->ScreenToClient(&rect);
	listCtrl.MoveWindow(&rect);
	::ShowWindow(listBox, SW_HIDE);
	::ShowWindow(listCtrl, SW_SHOW);
	listBox.GetClientRect(&rect);
	m_listBox.InsertColumn(0, TEXT("List"), LVCFMT_CENTER, rect.Width());
#endif

	UpdateListOfInstruments(TRUE);

	m_initialized = TRUE;

	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX Property Pages should return FALSE
}

#if USE_MYLISTBOX
void CPropertyPageInstruments::OnLbnSelchangeList1(UINT uNotifyCode, int nID, CWindow wndCtl)
#else
LRESULT CPropertyPageInstruments::OnLvnItemchangedList2(NMHDR *pNMHDR)
#endif
{
	if (! m_initialized)
		return 0;

	int iSel;
#if USE_MYLISTBOX
	iSel = GetCurSel();
#else
	LPNMLISTVIEW pNMLV = reinterpret_cast<LPNMLISTVIEW>(pNMHDR);
	if (pNMLV->uOldState)
		return 0;
	iSel = pNMLV->iItem;
#endif
	if (iSel >= 0)
	{
		TSTRING_STD2(name, size);
		GetItemText(iSel, name, size);
		const QuoteTable::Properties * pProperties = theApp.FindTableProperty(name);
		SelectInstrument(name, pProperties, 0);
		UpdateButtons(iSel);
	}	
#if USE_MYLISTBOX
#else
	return 0;
#endif
}

#if USE_MYLISTBOX
void CPropertyPageInstruments::OnLbnDblclkList1(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnBnClickedButtonSelect(uNotifyCode, nID, wndCtl);
}
#else
LRESULT CPropertyPageInstruments::OnNMDblClickList2(NMHDR *pNMHDR)
{
	OnBnClickedButtonSelect(0, 0, NULL);
	return 0;
}

LRESULT CPropertyPageInstruments::OnNMRClickList2(NMHDR *pNMHDR)
{
	LPNMITEMACTIVATE pNMItemActivate = reinterpret_cast<LPNMITEMACTIVATE>(pNMHDR);
	CPoint point(pNMItemActivate->ptAction);
	OnRButton(m_listBox, *this, point);
	return 0;
}

#endif // #if USE_MYLISTBOX

void CPropertyPageInstruments::OnBnClickedButtonAdd(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnAddInstrument(uNotifyCode, nID, wndCtl);
	m_listBox.SetFocus();
}

void CPropertyPageInstruments::OnBnClickedButtonAddCopy(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnAddInstrumentCopy(uNotifyCode, nID, wndCtl);
	m_listBox.SetFocus();
}

void CPropertyPageInstruments::OnBnClickedButtonRemove(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnRemoveInstrument(uNotifyCode, nID, wndCtl);
	m_listBox.SetFocus();
}

void CPropertyPageInstruments::OnBnClickedButtonModify(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnInstrumentProperties(uNotifyCode, nID, wndCtl);
	m_listBox.SetFocus();
}

void CPropertyPageInstruments::OnBnClickedButtonSelect(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnSelectInstrument(uNotifyCode, nID, wndCtl);
	m_listBox.SetFocus();
}

LRESULT CPropertyPageInstruments::OnWndMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = TRUE;
	switch (uMsg)
	{
	case UM_UPDATE_INSTRUMENTS:
		{// 
			UpdateListOfInstruments(TRUE);
			m_listBox.SetFocus();
		}
		break;
	case UM_UPDATE:
		{
			int what = static_cast<int>(wParam);
			HWND hWnd = reinterpret_cast<HWND>(lParam);
			if (what == UPDATE_BUTTONS)
				UpdateButtons();
			else if (what == UPDATE_REMOVE)
				OnRemoveInstrument(0, 0, NULL);
			m_listBox.SetFocus();
		}
		break;
	default:
		bHandled = FALSE;
	}
	return 0;
}

void CPropertyPageInstruments::OnAddInstrument(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	CWindow wnd = theApp.GetMainWnd();
	if (wnd)
	{
		CString name(TEXT("Новый инструмент"));
		ParamsShowProperties params(name, NULL, ParamsShowProperties::F_NEW);
		wnd.SendMessage(UM_SHOW_PROPERTIES, (WPARAM)&params, (LPARAM)this->m_hWnd);
	}
}

int CPropertyPageInstruments::GetCurSel() const
{
	int iSel;
#if USE_MYLISTBOX
	iSel = m_listBox.GetCurSel();
#else
	iSel = my::ctrl::GetFocusedItem(m_listBox);
#endif
	return iSel;
}

void CPropertyPageInstruments::SetCurSel(int iSel)
{
#if USE_MYLISTBOX
	m_listBox.SetCurSel(iSel);
#else
	my::ctrl::SetFocusedItem(m_listBox, iSel);
#endif
}

int CPropertyPageInstruments::GetItemCount() const
{
	int count;
#if USE_MYLISTBOX
	count = m_listBox.GetItemCount();
#else
	count = m_listBox.GetItemCount();
#endif
	return count;
}

int CPropertyPageInstruments::GetItemText(int i, LPTSTR dst, size_t size)
{
	int len;
#if USE_MYLISTBOX
	len = m_listBox.GetText(i, dst);
#else
	len = m_listBox.GetItemText(i, 0, dst, size);
#endif
	return len;
}

void CPropertyPageInstruments::OnAddInstrumentCopy(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	CWindow wnd = theApp.GetMainWnd();
	if (wnd)
	{
		int iSel = GetCurSel();
		if (iSel >= 0)
		{
			TSTRING_STD2(name, size);
			GetItemText(iSel, name, size);
			QuoteTable::Properties properties;
			const QuoteTable::Properties * pProperties = theApp.FindTableProperty(name);
			if (pProperties != NULL)
			{
				properties = *pProperties;
				pProperties = &properties;
			}
			lstrcat(name, TEXT(" - копия"));
			ParamsShowProperties params(name, pProperties, ParamsShowProperties::F_COPY);
			wnd.SendMessage(UM_SHOW_PROPERTIES, (WPARAM)&params, (LPARAM)this->m_hWnd);
		}
	}
}

void CPropertyPageInstruments::OnRemoveInstrument(UINT uNotifyCode, int nID, CWindow wndCtl)
{	
	int iSel = GetCurSel();
	if (iSel >= 0)
	{
		TSTRING_STD2(name, size);
		GetItemText(iSel, name, size);
		CString msg;
		msg.Format(TEXT("Вы уверены, что хотите удалить инструмент \"%s\"?"), name);
		if (IDYES == my::MessageBox(*this, msg, MB_ICONQUESTION|MB_YESNOCANCEL))
		{
			theApp.RemoveInstrument(name);
			UpdateListOfInstruments(TRUE);
			if (m_listBox.GetCurrent() == iSel)
			{
				m_listBox.SetCurrent(-1);
				m_name.Empty();

				CWindow wnd = GetParent();
				if (wnd)
					wnd.SendMessage(UM_APPLY, this->IDD, (LPARAM)(LPCTSTR)m_name);
			}
		}
	}
}

void CPropertyPageInstruments::OnRemoveInstrumentsAll(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	
	CString msg;
	msg.Format(TEXT("Вы уверены, что хотите удалить все инструменты?"));
	if (IDYES == my::MessageBox(*this, msg, MB_ICONQUESTION|MB_YESNOCANCEL))
	{
#if 0
		theApp.RemoveInstrumentsAll();
#else		
		int count;
#if USE_MYLISTBOX
		count = m_listBox.GetCount();
#else
		count = m_listBox.GetItemCount();
#endif
		for (int i = 0; i < count; i++)
		{
			TSTRING_STD2(name, size);
			GetItemText(i, name, size);
			theApp.RemoveInstrument(name);
		}
#endif
		UpdateListOfInstruments(TRUE);
		m_name.Empty();

		CWindow wnd = GetParent();
		if (wnd)
			wnd.SendMessage(UM_APPLY, this->IDD, (LPARAM)(LPCTSTR)m_name);
	}
}

void CPropertyPageInstruments::OnInstrumentProperties(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	CWindow wnd = theApp.GetMainWnd();
	if (wnd)
	{
		int iSel = GetCurSel();
		if (iSel >= 0)
		{
			TSTRING_STD2(name, size);
			GetItemText(iSel, name, size);
			int flags = (m_listBox.GetCurrent() == iSel) ? 0 : ParamsShowProperties::F_EDIT;
			ParamsShowProperties params(name, NULL, flags);
			wnd.SendMessage(UM_SHOW_PROPERTIES, (WPARAM)&params, (LPARAM)this->m_hWnd);
		}
	}
}

void CPropertyPageInstruments::OnSelectInstrument(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	int iSel = GetCurSel();
	if (iSel >= 0)
	{
		TSTRING_STD2(name, size);
		GetItemText(iSel, name, size);
		if (0 != lstrcmp(m_name, name))
		{
			m_name = name;

			CWindow parentWnd = theApp.GetMainWnd();
			if (parentWnd != NULL)
				parentWnd.SendMessage(UM_APPLY_SETTINGS, APPLY_SETTINGS_INSTRUMENT, (LPARAM)(LPCTSTR)m_name);

			UpdateListOfInstruments(FALSE);
			this->Invalidate(FALSE);

			SetModified();
		}
	}
}

void CPropertyPageInstruments::OnBnClickedButtonExport(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnExportInstruments(uNotifyCode, nID, wndCtl);
}

void CPropertyPageInstruments::OnBnClickedButtonImport(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnImportInstruments(uNotifyCode, nID, wndCtl);
}

static LPCTSTR s_strFilter = TEXT("Текстовые файлы\0*.txt\0Все файлы\0*.*\0\0");

void CPropertyPageInstruments::OnExportInstruments(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OPENFILENAME ofn;
	TCHAR szFile[MAX_PATH_EX];

	ZeroMemory(&ofn, sizeof(ofn));
	ofn.lStructSize = sizeof(ofn);
	ofn.hwndOwner = *this;
	ofn.lpstrFile = szFile;
	ofn.lpstrFile[0] = '\0';
	ofn.nMaxFile = sizeof(szFile);
	ofn.lpstrFilter = s_strFilter;
	ofn.nFilterIndex = 1;
	ofn.lpstrDefExt = TEXT("txt");
#if 0
	TCHAR buf[MAX_PATH_EX];
	SAFE_TSTRCPY(buf, TEXT("instruments"));
	ofn.lpstrFile = buf;
	ofn.nMaxFile = SIZEOF_ARRAY(buf);
#endif
	if (TRUE == ::GetSaveFileName(&ofn)) 
	{
		LPCTSTR name = szFile;
		if (S_OK == theApp.DoExportInstruments(name))
		{			
			CString msg;
			msg.Format(TEXT("Список интструментов успешно сохранён в файл \n\"%s\""), name);
			my::MessageBox(*this, msg, MB_ICONINFORMATION);
		}
		else
			my::MessageBox(*this, TEXT("Ошибка экспорта данных!"), MB_ICONERROR);
	}
}

void CPropertyPageInstruments::OnImportInstruments(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OPENFILENAME ofn;
	TCHAR szFile[MAX_PATH_EX];

	ZeroMemory(&ofn, sizeof(ofn));
	ofn.lStructSize = sizeof(ofn);
	ofn.hwndOwner = *this;
	ofn.lpstrFile = szFile;
	ofn.lpstrFile[0] = '\0';
	ofn.nMaxFile = sizeof(szFile);
	ofn.lpstrFilter = s_strFilter;
	ofn.nFilterIndex = 1;
	ofn.lpstrDefExt = TEXT("txt");
	if (TRUE == ::GetOpenFileName(&ofn)) 
	{
		LPCTSTR name = szFile;
		if (S_OK == theApp.DoImportInstruments(name))
		{
			UpdateListOfInstruments(TRUE);
			theApp.SaveTableProperties();
			theApp.UpdateTablesProperties();

			CString msg;
			msg.Format(TEXT("Список интструментов успешно загружен из файла \n\"%s\""), name);
			my::MessageBox(*this, msg, MB_ICONINFORMATION);
		}
		else
			my::MessageBox(*this, TEXT("Ошибка импорта данных!"), MB_ICONERROR);
	}
}
