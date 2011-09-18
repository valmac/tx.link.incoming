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
 *  DlgStop2.cpp : implementation file
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "DlgStop2.h"
#include "dlg.h"
#include "transactions.h"

// CDlgStop2 dialog

CDlgStop2::CDlgStop2()
{
	m_pTable = NULL;
	m_hActiveWindow = NULL;
	m_operation = Transaction::OPERATION_BUY;
}

CDlgStop2::~CDlgStop2()
{
}

void CDlgStop2::Init(QuoteTable * pTable, HWND hActiveWindow)
{
	m_pTable = pTable;
	const QuoteTable::Properties * pProperties = pTable ? pTable->GetProperties() : NULL;
#if 0
	if (pProperties != NULL)
#endif
	{
		if (::IsWindowEnabled(this->m_hWnd))
			UpdateControls(pProperties);
	}
	m_hActiveWindow = hActiveWindow;
}

void CDlgStop2::UpdateControls(const QuoteTable::Properties * pProperties)
{
	CComboBox combo;
	BOOL enable;
	LPCTSTR name = TEXT("Стоп-заявка");
	if (pProperties != NULL)
	{
		enable = TRUE;
		int iCur;
		// Код инструмента:
		combo = GetDlgItem(IDC_COMBO_INSTRUMENT);
		iCur = combo.FindStringExact(0, pProperties->name);
		if (iCur >= 0)
			combo.SetCurSel(iCur);
		// Номер счета:
		combo = GetDlgItem(IDC_COMBO_ACCOUNT);
		iCur = combo.FindStringExact(0, pProperties->account);
		if (iCur >= 0)
			combo.SetCurSel(iCur);
	}
	else
	{
		enable = FALSE;
	}
	this->SetWindowText(name);

	CWindow spinStopPrice = GetDlgItem(IDC_SPIN_STOP_STOPPRICE);
	CWindow spinPrice = GetDlgItem(IDC_SPIN_STOP_PRICE);
	spinStopPrice.EnableWindow(enable);
	spinPrice.EnableWindow(enable);

	RestoreUserValues(GetCurrentTable());
}

void CDlgStop2::KeepUserValues(QuoteTable * pTable)
{
	m_edits.price.Push(&pTable->user.sps);
	m_edits.price2.Push(&pTable->user.sps2);
	m_edits.quantity.Push(&pTable->user.sqs);
}

void CDlgStop2::RestoreUserValues(QuoteTable * pTable)
{
	const QuoteTable::Properties * pProperties = pTable->GetProperties();
	m_edits.price.Pop(&pTable->user.sps, pProperties);
	m_edits.price2.Pop(&pTable->user.sps2, pProperties);
	m_edits.quantity.Pop(&pTable->user.sqs, NULL);
}

// CDlgStop2 message handlers

BOOL CDlgStop2::OnInitDialog(CWindow wndFocus, LPARAM lInitParam)
{
	DoDataExchange(FALSE);

	m_picture = GetDlgItem(IDC_PICTURE);
	my::DialogMessageHook::InstallHook(*this);
#if 1
	CREATESTRUCT cs;
	if (S_OK == theApp.LoadWindowPosition(&cs, GetName()))
		MoveWindow(cs.x, cs.y, cs.cx, cs.cy);
#endif
	HICON hIcon = theApp.LoadSmallIcon(IDI_STOP);
	SetIcon(hIcon, TRUE); 
	SetIcon(hIcon, FALSE); 

	CComboBox combo(GetDlgItem(IDC_COMBO_TYPE));
	combo.AddString(TEXT("Стоп-лимит")); //SIMPLE_STOP_ORDER
	combo.AddString(TEXT("С условием по другой бумаге")); //CONDITION_PRICE_BY_OTHER_SEC
	combo.AddString(TEXT("Со связанной заявкой")); //WITH_LINKED_LIMIT_ORDER
	combo.AddString(TEXT("Тэйк-профит")); //TAKE_PROFIT_STOP_ORDER
	combo.SetCurSel(0);
	combo.EnableWindow(FALSE);

	CButton radio(GetDlgItem(IDC_RADIO_EXPIRE_TODAY));
	radio.SetCheck(BST_CHECKED);

	CWindow wnd = GetDlgItem(IDC_EXPIRE_DATE);
	wnd.EnableWindow(FALSE);

	UINT idBuySell, idGreaterLesser, idPict;
	if (m_operation == Transaction::OPERATION_BUY)
	{
		idBuySell = IDC_RADIO_BUY;
		idGreaterLesser = IDC_RADIO_GREATER;
		idPict = IDI_BUY_BIG;
	}
	else if (m_operation == Transaction::OPERATION_SELL)
	{
		idBuySell = IDC_RADIO_SELL;
		idGreaterLesser = IDC_RADIO_LESSER;
		idPict = IDI_SELL_BIG;
	}
	CButton radioBuySell = GetDlgItem(idBuySell);
	CButton radioGreaterLesser = GetDlgItem(idGreaterLesser);
	radioBuySell.SetCheck(BST_CHECKED);
	radioGreaterLesser.SetCheck(BST_CHECKED);
	SetPicture(idPict);

	// Заполняем список инструментов и список счетов известными значениями:
	const ListOfTableProperties * pListOfProperties = theApp.GetListOfTableProperties();
	if (pListOfProperties != NULL)
	{
		ListOfTableProperties::const_iterator it;
		for (it = pListOfProperties->begin(); it != pListOfProperties->end(); it++)
		{
			const QuoteTable::Properties * pProperties= &*(it);
			AddStringToCombo(pProperties->name, IDC_COMBO_INSTRUMENT);
			AddStringToCombo(pProperties->account, IDC_COMBO_ACCOUNT);
		}
	}

	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX Property Pages should return FALSE
}

void CDlgStop2::OnBnClickedOk(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	TSTRING_STD(str);
	TSTRING_SMALL(strName);

	CWindow wnd;
	Transaction ta;
	TCHAR * pEnd;

	QuoteTable * pTable = m_pTable;
	const QuoteTable::Properties * pProperties = NULL;
	QuoteTable::Properties properties;

	CButton radioBuy(GetDlgItem(IDC_RADIO_BUY));	

	int buy = 0;
	memset(&ta, 0, sizeof(ta));

	BOOL invalidParams = FALSE;

	wnd = GetDlgItem(IDC_COMBO_INSTRUMENT);
	if (0 == wnd.GetWindowText(strName, SIZEOF_ARRAY(strName)))
	{
		theApp.LogMessage(TEXT("Не указано название инструмента стоп-заявки!"), CODE_USER_WARNING); // [STOP_ORDER] 
		wnd.SetFocus();
		invalidParams = TRUE;
		goto end;
	}
	else
		SET_TRANSACTION_STRCODE(ta.strName, (LPCTSTR)strName);
	
	wnd = GetDlgItem(IDC_COMBO_ACCOUNT);
	if (0 == wnd.GetWindowText(str, SIZEOF_ARRAY(str)))
	{
		theApp.LogMessage(TEXT("Не указан номер счёта стоп-заявки!"), CODE_USER_WARNING);
		wnd.SetFocus();
		invalidParams = TRUE;
		goto end;
	}
	else
		SET_TRANSACTION_STRCODE(ta.strAccount, (LPCTSTR)str);

	wnd = GetDlgItem(IDC_EDIT_STOPPRICE);
	if (0 == wnd.GetWindowText(str, SIZEOF_ARRAY(str)))
	{
		theApp.LogMessage(TEXT("Не указана стоп-цена заявки!"), CODE_USER_WARNING);
		wnd.SetFocus();
		invalidParams = TRUE;
		goto end;
	}
	else
		ta.stop.price = _tcstod(str, &pEnd);

	if (pTable == NULL || 
		0 != lstrcmp(ta.strName, pTable->GetProperties()->name) || 
		0 != lstrcmp(ta.strAccount, pTable->GetProperties()->account))
	{
		pTable = theApp.FindTable(&ta);
		if (pTable == NULL)
		{// Поищем по коду инструмента:
			SET_TRANSACTION_STRCODE(ta.strSecCode, ta.strName);
			pTable = theApp.FindTable(NULL, ta.strSecCode, NULL, NULL, ta.strAccount);
		}
	}

	if (pTable)
	{
		pProperties = pTable->GetProperties();
#if 0
		if (m_pTable != pTable)
			m_pTable = pTable; // ?
#endif
	}
	else
		pProperties = theApp.FindTableProperty(&ta);
	if (pProperties == NULL)
	{
		properties.Initialize(ta.strName, ta.strSecCode, NULL, NULL, ta.strAccount);
		pProperties = &properties;
	}

	buy = (BST_CHECKED == radioBuy.GetCheck());

	wnd = GetDlgItem(IDC_EDIT_PRICE);
	if (0 == wnd.GetWindowText(str, SIZEOF_ARRAY(str)))
	{
#if 0
		ta.price = ta.stop.price;
#else
		if (pProperties == NULL)
			ta.price = ta.stop.price;
		else
		{
			double slippage = pProperties->trading.autos.stop.slippage;
			if (! buy)
				slippage =- slippage;
			ta.price = ta.stop.price + slippage;
		}
#endif
	}
	else
		ta.price = _tcstod(str, &pEnd);

	wnd = GetDlgItem(IDC_EDIT_QUANTITY);
	if (wnd.GetWindowText(str, SIZEOF_ARRAY(str)))
		ta.quantity = StrToInt(str);
	else
	{		
		if (pProperties != NULL)
			ta.quantity = pProperties->trading.quantity;
		else
			ta.quantity = 0;
	}	
		
	ta.baseAction = ta.action = Transaction::NEW_STOP_ORDER;
	ta.operation = (buy) ? Transaction::OPERATION_BUY : Transaction::OPERATION_SELL;
	if (ta.quantity == 0)
	{
		theApp.LogMessage(TEXT("Не указано количество лотов в стоп-заявке!"), CODE_USER_WARNING);
		wnd.SetFocus();
		invalidParams = TRUE;
		goto end;
	}
	else
	{
		AddStringToCombo(strName, IDC_COMBO_INSTRUMENT);

		// Срок действия заявки:
		CButton radioDate(GetDlgItem(IDC_RADIO_EXPIRE_DATE));
		CButton radioGtc(GetDlgItem(IDC_RADIO_EXPIRE_GTC));
		ta.expire.type = Transaction::EXPIRE_TODAY;
		if (BST_CHECKED == radioDate.GetCheck())
		{			
			ta.expire.type = Transaction::EXPIRE_DATE;
			CDateTimePickerCtrl dateTimeCtrl(GetDlgItem(IDC_EXPIRE_DATE));
			dateTimeCtrl.GetSystemTime(&ta.expire.date);
		}
		else if (BST_CHECKED == radioGtc.GetCheck())
			ta.expire.type = Transaction::EXPIRE_GTC; // до отмены;

#if 0
		ta.status = Transaction::STATUS_SENT;
#endif
		ta.volume = ta.price * ta.quantity;
#if 1
		m_operation = ta.operation;

		if (pTable)
			KeepUserValues(pTable);
#endif
		theApp.DoTransaction(&ta, pProperties);
	}

end:
	if (invalidParams)
	{
		if (theApp.m_settings.presentation.main.flags & F_VIEW_ENABLE_FLASHING)
			theApp.FlashWindow(*this);
		return;
	}
#if 0
	OnClose();
#endif
}

void CDlgStop2::OnCancel(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnClose();
}

void CDlgStop2::OnClose()
{
	HWND hActiveWindow = m_hActiveWindow;
#if 1
	theApp.SaveWindowPosition(*this, GetName());
#endif
#if DLG_STOP_MODAL
	EndDialog(IDCANCEL);
#else
	DestroyWindow();
#endif
	if (my::wnd::IsValid(hActiveWindow))
		::SetFocus(hActiveWindow);
}

void CDlgStop2::OnDestroy()
{
	my::DialogMessageHook::UninstallHook(*this);
}

int CDlgStop2::AddStringToCombo(LPCTSTR name, int id)
{
	if (lstrlen(name) > 0)
	{
		CComboBox combo(GetDlgItem(id));
		if (CB_ERR == combo.FindStringExact(0, name))
		{
			combo.InsertString(0, name);
		}
	}
	return 0;
}

void CDlgStop2::SetPicture(UINT id)
{
	HINSTANCE hInst = theApp.GetModuleInstance();
	HICON hIcon = CMyQuikApp::LoadIconEx(hInst, id, 32);
	m_picture.SetIcon(hIcon); 
}

void CDlgStop2::ModifyMenuEdit(CMenu & menu, UINT id)
{
#if USE_STATIC_MENU_USER_VALUES
	while (TRUE == menu.DeleteMenu(0, MF_BYPOSITION));
#endif
	const QuoteTable * pTable = theApp.GetCurrentTable();
	if (pTable != NULL)
	{
		BOOL isPrice = TRUE;
		const ListOfUserValues * pList = NULL;
		if (id == IDC_EDIT_PRICE)
			pList = &pTable->user.sps;
		else if (id == IDC_EDIT_STOPPRICE)
			pList = &pTable->user.sps2;
		else if (id == IDC_EDIT_QUANTITY)
		{
			pList = &pTable->user.sqs;
			isPrice = FALSE;
		}
		if (pList != NULL && pList->NotEmpty())
		{
			TSTRING_SMALL(str);
			const QuoteTable::Properties * pProperties = pTable->GetProperties();
			int nd = pProperties->price.nd;
			int i = 0;
			double current = pList->GetCurrentValue();
			int iCurent = -1;
			ListOfUserValues::const_iterator it;
			for (it = pList->begin(); it != pList->end() ; it++, i++)
			{
				double val = *it;
				FormatString(str, SIZEOF_ARRAY(str), val, isPrice ? nd : 0);
				menu.AppendMenu(MF_STRING, IDM_PRICE1 + i, str);
				if (val == current)
					iCurent = i;
			} // for (it)
#if 0
			if (iCurent >= 0)
				menu.CheckMenuRadioItem(0, i - 1, iCurent, MF_BYPOSITION);
#endif
			MENUINFO info;
			info.cbSize = sizeof(info);
			info.fMask = MIM_MENUDATA;
			info.dwMenuData = id;
			menu.SetMenuInfo(&info);
		}
	}
}

QuoteTable * CDlgStop2::GetCurrentTable()
{
	QuoteTable * pTable = m_pTable;

	return pTable;
}

void CDlgStop2::OnBnClickedRadioBuy(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	CButton radioBuy(GetDlgItem(IDC_RADIO_BUY));
	CButton radioGreater(GetDlgItem(IDC_RADIO_GREATER));
	CButton radioSell(GetDlgItem(IDC_RADIO_SELL));
	CButton radioLesser(GetDlgItem(IDC_RADIO_LESSER));
	CButton picture(GetDlgItem(IDC_PICTURE));
	if (BST_CHECKED != radioBuy.GetCheck())
	{
		radioBuy.SetCheck(BST_CHECKED);
		radioGreater.SetCheck(BST_CHECKED);
		radioSell.SetCheck(BST_UNCHECKED);
		radioLesser.SetCheck(BST_UNCHECKED);

		SetPicture(IDI_BUY_BIG);
	}
}

void CDlgStop2::OnBnClickedRadioSell(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	CButton radioBuy(GetDlgItem(IDC_RADIO_BUY));
	CButton radioGreater(GetDlgItem(IDC_RADIO_GREATER));
	CButton radioSell(GetDlgItem(IDC_RADIO_SELL));
	CButton radioLesser(GetDlgItem(IDC_RADIO_LESSER));
	CButton picture(GetDlgItem(IDC_PICTURE));
	if (BST_CHECKED != radioSell.GetCheck())
	{
		radioBuy.SetCheck(BST_UNCHECKED);
		radioGreater.SetCheck(BST_UNCHECKED);
		radioSell.SetCheck(BST_CHECKED);
		radioLesser.SetCheck(BST_CHECKED);

		SetPicture(IDI_SELL_BIG);
	}
}

void CDlgStop2::OnBnClickedRadioExpire(int id)
{
	CButton radioToday(GetDlgItem(IDC_RADIO_EXPIRE_TODAY));
	CButton radioDate(GetDlgItem(IDC_RADIO_EXPIRE_DATE));
	CButton radioGtc(GetDlgItem(IDC_RADIO_EXPIRE_GTC));
	CDateTimePickerCtrl dateTimeCtrl(GetDlgItem(IDC_EXPIRE_DATE));
	switch (id)
	{
	case IDC_RADIO_EXPIRE_TODAY:
		radioToday.SetCheck(BST_CHECKED);
		radioDate.SetCheck(BST_UNCHECKED);
		radioGtc.SetCheck(BST_UNCHECKED);
		dateTimeCtrl.EnableWindow(FALSE);
		break;
	case IDC_RADIO_EXPIRE_DATE:
		radioToday.SetCheck(BST_UNCHECKED);
		radioDate.SetCheck(BST_CHECKED);
		radioGtc.SetCheck(BST_UNCHECKED);
		dateTimeCtrl.EnableWindow();
		break;
	case IDC_RADIO_EXPIRE_GTC:
		radioToday.SetCheck(BST_UNCHECKED);
		radioDate.SetCheck(BST_UNCHECKED);
		radioGtc.SetCheck(BST_CHECKED);
		dateTimeCtrl.EnableWindow(FALSE);
		break;
	}
}

void CDlgStop2::OnBnClickedRadioExpireToday(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnBnClickedRadioExpire(IDC_RADIO_EXPIRE_TODAY);
}

void CDlgStop2::OnBnClickedRadioExpireDate(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnBnClickedRadioExpire(IDC_RADIO_EXPIRE_DATE);
}

void CDlgStop2::OnBnClickedRadioExpireGtc(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnBnClickedRadioExpire(IDC_RADIO_EXPIRE_GTC);
}

LRESULT CDlgStop2::OnDeltaposSpinStopStopprice(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);

	const QuoteTable::Properties * pProperties = m_pTable ? m_pTable->GetProperties() : NULL;
	m_edits.price2.OnDeltaposSpinPrice(pNMUpDown, pProperties);

	return 0;
}

LRESULT CDlgStop2::OnDeltaposSpinStopPrice(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);

	const QuoteTable::Properties * pProperties = m_pTable ? m_pTable->GetProperties() : NULL;
	m_edits.price.OnDeltaposSpinPrice(pNMUpDown, pProperties);

	return 0;
}

LRESULT CDlgStop2::OnDeltaposSpinStopQuantity(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);

	const QuoteTable::Properties * pProperties = m_pTable ? m_pTable->GetProperties() : NULL;
	m_edits.quantity.OnDeltaposSpinQuantity(pNMUpDown, pProperties);

	return 0;
}

void CDlgStop2::OnActivate(UINT nState, BOOL bMinimized, CWindow wndOther)
{
	::SendMessage(GetParent(), UM_ACTIVE, (WPARAM)this->m_hWnd, (LPARAM)nState);
}

void CDlgStop2::OnRButtonUp(UINT nFlags, CPoint point)
{
	UINT flags = TPM_LEFTALIGN|TPM_RIGHTBUTTON;
	CWindow wnd = RealChildWindowFromPoint(*this, point);
	UINT id = wnd.GetDlgCtrlID();
	switch (id)
	{
	case IDC_EDIT_PRICE:
	case IDC_EDIT_STOPPRICE:
	case IDC_EDIT_QUANTITY:
		{
#if 1
			::SetFocus(wnd);
#endif
			CMenu menu;
#if USE_STATIC_MENU_USER_VALUES
			CMenu & root = m_menuUserValues;
			if (! root.IsMenu())
				root.LoadMenu(IDR_MENU_USER_VALUES);
			menu = root.GetSubMenu(0);
#else			
			if (TRUE == menu.CreatePopupMenu())
#endif
			{
				ModifyMenuEdit(menu, id);
				if (menu.GetMenuItemCount() > 0)
				{
					CPoint point;
					GetCursorPos(&point);
					menu.TrackPopupMenu(flags, point.x, point.y, *this);
				}
			}
#if USE_STATIC_MENU_USER_VALUES
			menu.Detach();
#endif
		}
		break;
	} // switch (id)
}

LRESULT CDlgStop2::OnCmdMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = FALSE;

	int nCode = HIWORD(wParam);
	if (nCode)
		return 0; // выход, если сообщение не от меню;

	UINT nID = LOWORD(wParam);
	if (nID >= IDM_PRICE1 && nID <= IDM_PRICE10)
	{
		int i = nID - IDM_PRICE1;
		TSTRING_STD2(str, size);
#if USE_STATIC_MENU_USER_VALUES
		CMenu & root = m_menuUserValues;
		CMenu menu = root.GetSubMenu(0);
		menu.GetMenuString(i, str, size, MF_BYPOSITION);

		CWindow wnd;
#if 0
		wnd = GetFocus();
#else
		MENUINFO info;
		info.cbSize = sizeof(info);
		info.fMask = MIM_MENUDATA;
		menu.GetMenuInfo(&info);
		UINT id = info.dwMenuData;
		wnd = GetDlgItem(id);
#endif
		if (wnd)
		{
			wnd.SetWindowText(str);
			CEdit edit = wnd;
			edit.SetSel(edit.LineLength(), -1);
#if 1
			// Обновляем чтобы выбранное значение стало первым в списке:
			QuoteTable * pTable = GetCurrentTable();
			if (pTable != NULL)
				KeepUserValues(pTable);
#endif
		}

		menu.Detach();
#endif // USE_STATIC_MENU_USER_VALUES
		bHandled = TRUE;
	}

	return 0;
}
