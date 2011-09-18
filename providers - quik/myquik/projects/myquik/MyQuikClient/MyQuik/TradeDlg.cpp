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
 *  TradeDlg.cpp : implementation of the CTradeDlg class
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "TradeDlg.h"
#include "DlgGlass.h"
#include "DlgHistory.h"
#include "DlgStop2.h"
#include "DlgLog.h"
#include "DlgProperties.h"
#include "DlgAbout.h"
#include "PropertySheetSettings.h"

#include "transactions.h"
#include "trans2quik_api.h"

#include "strings.h"
#include "mcapture.h"

#include <ctrl.h>
#include <str.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

CDlgGlass dlgGlass;
CDlgHistory dlgHistory;
CDlgLog dlgLog;
CDlgStop2 dlgStop;

CPropertySheetSettings * dlgSettings = NULL;
CDlgProperties * dlgProperties = NULL;


//
// CTradeDlg dialog
//

CTradeDlg::CTradeDlg()
{
	m_initialized = FALSE;
	m_viewLog = FALSE;

	m_rect.left = m_rect.right = m_rect.top = m_rect.bottom = 0;

	m_mode = MODE_NORMAL;
	m_info = INFO_DISABLED;
	m_selectInstrument = 0;

	m_pToolTipCtrl = NULL;	

	m_osdCtrl.active = 0x0;

	m_pHistory = NULL;

	m_activePropertyPage = 0;

	m_hidden = FALSE;

	m_freeze = FALSE;

	m_hActiveWnd = NULL;

	m_loading = FALSE;
}

CTradeDlg::~CTradeDlg()
{
	SAFE_DELETE(m_pHistory);
	SAFE_DELETE(m_pToolTipCtrl);
}

void CTradeDlg::UpdateCaption()
{
	const QuoteTable::Properties * pProperties = theApp.GetCurrentTableProperty();
	UpdateCaption(pProperties);
}

void CTradeDlg::UpdateCaption(const QuoteTable::Properties * pProperties)
{
	TSTRING_STD2(text, size);
	LPCTSTR account = NULL;
#if 0
	int n = _stprintf_s(text, size, TEXT("%s - "), theApp.GetAppName());
#else
	int n = 0;
#endif
	if (pProperties != NULL)
	{
		n += _stprintf_s(text + n, size - n, TEXT("%s"), pProperties->name);
#if 0
		if (pProperties->classcode[0] != '\0')
			n += _stprintf_s(text + n, size - n, TEXT(" [%s"), pProperties->seccode);
		if (pProperties->classcode[0] != '\0')
			n += _stprintf_s(text + n, size - n, TEXT(" / %s"), pProperties->classcode);
#if 0
		if (pProperties->clientcode[0] != '\0')
			n += _stprintf_s(text + n, size - n, TEXT(" / %s"), pProperties->clientcode);
#endif	
		n += _stprintf_s(text + n, size - n, TEXT("]"));
#endif
		n += _stprintf_s(text + n, size - n, TEXT(" - "));
		if (pProperties->account[0] != '\0')
		{
#if 0
			n += _stprintf_s(text + n, size - n, TEXT("%s"), pProperties->account);
			n += _stprintf_s(text + n, size - n, TEXT(" - "));
#else
			account = pProperties->account;
#endif
		}
#if 1
		n += _stprintf_s(text + n, size - n, TEXT("сделки"));
#endif
	}
#if 0
	n += _stprintf_s(text + n, size - n, TEXT("сделки по инструменту"));
#endif
#if 0
	n += _stprintf_s(text + n, size - n, TEXT(" - %s"), theApp.GetAppName());
#endif
#if 0
	if (account != NULL)
		n += _stprintf_s(text + n, size - n, TEXT(" - %s"), account);
#endif
#if 1
	n += _stprintf_s(text + n, size - n, TEXT("%s%s"), n ? TEXT(" - ") : TEXT(""), theApp.GetAppName());
#endif
#if 1
	n += _stprintf_s(text + n, size - n, TEXT("%s"), 
		(theApp.m_settings.common.quik.autoCheckConnection && !m_quik.connected) ? TEXT(" - автономный режим") : TEXT(""));	
#endif
	SetWindowText(text);
}

void CTradeDlg::UpdatePropertiesEx(const QuoteTable::Properties * pProperties)
{
	UpdateCaption(pProperties);
	if (pProperties != NULL)
	{
		CString str;
		CWindow wnd;
		wnd = GetDlgItem(IDC_EDIT_TRADE_QUANTITY);
		str.Format(TEXT("%.0f"), pProperties->trading.quantity);
		wnd.SetWindowText(str);

		EnableItems();
	}
}

LPCTSTR CTradeDlg::ColumnIndexToName(int index)
{
	return m_listCtrl.ColumnIndexToName(index);
}

int CTradeDlg::GetColumnWidth(int index, LPCTSTR name)
{
	return m_listCtrl.GetColumnWidth(index, name);
}

void CTradeDlg::InitListCtrl()
{
	m_listCtrl.SetParentName(this->GetName());

	int width;
	int format;

	int iCol = 0;
	for (int i = 0; i < NR_COLS_MAX; i++)
	{
		const Settings::Presentation::ListOfDeals::Column & column = theApp.m_settings.presentation.list.columns.item[i];
		if (column.index < 0)
			break;
		else if (! column.visible)
			continue;

		LPCTSTR name = ColumnIndexToName(column.index);
		if (name != NULL)
		{
			width = GetColumnWidth(column.index, name);
			format = column.format;
			m_listCtrl.InsertColumn(iCol, name, format, width);

			CHeaderCtrl header = m_listCtrl.GetHeader();
			HDITEM hdi;
			hdi.mask = HDI_LPARAM;
			hdi.lParam = static_cast<LPARAM>(column.index);
			header.SetItem(iCol, &hdi);
			++iCol;
		}
	} // for (i)

	m_listCtrl.SetExtendedListViewStyle(m_listCtrl.GetExtendedListViewStyle()|
		LVS_EX_FULLROWSELECT|0|LVS_EX_DOUBLEBUFFER|LVS_EX_BORDERSELECT|
		0  ); // LVS_EX_BORDERSELECT LVS_EX_GRIDLINES LVS_EX_FULLROWSELECT

	m_listCtrl.SetImageList(theApp.GetIconList(), LVSIL_SMALL);
}

void CTradeDlg::SaveListCtrlParams()
{
	m_listCtrl.SaveListCtrlParams();
}

void CTradeDlg::EnableItems(BOOL enable)
{
	CWindow wnd;
	wnd = GetDlgItem(IDC_LIST1); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_EDIT_TRADE_PRICE1); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_STATIC_PRICE); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_EDIT_TRADE_QUANTITY); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_STATIC_QUANTITY); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_BUTTON_BUY); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_BUTTON_SELL); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_BUTTON_CANCEL); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_BUTTON_CANCEL_ALL); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_BUTTON_STOP); wnd.EnableWindow(enable);
}

void CTradeDlg::ModifyMenuEdit(CMenu & menu, UINT id)
{
#if USE_STATIC_MENU_USER_VALUES
	while (TRUE == menu.DeleteMenu(0, MF_BYPOSITION));
#endif
	const QuoteTable * pTable = theApp.GetCurrentTable();
	if (pTable != NULL)
	{
		BOOL isPrice = FALSE;
		const ListOfUserValues * pList = NULL;
		if (id == IDC_EDIT_TRADE_PRICE1)
		{
			pList = &pTable->user.ps;
			isPrice = TRUE;
		}
		else if (id == IDC_EDIT_TRADE_QUANTITY)
			pList = &pTable->user.qs;
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

void CTradeDlg::ModifyMenuButton(CMenu & menu, UINT id)
{
	BOOL enableBuy, enableSell;
	double price, priceCurrent, priceBid, priceOffer, priceTa;

	const QuoteTable * pTable = theApp.GetCurrentTable();

	priceCurrent = priceBid = priceOffer = 0;
	price = this->GetPriceUser();
	priceTa = this->GetPriceTa();

	if (pTable)
	{
		priceCurrent = pTable->price;
		pTable->GetPricesBidOffer(priceBid, priceOffer);
	}

	Mode mode = GetMode();

	CheckEnableBuySell(enableBuy, enableSell);

	if (id == IDC_BUTTON_BUY && enableBuy)
	{
		if (price)
			menu.AppendMenu(MF_STRING, ID_BUY_FIXED, STRING_BUY_FIXED);
		if (priceTa)
			menu.AppendMenu(MF_STRING, ID_BUY_TA, STRING_BUY_TA);
		if (priceBid)
			menu.AppendMenu(MF_STRING, ID_BUY_MARKET, STRING_BUY_MARKET);
		if (priceCurrent)
			menu.AppendMenu(MF_STRING, ID_BUY_CURRENT, STRING_BUY_CURRENT);
	}
	else if (id == IDC_BUTTON_SELL && enableSell)
	{
		if (price)
			menu.AppendMenu(MF_STRING, ID_SELL_FIXED, STRING_SELL_FIXED);
		if (priceTa)
			menu.AppendMenu(MF_STRING, ID_SELL_TA, STRING_SELL_TA);
		if (priceOffer)
			menu.AppendMenu(MF_STRING, ID_SELL_MARKET, STRING_SELL_MARKET);
		if (priceCurrent)
			menu.AppendMenu(MF_STRING, ID_SELL_CURRENT, STRING_SELL_CURRENT);
	}

	if (id == IDC_BUTTON_CANCEL || id == IDC_BUTTON_CANCEL_ALL)
	{
		BOOL enableCancel, enableCancelAll, enableCancelBuy, enableCancelSell, enableCancelStop;
		if (CheckEnableCancel(enableCancel, enableCancelAll, enableCancelBuy, enableCancelSell, enableCancelStop))
		{
			if (enableCancel)
				if (id == IDC_BUTTON_CANCEL || mode == MODE_ALTERNATIVE)
					menu.AppendMenu(MF_STRING, ID_CANCEL_ORDER, STRING_CANCEL_BID);
			if (enableCancelAll)
				if (id == IDC_BUTTON_CANCEL_ALL || mode == MODE_ALTERNATIVE)
					menu.AppendMenu(MF_STRING, ID_CANCEL_ALL_ORDERS, STRING_CANCEL_ALL);
			if (enableCancelBuy)
				if ((id == IDC_BUTTON_CANCEL && mode == MODE_ALTERNATIVE) || (id == IDC_BUTTON_CANCEL_ALL && mode == MODE_NORMAL))
					menu.AppendMenu(MF_STRING, ID_CANCEL_ALL_ORDERS_BUY, STRING_CANCEL_ALL_BUY);
			if (enableCancelSell)
				if (id == IDC_BUTTON_CANCEL_ALL)
					menu.AppendMenu(MF_STRING, ID_CANCEL_ALL_ORDERS_SELL, STRING_CANCEL_ALL_SELL);				
			if (enableCancelStop)
				if (id == IDC_BUTTON_CANCEL_ALL || (id == IDC_BUTTON_CANCEL && mode == MODE_ALTERNATIVE))
					menu.AppendMenu(MF_STRING, ID_CANCEL_ALL_ORDERS_STOP, STRING_CANCEL_ALL_STOPS);			
		}
	}

	if (id == IDC_BUTTON_ZERO || id == IDC_BUTTON_REVERSE)
	{
		BOOL enableTradeZero, enableTradeReverse;
		if (CheckEnableTrade(enableTradeZero, enableTradeReverse))
		{
			if (id == IDC_BUTTON_ZERO && enableTradeZero)
				menu.AppendMenu(MF_STRING, ID_TRADE_ZERO, STRING_TRADE_ZERO);
			else if (id == IDC_BUTTON_REVERSE && enableTradeReverse)
				menu.AppendMenu(MF_STRING, ID_TRADE_REVERSE, STRING_TRADE_REVERSE);
		}
	}

	if (id == IDC_BUTTON_STOP)
	{
		BOOL enableStop;
		CheckEnableStopOrder(enableStop);
		if (enableStop)
		{
			if (menu.GetMenuItemCount())
				menu.AppendMenu(MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));
			menu.AppendMenu(MF_STRING, ID_STOP_ORDER, STRING_STOP_ORDER_);
			if (price)
			{
				menu.AppendMenu(MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));
				menu.AppendMenu(MF_STRING, ID_STOP_BID_BUY, STRING_STOP_BID_BUY);
				menu.AppendMenu(MF_STRING, ID_STOP_BID_SELL, STRING_STOP_BID_SELL);
				menu.AppendMenu(MF_STRING, ID_STOP_BID_BUY_OFFSET, STRING_STOP_BID_BUY_OFFSET);
				menu.AppendMenu(MF_STRING, ID_STOP_BID_SELL_OFFSET, STRING_STOP_BID_SELL_OFFSET);
			}
			if (priceTa)
			{
				menu.AppendMenu(MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));
				menu.AppendMenu(MF_STRING, ID_STOP_BID_BUY_TA, STRING_STOP_BID_BUY_TA);
				menu.AppendMenu(MF_STRING, ID_STOP_BID_SELL_TA, STRING_STOP_BID_SELL_TA);
				menu.AppendMenu(MF_STRING, ID_STOP_BID_BUY_OFFSET_TA, STRING_STOP_BID_BUY_OFFSET_TA);
				menu.AppendMenu(MF_STRING, ID_STOP_BID_SELL_OFFSET_TA, STRING_STOP_BID_SELL_OFFSET_TA);
			}
			const Transaction * pTa = GetSelectedTa();
			if (pTa)
			{
				menu.AppendMenu(MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));
				menu.AppendMenu(MF_STRING, ID_STOP_BID_TA, STRING_STOP_BID_TA);
			}
		}
	}

	ModifyMenuItemsText(menu);
}

void CTradeDlg::ModifyMenuTrading(CMenu & menu, int flags)
{
	BOOL enableBuy, enableSell;
	double price, priceCurrent, priceBid, priceOffer, priceTa;

	CWindow activeWnd = GetActiveWindow();
	BOOL glassIsActive = FALSE;

	const QuoteTable * pTable = theApp.GetCurrentTable();
	const QuoteTable::Properties * pProperties = pTable ? pTable->GetProperties() : NULL;

	price = priceCurrent = priceBid = priceOffer = priceTa = 0;

	// Если стакан активен, то фиксированную цену берём из него:
	if (my::wnd::IsVisible(dlgGlass) && (dlgGlass == activeWnd))
	{		
		if (pTable != NULL)
			price = pTable->priceUser;
		glassIsActive = TRUE;
	}
	else
	{
		price = this->GetPriceUser();
		priceTa = this->GetPriceTa();
	}

	if (CheckEnableBuySell(enableBuy, enableSell))
	{			
		if (enableBuy || enableSell)
		{
			if (pTable)
			{
				priceCurrent = pTable->price;
				if (theApp.m_settings.trading.bestIsCurrent)
					priceBid = priceOffer = priceCurrent;
				else
					pTable->GetPricesBidOffer(priceBid, priceOffer);
			}
			if ((flags & OPERATION_FIXED) && price)
			{
#if 0
				if (pProperties)
				{
					TSTRING_SMALL2(str, size);
					TSTRING_SMALL(strPrice);					
					int nd = pProperties->price.nd;
					FormatString(strPrice, SIZEOF_ARRAY(strPrice), price, nd);
					_stprintf_s(str, size, TEXT("%s (%s)"), STRING_BUY_FIXED, strPrice);
					menu.AppendMenu(MF_STRING, ID_BUY_FIXED, str);
					_stprintf_s(str, size, TEXT("%s (%s)"), STRING_SELL_FIXED, strPrice);
					menu.AppendMenu(MF_STRING, ID_SELL_FIXED, str);
				}
				else
#endif
				{
					menu.AppendMenu(MF_STRING, ID_BUY_FIXED, STRING_BUY_FIXED);
					menu.AppendMenu(MF_STRING, ID_SELL_FIXED, STRING_SELL_FIXED);
				}
			}
#if 0
			else
			{
				menu.AppendMenu(MF_STRING, ID_BUY, STRING_BUY);
				menu.AppendMenu(MF_STRING, ID_SELL, STRING_SELL);
			}
#endif
			if ((flags & OPERATION_TA) && priceTa)
			{
				//menu.AppendMenu(MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));
				menu.AppendMenu(MF_STRING, ID_BUY_TA, STRING_BUY_TA);
				menu.AppendMenu(MF_STRING, ID_SELL_TA, STRING_SELL_TA);
			}
			if (priceBid || priceOffer)
			{
				menu.AppendMenu(MF_STRING, ID_BUY_MARKET, STRING_BUY_MARKET);
				menu.AppendMenu(MF_STRING, ID_SELL_MARKET, STRING_SELL_MARKET);
			}
			if (priceCurrent)
			{
				menu.AppendMenu(MF_STRING, ID_BUY_CURRENT, STRING_BUY_CURRENT);
				menu.AppendMenu(MF_STRING, ID_SELL_CURRENT, STRING_SELL_CURRENT);
			}
		}
	}
#if USE_CHANGE_ACTIVE_BID
	if (! glassIsActive)
	{
		int iItem = my::ctrl::GetFocusedItem(m_listCtrl);
		Transaction * pTransaction = (Transaction*)m_listCtrl.GetItemData(iItem);
		if (pTransaction != NULL && Transaction::IsActive(pTransaction->status))
		{
			menu.AppendMenu(MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));
			menu.AppendMenu(MF_STRING, ID_CHANGE_ACTIVE_BID, 
				(Transaction::IsActionStop(pTransaction->baseAction)) ? STRING_CHANGE_STOP_BID : STRING_CHANGE_BID);
		}
	}
#endif // USE_CHANGE_ACTIVE_BID
	BOOL enableCancel, enableCancelAll, enableCancelBuy, enableCancelSell, enableCancelStop;
	BOOL enableCancelGlass;
	if (CheckEnableCancel(enableCancel, enableCancelAll, enableCancelBuy, enableCancelSell, enableCancelStop))
	{
		if (menu.GetMenuItemCount())
			menu.AppendMenu(MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));
#if 1
		enableCancelGlass = FALSE;
		if (glassIsActive && dlgGlass.IsMarkerVisible())
		{		
			enableCancel = FALSE;
			const CDlgGlass::Active::Item & active = dlgGlass.GetActiveItem(CDlgGlass::I_ACTIVE_MARKER);
			if (active.flags & CDlgGlass::F_ACTIVE_ITEM_IS_VALID)
			{
				int ii[] = {
					QuoteTable::ITEM_USER_BID_BUY, QuoteTable::ITEM_USER_BID_SELL, 
					QuoteTable::ITEM_USER_STOP_BUY, QuoteTable::ITEM_USER_STOP_SELL, 
				};
				for (int i = 0; i < SIZEOF_ARRAY(ii); i++)
				{
					if (active.item.values[ii[i]])
					{
						enableCancelGlass = TRUE;
						break;
					}
				} // for (i)					
			}
		}
		if (enableCancelGlass)
			menu.AppendMenu(MF_STRING, ID_CANCEL_ORDER_GLASS, STRING_CANCEL_BID);
#endif
		if (enableCancel)
			menu.AppendMenu(MF_STRING, ID_CANCEL_ORDER, STRING_CANCEL_BID);
		if (enableCancelAll)
			menu.AppendMenu(MF_STRING, ID_CANCEL_ALL_ORDERS, STRING_CANCEL_ALL);
		if (enableCancelBuy)
			menu.AppendMenu(MF_STRING, ID_CANCEL_ALL_ORDERS_BUY, STRING_CANCEL_ALL_BUY);
		if (enableCancelSell)
			menu.AppendMenu(MF_STRING, ID_CANCEL_ALL_ORDERS_SELL, STRING_CANCEL_ALL_SELL);				
		if (enableCancelStop)
			menu.AppendMenu(MF_STRING, ID_CANCEL_ALL_ORDERS_STOP, STRING_CANCEL_ALL_STOPS);				
	}
	BOOL enableTradeZero, enableTradeReverse;
	if (CheckEnableTrade(enableTradeZero, enableTradeReverse))
	{
		if (menu.GetMenuItemCount())
			menu.AppendMenu(MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));
		if (enableTradeZero)
			menu.AppendMenu(MF_STRING, ID_TRADE_ZERO, STRING_TRADE_ZERO);
		if (enableTradeReverse)
			menu.AppendMenu(MF_STRING, ID_TRADE_REVERSE, STRING_TRADE_REVERSE);
	}
	
	BOOL enableStop;
	CheckEnableStopOrder(enableStop);
	if (enableStop)
	{
		if (menu.GetMenuItemCount())
			menu.AppendMenu(MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));
		menu.AppendMenu(MF_STRING, ID_STOP_ORDER, STRING_STOP_ORDER_);
		if ((flags & OPERATION_FIXED) && price)
		{
			menu.AppendMenu(MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));
			menu.AppendMenu(MF_STRING, ID_STOP_BID_BUY, STRING_STOP_BID_BUY); //  TEXT(" по указанной цене")
			menu.AppendMenu(MF_STRING, ID_STOP_BID_SELL, STRING_STOP_BID_SELL); //  TEXT(" по указанной цене")
			menu.AppendMenu(MF_STRING, ID_STOP_BID_BUY_OFFSET, STRING_STOP_BID_BUY_OFFSET);
			menu.AppendMenu(MF_STRING, ID_STOP_BID_SELL_OFFSET, STRING_STOP_BID_SELL_OFFSET);
		}
		if ((flags & OPERATION_TA) && priceTa)
		{
			menu.AppendMenu(MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));
			menu.AppendMenu(MF_STRING, ID_STOP_BID_BUY_TA, STRING_STOP_BID_BUY_TA);
			menu.AppendMenu(MF_STRING, ID_STOP_BID_SELL_TA, STRING_STOP_BID_SELL_TA);
			menu.AppendMenu(MF_STRING, ID_STOP_BID_BUY_OFFSET_TA, STRING_STOP_BID_BUY_OFFSET_TA);
			menu.AppendMenu(MF_STRING, ID_STOP_BID_SELL_OFFSET_TA, STRING_STOP_BID_SELL_OFFSET_TA);
		}
		const Transaction * pTa = GetSelectedTa();
		if (!glassIsActive && pTa) // flags & OPERATION_TA)
		{
			menu.AppendMenu(MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));
			menu.AppendMenu(MF_STRING, ID_STOP_BID_TA, STRING_STOP_BID_TA);
		}
	}

	ModifyMenuItemsText(menu);
}

void CTradeDlg::ModifyMenuInstruments(CMenu & menu)
{
	CMenu * pMenu = &menu;
	UINT flags = MF_BYPOSITION;
	const QuoteTable * pTable;
	LPCTSTR name;
	int nrInstruments;
	UINT enabled;
	int checked;
	int i;

	while (TRUE == menu.DeleteMenu(0, flags));

	nrInstruments = 0;
	Tables::Iterator it;
	pTable = theApp.GetFirstTable(it);
	for (i = 0; pTable; i++)
	{
		pTable = (*it);
		UINT pos = i;
		UINT id = ID_INSTRUMENT_XXX + pos;		
		name = pTable->GetName();

		LPCTSTR seccode = NULL;
		const QuoteTable::Properties * pProperties = pTable->GetProperties();
		if (pProperties)
			seccode = pProperties->seccode;
		CString str;
		if (seccode)
			str.Format(TEXT("%s (%s)"), name, seccode);
		else
			str.Format(TEXT("%s"), name);
		name = str;

		menu.InsertMenu(pos, flags, id, name);
		pTable = theApp.GetNextTable(it, 0);
	} // for (i)

	nrInstruments = i;
	if (nrInstruments > 0)
	{
		menu.InsertMenu(nrInstruments + 0, flags|MF_SEPARATOR, ID_PROPERTIES, TEXT(""));
		menu.InsertMenu(nrInstruments + 1, flags|MF_DISABLED|MF_GRAYED, ID_PROPERTIES, STRING_PROPERTIES);				
#if 1
		if (nrInstruments >= 2)
		{
			menu.InsertMenu(nrInstruments + 0, flags|MF_SEPARATOR, ID_PROPERTIES, TEXT(""));
			menu.InsertMenu(nrInstruments + 1, flags, ID_INSTRUMENT_NEXT, TEXT("Следующий"));
			menu.InsertMenu(nrInstruments + 2, flags, ID_INSTRUMENT_PREVIOS, TEXT("Предыдущий"));
		}
#endif
	} // if (nrInstruments > 0)

	checked = -1;
	pTable = theApp.GetCurrentTable();	
	if (pTable != NULL)
	{		
		enabled = MF_ENABLED;
		LPCTSTR name = pTable->GetName();
		for (int i = 0; i < nrInstruments; i++)
		{
			TSTRING_STD2(str, size);
			pMenu->GetMenuString(i, str, size, flags);

			ExtractInstrumentName(str, size);

			if (0 == lstrcmp(name, str))
			{
				flags |= enabled;
				checked = i;
				break;
			}
		} // for (i)
	}
	else
	{
		enabled = MF_DISABLED|MF_GRAYED;
		flags |= enabled;
	}
	// Помечаем выбранный инструмент:
	if (checked >= 0)
		pMenu->CheckMenuRadioItem(0, nrInstruments, checked, MF_BYPOSITION);

	pMenu->EnableMenuItem(ID_INSTRUMENT_NEXT, MF_BYCOMMAND|enabled);
	pMenu->EnableMenuItem(ID_INSTRUMENT_PREVIOS, MF_BYCOMMAND|enabled);

	UINT count = pMenu->GetMenuItemCount();
	pMenu->EnableMenuItem(count - 1, flags);

	ModifyMenuItemsText(menu);
}

void CTradeDlg::ModifyMenuShow(CMenu & menu)
{
	UINT flags = MF_BYCOMMAND;	

	menu.CheckMenuItem(ID_TRANSACTION_INFO, ((m_info == INFO_TRANSACTION) ? MF_CHECKED : MF_UNCHECKED)|flags);

	menu.CheckMenuItem(ID_SHOW_MAIN, (my::wnd::IsVisible(*this) ? MF_CHECKED : MF_UNCHECKED)|flags);
	menu.CheckMenuItem(ID_SHOW_GLASS, (my::wnd::IsVisible(dlgGlass) ? MF_CHECKED : MF_UNCHECKED)|flags);
	menu.CheckMenuItem(ID_SHOW_HISTORY, (my::wnd::IsVisible(dlgHistory) ? MF_CHECKED : MF_UNCHECKED)|flags);
	menu.CheckMenuItem(ID_SHOW_MSGLOG, (my::wnd::IsVisible(dlgLog) ? MF_CHECKED : MF_UNCHECKED)|flags);

	menu.CheckMenuItem(ID_SHOW_TOPMOST, ((theApp.GetSettings().presentation.main.flags & F_VIEW_ALLWAYS_ON_TOP) ? MF_CHECKED : MF_UNCHECKED)|flags);
#if 0
	ModifyMenuViewDeals(...);
#endif
	ModifyMenuItemsText(menu);
}

void CTradeDlg::ModifyMenuViewDeals(CMenu & menu)
{
	UINT flags = MF_BYCOMMAND;	

	const Settings::Presentation::Main & presentation = theApp.GetSettings().presentation.main;
	menu.CheckMenuItem(ID_SHOW_LISTOFDEALS, ((presentation.show.wnd & F_MAIN_SHOW_LIST) ? MF_CHECKED : MF_UNCHECKED)|flags);
	menu.CheckMenuItem(ID_SHOW_OSD, ((presentation.show.wnd & F_MAIN_SHOW_OSD) ? MF_CHECKED : MF_UNCHECKED)|flags);
	menu.CheckMenuItem(ID_SHOW_CONTROLS, ((presentation.show.wnd & F_MAIN_SHOW_CONTROLS) ? MF_CHECKED : MF_UNCHECKED)|flags);

	int pos1 = ID_SET_BASE;
	int pos2 = ID_SET_EXTENDED;
	int pos = (m_mode == MODE_NORMAL) ? pos1 : pos2;
	menu.CheckMenuRadioItem(pos1, pos2, pos, flags);

	int enable = (presentation.show.wnd & F_MAIN_SHOW_CONTROLS) ? MF_ENABLED : MF_DISABLED|MF_GRAYED;
	menu.EnableMenuItem(pos1, enable|flags);
	menu.EnableMenuItem(pos2, enable|flags);

	ModifyMenuItemsText(menu);
}

void CTradeDlg::ModifyMenuShow2(CMenu & menu, CMenu & menuViewDeals)
{
	UINT flags = MF_BYCOMMAND;
	BOOL main = (*this == GetActiveWindow());

	// Добавляем элементы:
	menu.AppendMenu(MF_STRING, ID_TRANSACTION_INFO, (STRING_TRANSACTION_INFO));
	menu.AppendMenu(MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));
		
	menu.AppendMenu(MF_STRING, ID_SHOW_MAIN, STRING_WND_DEALS);
	if (main)
		menu.AppendMenu(MF_STRING, ID_SHOW_GLASS, STRING_WND_GLASS);
	menu.AppendMenu(MF_STRING, ID_SHOW_HISTORY, STRING_WND_HISTORY);
	menu.AppendMenu(MF_STRING, ID_SHOW_MSGLOG, STRING_WND_MSGLOG);
	menu.AppendMenu(MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));

	if (main)
	{
		ModifyMenuViewDeals(menuViewDeals);
		menu.InsertMenu(menu.GetMenuItemCount(), MF_POPUP|MF_BYPOSITION|MF_STRING, 
			(UINT_PTR)menuViewDeals.m_hMenu, STRING_DEALS_VIEW);		
		menu.AppendMenu(MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));
	}

	menu.AppendMenu(MF_STRING, ID_SHOW_TOPMOST, STRING_TOPMOST);
	menu.AppendMenu(MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));

	menu.AppendMenu(MF_STRING, ID_ABOUT, STRING_ABOUT);

	ModifyMenuShow(menu);
}

void CTradeDlg::RemoveMenu(CMenu & menu, int pos, int flags)
{
	CMenu subMenu = menu.GetSubMenu(pos);
	if (subMenu)
		menu.RemoveMenu(pos, flags);
	subMenu.Detach();
}

void CTradeDlg::ShowMenuTrading()
{
	CMenu menu;
	menu.CreatePopupMenu();

	ModifyMenuTrading(menu, OPERATION_TA);	

	CPoint point;
	GetCursorPos(&point);
	menu.TrackPopupMenu(TPM_LEFTALIGN |TPM_RIGHTBUTTON, point.x, point.y, *this);
}

int CTradeDlg::MakeMainMenu(CMenu & menu, int get)
{
	int status = S_OK;

	CMenu * pMenu = NULL;
#if USE_STATIC_MENU_MAIN
	CMenu & root = m_menu;
	menu = root.GetSubMenu(0);
	pMenu = &menu;
#else
	CMenu root;
	if (TRUE == root.LoadMenu(IDR_MENU_MAIN))
		menu = root.GetSubMenu(0);
#endif
	if (pMenu && pMenu->m_hMenu)
	{
		int flags = MF_BYPOSITION;
		int iMenuItem = MMENU_TRADING;
		CMenu subMenu;
		if (get & F_GET_MENU_TRADE)
		{
			subMenu.Attach (pMenu->GetSubMenu(iMenuItem));
			// Удаляем все элементы:
			while (TRUE == subMenu.DeleteMenu(0, flags));
			// Изменяем меню в соответсвии с текущей ситуацией:
			ModifyMenuTrading(subMenu);
			pMenu->EnableMenuItem(iMenuItem, ((subMenu.GetMenuItemCount()) ? MF_ENABLED : MF_DISABLED|MF_GRAYED)|flags);
			subMenu.Detach();
		}
		if (get & F_GET_MENU_INSTRUMENTS)
		{
			iMenuItem = MMENU_INSTRUMENTS;
			subMenu.Attach (pMenu->GetSubMenu(iMenuItem));
			pMenu->EnableMenuItem(iMenuItem, ((theApp.GetNrInstruments()) ? MF_ENABLED : MF_DISABLED|MF_GRAYED)|flags);
			ModifyMenuInstruments(subMenu);
			subMenu.Detach();
		}
		if (get & F_GET_MENU_SHOW)
		{
			iMenuItem = MMENU_SHOW;
			subMenu.Attach (pMenu->GetSubMenu(iMenuItem));

			RemoveMenu(subMenu, subMenu.GetMenuItemCount() - 5, MF_BYPOSITION);
			while (TRUE == subMenu.DeleteMenu(0, flags));

			CMenu menuViewDeals;
			menuViewDeals.Attach (root.GetSubMenu(1));
			ModifyMenuShow2(subMenu, menuViewDeals);
			menuViewDeals.Detach();
			subMenu.Detach();
		}
#if !defined _DEBUG
		TSTRING_STD2(str, size);
		pMenu->GetMenuString(pMenu->GetMenuItemCount() - 1, str, size, flags);
		if (0 == lstrcmp(str, TEXT("DEBUG")))
		{
			pMenu->DeleteMenu(pMenu->GetMenuItemCount() - 1, flags);
			pMenu->DeleteMenu(pMenu->GetMenuItemCount() - 1, flags);
		}
#endif // #if !defined _DEBUG	
		ModifyMenuItemsText(menu);
	}
#if USE_STATIC_MENU_MAIN
#else
	pMenu = NULL;
#endif
	return status;
}

int CTradeDlg::UpdateColumn(int iCol)
{
	return m_listCtrl.UpdateColumn(iCol);
}

int CTradeDlg::UpdateColumnItem(int iCol, int iItem, const Transaction * pTransaction, const QuoteTable::Properties * pProperties)
{
	return m_listCtrl.UpdateColumnItem(iCol, iItem, pTransaction, pProperties);
}

int CTradeDlg::UpdateColumns(int iItem, const Transaction * pTransaction, const QuoteTable::Properties * pProperties)
{
	return m_listCtrl.UpdateColumns(iItem, pTransaction, pProperties);
}

void CTradeDlg::UpdateColumns(const Settings::Presentation::ListOfDeals & settings, const Settings::Presentation::ListOfDeals & prev)
{
	m_listCtrl.UpdateColumns(settings, prev);
}

int CTradeDlg::UpdateColumnIndex()
{
	return m_listCtrl.UpdateColumnIndex();
}

void CTradeDlg::UpdatePresentation(const Settings::Presentation & presentation, int show, int resize)
{
	this->SetWindowPos((presentation.main.flags & F_VIEW_ALLWAYS_ON_TOP) ? HWND_TOPMOST : HWND_NOTOPMOST, 
		0, 0, 0, 0, SWP_NOMOVE|SWP_NOSIZE|SWP_NOACTIVATE|SWP_NOREDRAW);
	
	const Settings::Presentation::Window & wdeals = presentation.view.item[I_WINDOW_DEALS];
	if (wdeals.flags & F_SHOW_WINDOW)
	{
		SetHidden(FALSE);
		my::wnd::SetDialogTitleBar(*this, wdeals.flags & F_SHOW_TITLE);
#if 1
		if (my::wnd::IsValid(dlgGlass))
			dlgGlass.ModifyStyle(WS_MINIMIZEBOX|WS_MAXIMIZEBOX, 0);
#endif
		if (show)
		{
			this->ShowWindow(SW_SHOW);
		}		
	}
	else
	{// Скрываем главное окно и делаем главным Стакан заявок:
		theApp.SaveWindowPosition(*this, GetName());
		this->ShowWindow(SW_HIDE);
		SetHidden(TRUE);
		ShowGlass();
#if 0
		if (my::wnd::IsValid(dlgGlass))
			dlgGlass.ModifyStyle(0, WS_MINIMIZEBOX); // |WS_MAXIMIZEBOX
#endif
	}

	if (my::wnd::IsValid(dlgGlass))
	{
		dlgGlass.SetQuantity(IsHidden() ? 0 : GetQuantity());
		dlgGlass.UpdateName();
	}

	if (resize)
	{
		if (show)
		{// Сохраняем положение окна
			CREATESTRUCT cs;
			LPCTSTR name = GetName();
			if (S_OK != theApp.LoadWindowPosition(&cs, name))
				theApp.SaveWindowPosition(*this, name, my::lib::FWP_GENERIC|my::lib::FWP_FORCE);
		}
		Resize(0, 0, TRUE);
	}
}

void CTradeDlg::UpdateTransparence(const Settings::Presentation & presentation)
{
#if USE_TRANSPARENCE
	HWND hh[I_WINDOW_LAST];
	hh[0] = this->m_hWnd;
	hh[1] = dlgGlass;
	hh[2] = dlgHistory;
	hh[3] = dlgLog;
	hh[4] = dlgStop;
	int count = SIZEOF_ARRAY(hh);
	for (int i = 0; i < count; i++)
	{
		HWND hWnd = hh[i];
		if (my::wnd::IsValid(hWnd))
		{
			const Settings::Presentation::Window & window = presentation.view.item[i];
			int alpha = 255 * window.opacity.inactive / 100;
			my::wnd::SetOpacity(hWnd, alpha);
		}
	} // for (i)
#endif // #if USE_TRANSPARENCE
}

int CTradeDlg::GetIconIndex(int action, int status, int operation)
{
	return m_listCtrl.GetIconIndex(action, status, operation);
}

void CTradeDlg::SetItemIcon(int iItem, int action, int status, int operation)
{
	m_listCtrl.SetItemIcon(iItem, action, status, operation);
}

int CTradeDlg::AddTransaction(const Transaction * pTransaction, const QuoteTable::Properties * pProperties)
{
	m_listCtrl.AddTransaction(pTransaction, pProperties);
#if 1
	AddToHistory(pTransaction);
#endif
	return 0;
}

int CTradeDlg::UpdateTransaction(const Transaction * pTransaction, const QuoteTable::Properties * pProperties)
{	
	TRACE("UpdateTransaction N %.0f ... ", pTransaction->orderKey);
	int status = m_listCtrl.UpdateTransaction(pTransaction, pProperties);
	if (status == S_OK)
		TRACE("Ok\r\n");
	else
		TRACE("Failed\r\n");
#if 0
	theApp.AddToHistory(pTransaction);
#else
	UpdateHistory(pTransaction);
#endif
	UpdateStateButtons(); // ?
	return status;
}

int CTradeDlg::RemoveTransaction(const Transaction * pTransaction, int flags)
{	
	int status = S_OK;
	Transactions::Iterator it;
	int count;
#if 1
	Transaction * pTa;
#else
	const Transaction * pTa;
#endif
	History * history = GetHistory();

	TSTRING_STD2(msg, size);
	if (flags & History::F_CLEAR)
	{
		LogMessage(TEXT("Удаление истории транзакций"));

		m_listCtrl.DeleteAllItems();
		if (history)
		{
			::std::list<DWORD> dates;
			m_transCtrl.GetDates(&dates);
			if (dates.size())
			{
				for (::std::list<DWORD>::iterator it = dates.begin(); it != dates.end(); it++)
					history->SetModified(*it);
			}
		}
		m_transCtrl.DeleteAllTransactions();		
		
		LogMessage(TEXT("История транзакций удалена"));
	}
	else if (pTransaction != NULL)
	{		
#if 1	
		pTa = m_transCtrl.FindTransactionByOrderNumber2(pTransaction->orderKey, pTransaction->line, it, GetTimeStampDate(pTransaction));
#else
		pTa = pTransaction;
#endif
		if (pTa != NULL)
		{
			_stprintf_s(msg, size, TEXT("Удаление транзакции N %.0f"), pTa->orderKey);
			LogMessage(msg);
			// Удаляем из списка транзакций:
			count = m_listCtrl.GetItemCount();
			for (int i = 0; i < count; i++)
			{
				if (pTa == (Transaction*)m_listCtrl.GetItemData(i))
				{
					m_listCtrl.DeleteItem(i);
					break;
				}
			} // for (i)

			if (history)
				history->SetModified(pTa);

			// Удаляем:
			m_transCtrl.RemoveTransaction(pTa, it);
		}
	}	
	
	if (flags & History::F_UPDATE)
	{
		m_listCtrl.Invalidate();

		// Пересчитываем трэйд (все трэйды):
		theApp.ResetAllTrades();

		BOOL sorted = FALSE;
		CMyTradeListCtrl::Sort sort, newSort;
		m_listCtrl.GetSort(sort);
		// Сортируем по возрастанию даты и времени:
		newSort.descending = 0;
		newSort.type = SORT_BY_DATE|SORT_BY_TIME;
		if (sort.type != newSort.type || sort.descending != newSort.descending)
		{
			m_listCtrl.DoSort(&newSort, 0);
			sorted = TRUE;
		}
		count = m_listCtrl.GetItemCount();
		for (int i = 0; i < count; i++)
		{
			pTa = (Transaction*)m_listCtrl.GetItemData(i);
			if (pTa)
			{
				LPCTSTR name = pTa->strName;
				theApp.OnTradeAction(name, pTa);
			}
		} // for (i)

		if (sorted)
		{// Восстанавливаем сортировку:
			m_listCtrl.DoSort(&sort, 0);
		}
		
		UpdateOSD();
		UpdateUserTrade();

		UpdateStateButtons();
		m_listCtrl.DoSort();
	}

	return status;
}

int CTradeDlg::AddToHistory(const Transaction * pTa)
{
	int status = -1;
	History * history = GetHistory();
	if (history)
	{
#if 0
		status = history->Add(pTa);
		if (status == S_OK)
#else
		// Это только ссылка на транзакции, поэтому всё уже добавлено!
#endif
		{
			if (my::wnd::IsValid(dlgHistory))
				dlgHistory.Update(pTa, History::F_ADD);
		}
	}
	return status;
}

int CTradeDlg::UpdateHistory(const Transaction * pTa)
{ 
	int status = -1;
	History * history = GetHistory();
	if (history)
	{
#if 0
		status = history->Update(pTa); 
		if (status == S_OK)
#endif
		{
			if (my::wnd::IsValid(dlgHistory))
				dlgHistory.Update(pTa, History::F_UPDATE);
		}
	}
	return status;
}

void CTradeDlg::SetHistoryModified(Transaction * pTa)
{
	History * history = GetHistory();
	if (history)
		history->SetModified(pTa);
} // if (! pExtTa)

void CTradeDlg::UpdateStateButtons(int iItem)
{
	if ((theApp.m_settings.common.quik.autoCheckConnection && !m_quik.connected))
	{
		BOOL enable = FALSE;
		EnableCancel(enable); EnableCancelAll(enable);
		EnableCancelSell(enable); EnableCancelSell(enable);
		EnableTradeZero(enable); EnableTradeReverse(enable);
	}
	else
	{
		BOOL enable, enableAll, enableBuy, enableSell, enableStop;
		CheckEnableCancel(enable, enableAll, enableBuy, enableSell, enableStop);
		EnableCancel(enable); EnableCancelAll(enableAll);
		EnableCancelBuy(enableBuy); EnableCancelSell(enableSell);

		BOOL enableZero, enableReverse;
		CheckEnableTrade(enableZero, enableReverse);
		EnableTradeZero(enableZero); EnableTradeReverse(enableReverse);
	}
}

BOOL CTradeDlg::CheckEnableBuySell(BOOL & enableBuy, BOOL & enableSell)
{
	enableBuy = enableSell = FALSE;
	enableBuy = IsAllowedBuy();
	enableSell = IsAllowedSell();
	return (enableBuy|enableSell);
}

BOOL CTradeDlg::CheckEnableStopOrder(BOOL & enable)
{
	enable = FALSE;
	enable = IsAllowedStop();
	return (enable);
}

BOOL CTradeDlg::CheckEnableCancel(BOOL & enable, BOOL & enableAll, BOOL & enableBuy, BOOL & enableSell, BOOL & enableStop, int iItem)
{
	enable = FALSE;
	enableAll = FALSE;
	enableBuy = FALSE;
	enableSell = FALSE;
	enableStop = FALSE;
#if 0
	if (IsAllowedCancel() || IsAllowedCancelAll())
#endif
	{
		if (iItem >= 0)
		{
			Transaction * pTransaction = (Transaction*)m_listCtrl.GetItemData(iItem);
			if (pTransaction != NULL)
			{
				if (Transaction::IsActive(pTransaction->status))
				{
					enable = TRUE;
					enableAll = TRUE;
					if (pTransaction->operation == Transaction::OPERATION_BUY)
						enableBuy = TRUE;
					if (pTransaction->operation == Transaction::OPERATION_SELL)
						enableSell = TRUE;
					if (Transaction::IsActionStop(pTransaction->baseAction))
						enableStop = TRUE;
				}			
			}
		}
		else
		{
			int count = m_listCtrl.GetItemCount();
			for (int i = 0; i < count; i++)
			{			
				Transaction * pTransaction = (Transaction*)m_listCtrl.GetItemData(i);
				if (pTransaction != NULL)
				{
					if (Transaction::IsActive(pTransaction->status))// || pTransaction->status == Transaction::STATUS_SENT)
					{					
						enableAll = TRUE;
						if (pTransaction->operation == Transaction::OPERATION_BUY)
							enableBuy = TRUE;
						if (pTransaction->operation == Transaction::OPERATION_SELL)
							enableSell = TRUE;
						if (Transaction::IsActionStop(pTransaction->baseAction))
							enableStop = TRUE;
						if (m_listCtrl.GetItemState(i, LVIS_SELECTED) == LVIS_SELECTED)
							enable = TRUE;
					}
				}
			} // for (i)
		}
	}
	return (enable|enableAll|enableBuy|enableSell|enableStop);
}

BOOL CTradeDlg::CheckEnableTrade(BOOL & enableZero, BOOL & enableReverse)
{
	enableZero = enableReverse = FALSE;
#if 0
	if (IsAllowedZero() || IsAllowedReverse())
#endif
	{
		QuoteTable * pTable = theApp.GetCurrentTable();
		if (pTable)
		{
			Trade trade;
			if (S_OK == theApp.GetTrade(pTable->GetName(), trade))
			{
				if (trade.quantity > 0)
					enableZero = enableReverse = TRUE;
			}
		}
	}
	return (enableZero | enableReverse);
}

BOOL CTradeDlg::CheckActiveBid(BOOL & active, int iItem)
{
	active = FALSE;
	if (iItem >= 0)
	{
		Transaction * pTransaction = (Transaction*)m_listCtrl.GetItemData(iItem);
		if (pTransaction != NULL && Transaction::IsActive(pTransaction->status))
			active = TRUE;
	}
	return active;
}

void CTradeDlg::EnableButtons(BOOL enable)
{
	EnableBuySell(enable);
	EnableTradeZero(enable); EnableTradeReverse(enable);
	EnableCancel(enable); EnableCancelAll(enable);
	EnableCancelSell(enable); EnableCancelSell(enable);
	EnableStop(enable);
}

void CTradeDlg::EnableBuySell(BOOL enable)
{
	CWindow btnBuy = GetDlgItem(IDC_BUTTON_BUY);
	CWindow btnSell = GetDlgItem(IDC_BUTTON_SELL);

	btnBuy.EnableWindow(enable);
	btnSell.EnableWindow(enable);
#if 0
	CWindow wnd;
	wnd = GetDlgItem(IDC_SPIN_TRADE_PRICE1); wnd.EnableWindow(enable);
	wnd = GetDlgItem(IDC_SPIN_TRADE_QUANTITY); wnd.EnableWindow(enable);
#endif
}

void CTradeDlg::EnableCancel(BOOL enable)
{
	if (m_mode == MODE_NORMAL)
	{
		CWindow btn = GetDlgItem(IDC_BUTTON_CANCEL);
		btn.EnableWindow(enable);
	}
}

void CTradeDlg::EnableCancelAll(BOOL enable)
{
	if (m_mode == MODE_NORMAL)
	{
		CWindow btn = GetDlgItem(IDC_BUTTON_CANCEL_ALL);
		btn.EnableWindow(enable);
	}
}

void CTradeDlg::EnableCancelBuy(BOOL enable)
{
	if (m_mode == MODE_ALTERNATIVE)
	{
		CWindow btn = GetDlgItem(IDC_BUTTON_CANCEL);
		btn.EnableWindow(enable);
	}
}

void CTradeDlg::EnableCancelSell(BOOL enable)
{
	if (m_mode == MODE_ALTERNATIVE)
	{
		CWindow btn = GetDlgItem(IDC_BUTTON_CANCEL_ALL);
		btn.EnableWindow(enable);
	}
}

void CTradeDlg::EnableTradeZero(BOOL enable)
{
	CWindow btn = GetDlgItem(IDC_BUTTON_ZERO);
	btn.EnableWindow(enable);
}

void CTradeDlg::EnableTradeReverse(BOOL enable)
{
	CWindow btn = GetDlgItem(IDC_BUTTON_REVERSE);
	btn.EnableWindow(enable);
}

void CTradeDlg::EnableStop(BOOL enable)
{
	CWindow btn = GetDlgItem(IDC_BUTTON_STOP);
	btn.EnableWindow(enable);
}

void CTradeDlg::EnableTrading(BOOL enable)
{
	if (enable)
	{
		EnableBuySell(enable);

		BOOL enableCancel, enableCancelAll, enableCancelBuy, enableCancelSell, enableCancelStop;
		CheckEnableCancel(enableCancel, enableCancelAll, enableCancelBuy, enableCancelSell, enableCancelStop);
		EnableCancel(enableCancel & enable); EnableCancelAll(enableCancelAll & enable);
		EnableCancelBuy(enableCancelBuy & enable); EnableCancelSell(enableCancelSell & enable);

		BOOL enableTradeZero, enableTradeReverse;
		CheckEnableTrade(enableTradeZero, enableTradeReverse);
		EnableTradeZero(enableTradeZero & enable); EnableTradeReverse(enableTradeReverse & enable);

		EnableStop(enable);
	}
	else
		EnableButtons(enable);
#if 1
	UpdateCaption();
#endif
}

void CTradeDlg::CheckAndEnableTrading()
{
	BOOL enable = (theApp.GetSettings().common.quik.autoCheckConnection) ? m_quik.connected : TRUE;
	EnableTrading(enable);
}

void CTradeDlg::CheckAndEnableTrade()
{
	BOOL enable = (theApp.GetSettings().common.quik.autoCheckConnection) ? m_quik.connected : TRUE;

	BOOL enableTradeZero, enableTradeReverse;
	CheckEnableTrade(enableTradeZero, enableTradeReverse);
	EnableTradeZero(enableTradeZero & enable); 
	EnableTradeReverse(enableTradeReverse & enable);
}

CTradeDlg::Mode CTradeDlg::GetMode() const
{
	return m_mode;
}

void CTradeDlg::SetMode(Mode mode)
{
	m_mode = mode;
}

void CTradeDlg::ChangeMode()
{
	int mode = GetMode();
	if (mode == MODE_NORMAL)
		SetMode(MODE_ALTERNATIVE);
	else
		SetMode(MODE_NORMAL);

	UpdateStateButtons();
	UpdateTextButtons();
	UpdateHintButtons();
}

void CTradeDlg::UpdateTextButtons()
{
	int mode = GetMode();
	HINSTANCE hInst = theApp.GetModuleInstance();

	TSTRING_SMALL(strBuy);
	TSTRING_SMALL(strSell);
	TSTRING_SMALL(strCancel1);
	TSTRING_SMALL(strCancel2);
	TSTRING_SMALL(strStop);
	TSTRING_SMALL(strZero);
	TSTRING_SMALL(strReverse);
	HICON hIconBuy, hIconSell, hIconCancel, hIconStop;
	
	CButton btnBuy(GetDlgItem(IDC_BUTTON_BUY));
	CButton btnSell(GetDlgItem(IDC_BUTTON_SELL));
	CButton btnCancel1(GetDlgItem(IDC_BUTTON_CANCEL));
	CButton btnCancel2(GetDlgItem(IDC_BUTTON_CANCEL_ALL));
	CButton btnStop(GetDlgItem(IDC_BUTTON_STOP));
	CButton btnZero(GetDlgItem(IDC_BUTTON_ZERO));
	CButton btnReverse(GetDlgItem(IDC_BUTTON_REVERSE));
	
	Settings::Trading & settings = theApp.m_settings.trading;	
	
	hIconBuy = hIconSell = hIconCancel = NULL;

#if 1
	CRect rect;
	btnBuy.GetWindowRect(&rect);
	int width = rect.Width();
	int width1 = 65;
	int width2 = 48;
	int width3 = 38;
	BOOL modern = TRUE;
#if AUTOSELECT_MODERN_THEME
	modern = IsModernOS();
#endif
	if (modern)
	{
		btnBuy.ModifyStyle(BS_ICON, BS_TEXT);
		btnSell.ModifyStyle(BS_ICON, BS_TEXT);
		btnStop.ModifyStyle(BS_ICON, BS_TEXT);
	}
#endif
	SAFE_TSTRCPY(strZero, TEXT("Обнулить"));
	SAFE_TSTRCPY(strReverse, TEXT("Развернуть"));

	if (mode == MODE_NORMAL)
	{
		SAFE_TSTRCPY(strBuy, TEXT(" Купить"));
		SAFE_TSTRCPY(strSell, TEXT("Продать"));
		SAFE_TSTRCPY(strCancel1, TEXT("Снять"));
		SAFE_TSTRCPY(strCancel2, TEXT("Снять все"));

		hIconBuy = CMyQuikApp::LoadIconEx(hInst, IDI_BUY, 16);
		hIconSell = CMyQuikApp::LoadIconEx(hInst, IDI_SELL, 16);
#if 1
		if (width < width1)
		{		
			SAFE_TSTRCPY(strCancel2, TEXT("xx"));
			if (width < width2)
			{
				SAFE_TSTRCPY(strCancel1, TEXT("x"));
				
				if (width < width3)
				{
					SAFE_TSTRCPY(strBuy, TEXT("К"));
					SAFE_TSTRCPY(strSell, TEXT("П"));
					if (modern)
					{
						btnBuy.ModifyStyle(BS_TEXT, BS_ICON);
						btnSell.ModifyStyle(BS_TEXT, BS_ICON);
					}
				}
				else
				{					
					SAFE_TSTRCPY(strBuy, TEXT(" К."));
					SAFE_TSTRCPY(strSell, TEXT(" П."));
				}
			}
			else
			{
				my::str::TrimRight(strBuy, -1, 3, TEXT('.'));
				my::str::TrimRight(strSell, -1, 3, TEXT('.'));
			}
		}
#endif
	}
	else
	{
#if 0
		SAFE_TSTRCPY(strBuy, settings.useSpread ? TEXT("Куп. рын.") : TEXT("Куп. спрэд"));
		SAFE_TSTRCPY(strSell, settings.useSpread ? TEXT("Прод. рын.") : TEXT("Прод. спрэд"));
#else
		SAFE_TSTRCPY(strBuy, TEXT("Куп. рын."));
		SAFE_TSTRCPY(strSell, TEXT("Прод. рын."));
#endif
		SAFE_TSTRCPY(strCancel1, TEXT("Снять куп."));
		SAFE_TSTRCPY(strCancel2, TEXT("Снять прод."));
#if 1
		if (width < width1)
		{
			int nBuy, nSell, nCancel1, nCancel2;
			int ch = TEXT('.');
			if (width < width2)
			{
				if (width < width3)
				{
					SAFE_TSTRCPY(strBuy, TEXT("Кр"));
					SAFE_TSTRCPY(strSell, TEXT("Пр"));
					SAFE_TSTRCPY(strCancel1, TEXT("xк"));
					SAFE_TSTRCPY(strCancel2, TEXT("xп"));
				}
				else
				{
					SAFE_TSTRCPY(strBuy, TEXT("К. р."));
					SAFE_TSTRCPY(strSell, TEXT("П. р."));
					SAFE_TSTRCPY(strCancel1, TEXT("xx к."));
					SAFE_TSTRCPY(strCancel2, TEXT("xx п."));
				}
				nBuy = nSell = 0;
				nCancel1 = nCancel2 = 0;
			}
			else
			{
				nBuy = 3; nSell = 3;
				SAFE_TSTRCPY(strCancel1, TEXT("xx куп."));
				SAFE_TSTRCPY(strCancel2, TEXT("xx прод."));
				nCancel1 = nCancel2 = 0;
			}
			my::str::TrimRight(strBuy, -1, nBuy, ch);
			my::str::TrimRight(strSell, -1, nSell, ch);
			my::str::TrimRight(strCancel1, -1, nCancel1, ch);
			my::str::TrimRight(strCancel2, -1, nCancel2, ch);
		}
#endif
	}
#if 0
	hIconCancel = CMyQuikApp::LoadIconEx(hInst, IDI_CANCEL, 16);
#endif
	SAFE_TSTRCPY(strStop, TEXT(" Стоп..."));
	hIconStop = CMyQuikApp::LoadIconEx(hInst, IDI_STOP, 16);
#if defined _DEBUG && 0
	CString str;
	str.Format(TEXT("hIconStop = 0x%x"), hIconStop);
	LogMessage(str);
#endif
#if 1
	if (width < width1)
	{
		int nZero, nReverse, nStop;
		int ch, chStop;
		ch = chStop = TEXT('.');
		if (width < width2)
		{			
			if (width < width3)
			{
				nZero = 7; nReverse = 9;
				nStop = 6;
				ch = 0; chStop = 0;
				if (modern)
					btnStop.ModifyStyle(BS_TEXT, BS_ICON);
			}
			else
			{
				nZero = 5; nReverse = 6;
				nStop = modern ? 6 : 5;
			}
		}
		else
		{
			nZero = 3; nReverse = 4;
			nStop = modern ? 3 : 0; chStop = 0;
		}
		my::str::TrimRight(strZero, -1, nZero, ch);
		my::str::TrimRight(strReverse, -1, nReverse, ch);

		my::str::TrimRight(strStop, -1, nStop, chStop);
	}
#endif
	btnBuy.SetWindowText(strBuy);
	btnSell.SetWindowText(strSell);
	btnCancel1.SetWindowText(strCancel1);
	btnCancel2.SetWindowText(strCancel2);
	btnReverse.SetWindowText(strReverse);
	btnZero.SetWindowText(strZero);
	btnStop.SetWindowText(strStop);
	if (modern)
	{
		btnBuy.SetIcon(hIconBuy); 
		btnSell.SetIcon(hIconSell); 
		btnCancel1.SetIcon(hIconCancel); 
#if 1
		btnStop.SetIcon(hIconStop); 
#else
		HBITMAP hBitmapStop = (HBITMAP)::LoadImage(hInst, MAKEINTRESOURCE(IDB_BITMAP1), IMAGE_BITMAP, 16, 16, LR_DEFAULTCOLOR);
		btnStop.SetBitmap(hBitmapStop); 
#endif
	}
}

void CTradeDlg::UpdateHintButtons()
{
	int mode = GetMode();
	HINSTANCE hInst = theApp.GetModuleInstance();

	LPCTSTR hintBuy, hintSell, hintCancel, hintCancelAll;
	
	CButton btnBuy(GetDlgItem(IDC_BUTTON_BUY));
	CButton btnSell(GetDlgItem(IDC_BUTTON_SELL));
	CButton btnCancel1(GetDlgItem(IDC_BUTTON_CANCEL));
	CButton btnCancel2(GetDlgItem(IDC_BUTTON_CANCEL_ALL));
	CButton btnStop(GetDlgItem(IDC_BUTTON_STOP));
	
	Settings::Trading & settings = theApp.m_settings.trading;	
	
	if (mode == MODE_NORMAL)
	{
		hintBuy = STRING_BUY; 
		hintSell = STRING_SELL;

		double price = this->GetPriceUser();
		if (price > 0)
		{
			hintBuy = STRING_BUY_FIXED; 
			hintSell = STRING_SELL_FIXED;
		}
		else
		{
			if (settings.autoPrice)
			{
				hintBuy = STRING_BUY_MARKET; 
				hintSell = STRING_SELL_MARKET;
			}
		}

		hintCancel = STRING_CANCEL_BID;
		hintCancelAll = STRING_CANCEL_ALL;
	}
	else
	{
		hintBuy = STRING_BUY_MARKET; 
		hintSell = STRING_SELL_MARKET;
		hintCancel = STRING_CANCEL_ALL_BUY;
		hintCancelAll = STRING_CANCEL_ALL_SELL;
	}

	m_hints.UpdateTipText(hintBuy, GetDlgItem(IDC_BUTTON_BUY));
	m_hints.UpdateTipText(hintSell, GetDlgItem(IDC_BUTTON_SELL));
	m_hints.UpdateTipText(hintCancel, GetDlgItem(IDC_BUTTON_CANCEL));
	m_hints.UpdateTipText(hintCancelAll, GetDlgItem(IDC_BUTTON_CANCEL_ALL));
}

void CTradeDlg::ChangeStyleQuotes()
{
	Settings::Presentation::Glass & settings = theApp.m_settings.presentation.glass;
	if (settings.view.style < GLASS_STYLE_3)
		settings.view.style++;
	else
		settings.view.style = GLASS_STYLE_1;
	theApp.SetModifySettings();
	if (my::wnd::IsValid(dlgGlass))
	{
		dlgGlass.SetSettings(settings);
		dlgGlass.Update(NULL);
	}
}

void CTradeDlg::SetHidden(BOOL hidden)
{
	if (my::wnd::IsValid(dlgGlass))
		dlgGlass.SetMaster(hidden);
	m_hidden = hidden;
}

BOOL CTradeDlg::IsHidden() const 
{ 
	return m_hidden; 
}

BOOL CTradeDlg::IsMaster() const
{
	return !IsHidden();
}

void CTradeDlg::KeepUserValues(QuoteTable * pTable)
{
	m_edits.price.Push(&pTable->user.ps);
	m_edits.quantity.Push(&pTable->user.qs);
}

void CTradeDlg::RestoreUserValues(QuoteTable * pTable)
{
	const QuoteTable::Properties * pProperties = pTable->GetProperties();
	m_edits.price.Pop(&pTable->user.ps, pProperties);
	m_edits.quantity.Pop(&pTable->user.qs, NULL);
}

void CTradeDlg::SelectInstrument(QuoteTable * pTable, LPCTSTR name)
{
#if 1	
	QuoteTable * pCurrentTable = theApp.GetCurrentTable();
	if (pCurrentTable && pCurrentTable != pTable)
	{// Запоминаем текущие параметры пользователя:		
		KeepUserValues(pCurrentTable);
	}
#endif
	const QuoteTable::Properties * pProperties = NULL;
	if (pTable != NULL)
	{
		name = pTable->GetName();
		pProperties = pTable->GetProperties();
	}

	if (theApp.IsAnotherToolName(name))
		theApp.SetModifySettings();

	theApp.SetCurrentTable(pTable);
	theApp.SetCurrentTrade(name);
	theApp.SetCurrentToolName(pProperties ? pProperties->name : name);

	if (my::wnd::IsValid(dlgGlass))
	{
#if 1		
		dlgGlass.SetUserTrade(0, NULL); // выход из режима активных заявок;

		dlgGlass.ClearActiveItems();

		if (IsGlassFrozen())
			ToggleFreeze(); // возобновляем движение цен;
		else
			dlgGlass.SetTable(theApp.GetCurrentTable());
#endif
		if (pTable != NULL)
			dlgGlass.Update(name, pTable ? pTable->priceUser : 0);
		else
			dlgGlass.Empty();
		dlgGlass.SetQuantity(GetQuantity());
	}
	UpdateCaption(pProperties);
	UpdateUserTrade();
	UpdateOSD();
#if 1
	if (pTable && pTable != pCurrentTable)
		RestoreUserValues(pTable);
	CheckAndEnableTrading();
#endif
}

void CTradeDlg::SelectInstrument(LPCTSTR name, DWORD flags)
{
	QuoteTable * pTable = theApp.FindTable(name);
#if 0
	if (pTable != NULL && theApp.GetCurrentTable() != pTable)
#else
	if (pTable == NULL && !(flags & F_FORCE))
		return;
	if (theApp.GetCurrentTable() != pTable || (pTable == NULL))
#endif
	{
		SelectInstrument(pTable, name);
	}
}

int CTradeDlg::ExtractInstrumentName(LPCTSTR name, size_t size)
{
	// Формат названия: "Инструмент (SECCODE)".
	// Выделяем только название инструмента:
	LPTSTR pStr = StrRChr(name, NULL, TEXT('('));
	if (pStr != NULL)
		pStr[-1] = 0;
	return 0;
}

double CTradeDlg::GetPriceUser() const
{
	double price = 0;
	CWindow wnd = GetDlgItem(IDC_EDIT_TRADE_PRICE1);
	TSTRING_SMALL2(str, size);
	TCHAR * pEnd;
	wnd.GetWindowText(str, size);
	price = _tcstod(str, &pEnd);
	return price;
}

double CTradeDlg::GetPriceTa() const
{
	double price = 0;
#if 0
	if (GetFocus() == m_listCtrl)
#endif
	{
		const Transaction * pTa = GetSelectedTa();
		if (pTa)
			price = pTa->price;
	}
	return price;
}

double CTradeDlg::GetQuantity() const
{
	double quantity = 0;
	CWindow wnd = GetDlgItem(IDC_EDIT_TRADE_QUANTITY);
	TSTRING_SMALL2(str, size);
	wnd.GetWindowText(str, size);				
	quantity = StrToInt(str);
	return quantity;
}

double CTradeDlg::GetPriceCurrent() const
{
	double price = 0;
	const QuoteTable * pTable = theApp.GetCurrentTable();
	if (pTable)
		price = pTable->price;
	return price;
}

Transaction * CTradeDlg::GetSelectedTa()
{
	Transaction * pTa = NULL;
	int iItem = my::ctrl::GetFocusedItem(m_listCtrl);
	if (iItem >= 0)
		pTa = reinterpret_cast<Transaction*>(m_listCtrl.GetItemData(iItem));
	return pTa;
}

const Transaction * CTradeDlg::GetSelectedTa() const
{
	const Transaction * pTa = NULL;
	int iItem = my::ctrl::GetFocusedItem(m_listCtrl);
	if (iItem >= 0)
		pTa = reinterpret_cast<Transaction*>(m_listCtrl.GetItemData(iItem));
	return pTa;
}

void CTradeDlg::SetLoading(BOOL loading)
{
	m_loading = loading;
}

void CTradeDlg::Buy(int flags)
{
	BuySell(TRUE, flags);
}

void CTradeDlg::Sell(int flags)
{
	BuySell(FALSE, flags);
}

int CancelTransaction(Transaction * pTa)
{
	//pTa->baseAction = pTa->action; // ????????????????
#if 0
	pTa->action = pTa::KILL_ORDER;
#else
	if (pTa->baseAction == Transaction::NEW_STOP_ORDER)
		pTa->action = Transaction::KILL_STOP_ORDER;
	else
		pTa->action = Transaction::KILL_ORDER;
#endif
	return theApp.DoTransaction(pTa);
}

void CTradeDlg::CancelOrders(int flags)
{
	BOOL cancel = FALSE;
	int count = m_listCtrl.GetItemCount();
	int n = 0;
	const Settings::Trading & settings = theApp.m_settings.trading;
	if (settings.flags & F_TRADING_CANCEL_ALL)
	{
		TSTRING_STD2(text, size);
		LPCTSTR pStr = NULL;
		Transaction::Action action = Transaction::NEW_ORDER;
		Transaction::Operation operation = Transaction::OPERATION_UNKNOWN;
		LPCTSTR classcode = NULL;
		LPCTSTR clientcode = NULL;
		LPCTSTR account = NULL;
		LPCTSTR seccode = NULL;

		if (flags & CANCEL_ORDERS_STOP)
			action = Transaction::KILL_ALL_STOP_ORDERS;
		else if (flags != CANCEL_ORDER)
			action = Transaction::KILL_ALL_ORDERS;
		if (action != Transaction::NEW_ORDER)
		{
#if 0
			for (int i = 0; i < count; i++)
			{
				Transaction * pTa = (Transaction*)m_listCtrl.GetItemData(i);
				if (pTa != NULL && Transaction::IsActive(pTa->status))
				{
					if ((flags & CANCEL_ORDER) && (m_listCtrl.GetItemState(i, LVIS_SELECTED) == LVIS_SELECTED))
						cancel = TRUE;
					else if ((flags & CANCEL_ORDERS_BUY) && pTa->operation == Transaction::OPERATION_BUY)
						cancel = TRUE;
					else if ((flags & CANCEL_ORDERS_SELL) && pTa->operation == Transaction::OPERATION_SELL)
						cancel = TRUE;
					else if ((flags & CANCEL_ORDERS_STOP) && Transaction::IsActionStop(pTa->baseAction))
						cancel = TRUE;
					else if (flags == CANCEL_ORDERS_ALL)
						cancel = TRUE;
					if (cancel == TRUE)
					{
						if (-1 == CancelTransaction(pTa))
							break;
						cancel = FALSE;
						n++;
					}
				}
			} // for (i)
#else
			classcode = TEXT("SPBFUT");
			//account = TEXT("SPBFUTXXXXX");
			//action = Transaction::KILL_ALL_FUTURES_ORDERS;
			//seccode = TEXT("SBRF");
#endif
			if (! ((flags & CANCEL_ORDERS_BUY) && (flags & CANCEL_ORDERS_SELL)))
			{
				if (flags & CANCEL_ORDERS_BUY)
					operation = Transaction::OPERATION_BUY;
				else if (flags & CANCEL_ORDERS_SELL)
					operation = Transaction::OPERATION_SELL;
			}
			int ret;
			if (settings.confirmTransaction)
			{
				int n = _stprintf_s(text, size, TEXT("Вы действительно желаете снять все %sзаявки"),
					(action == Transaction::KILL_ALL_STOP_ORDERS) ? TEXT("стоп-") : TEXT(""));
				if (operation != Transaction::OPERATION_UNKNOWN)
				{
					n += _stprintf_s(text + n, size - n, TEXT("%s"), 
					(operation == Transaction::OPERATION_BUY) ? TEXT(" на покупку") : TEXT(" на продажу"));
				}
				n += _stprintf_s(text + n, size - n, TEXT("?"));
				ret = MessageBox(text, MB_ICONQUESTION|MB_YESNOCANCEL);
			}
			else
				ret = IDYES;
			if (ret == IDYES)
			{
				int result;
				m_transCtrl.CancelTransactions(action, operation, seccode, classcode, clientcode, account, this, &result);
				if (result != S_OK)
					OnFailTransaction(result);
			}
			cancel = TRUE;
		}
	}
	if (! cancel)
	{
		for (int i = 0; i < count; i++)
		{
			Transaction * pTa = (Transaction*)m_listCtrl.GetItemData(i);
			if (pTa != NULL && Transaction::IsActive(pTa->status))
			{
				if ((flags & CANCEL_ORDER) && (m_listCtrl.GetItemState(i, LVIS_SELECTED) == LVIS_SELECTED))
					cancel = TRUE;
				else if ((flags & CANCEL_ORDERS_BUY) && pTa->operation == Transaction::OPERATION_BUY)
					cancel = TRUE;
				else if ((flags & CANCEL_ORDERS_SELL) && pTa->operation == Transaction::OPERATION_SELL)
					cancel = TRUE;
				else if ((flags & CANCEL_ORDERS_STOP) && Transaction::IsActionStop(pTa->baseAction))
					cancel = TRUE;
				else if (flags == CANCEL_ORDERS_ALL)
					cancel = TRUE;
				if (cancel == TRUE)
				{
					if (-1 == CancelTransaction(pTa))
						break;
					cancel = FALSE;
					n++;
				}
			} // if (pTa != NULL)
		} // for (i)
	}
#if 0
	if (n > 0)
		SetUserTrade(-1);
#endif
}

void CTradeDlg::OnFailTransaction(int result)
{
	TSTRING_STD2(str, size);
	int n = _stprintf_s(str, size, TEXT("Ошибка транзакции"));
	LogMessage(str, CODE_USER_ERROR);

	if (theApp.m_settings.trading.flags & F_TRADING_PRINT_MESSAGE_ON_TRANSACTION_ERROR)
	{
		_stprintf_s(str + n, size - n, TEXT("!\r\n%s"), TRANS2QUIK::ResultToString(result));
		MessageBox(str, MB_ICONWARNING|MB_OK);
	}
}

void MakeUserParams(int iUser, int & bid, int & stop, Transaction::Operation & operation)
{
	bid = stop = 0;
	operation = Transaction::OPERATION_UNKNOWN;
	if (iUser == QuoteTable::ITEM_USER_BID_BUY) {bid = 1; operation = Transaction::OPERATION_BUY;}
	else if (iUser == QuoteTable::ITEM_USER_BID_SELL) {bid = 1; operation = Transaction::OPERATION_SELL;}
	else if (iUser == QuoteTable::ITEM_USER_STOP_BUY) {stop = 1; operation = Transaction::OPERATION_BUY;}
	else if (iUser == QuoteTable::ITEM_USER_STOP_SELL) {stop = 1; operation = Transaction::OPERATION_SELL;}
}

void CTradeDlg::CancelOrderGlass()
{
	typedef ::std::list<TradeItem> List;
	struct ArrayItem {
		Transaction * pTa;
		List::iterator it;
		ArrayItem()
		{}
		ArrayItem(Transaction * pTa, List::iterator it)
		{this->pTa = pTa; this->it = it;}
	};
	typedef ::std::vector<ArrayItem> Array;

	if (my::wnd::IsValid(dlgGlass) && dlgGlass.IsMarkerVisible())
	{		
		BOOL enableCancel = FALSE;	
		const CDlgGlass::Active::Item & active = dlgGlass.GetActiveItem(CDlgGlass::I_ACTIVE_MARKER);
		if (active.flags & CDlgGlass::F_ACTIVE_ITEM_IS_VALID)
		{			
			const double price = active.item.values[QuoteTable::ITEM_PRICE];
			List list;
			if (theApp.FindItems(list, price))
			{
				int ii[] = {
					QuoteTable::ITEM_USER_BID_BUY, QuoteTable::ITEM_USER_BID_SELL, 
					QuoteTable::ITEM_USER_STOP_BUY, QuoteTable::ITEM_USER_STOP_SELL, 
				};

				for (int i = 0; i < SIZEOF_ARRAY(ii); i++)
				{
					int iUser = ii[i];
					BOOL pass = FALSE;
					if (active.iUser >= 0)
					{
						if (active.iUser == iUser)
							pass = TRUE;
					}
					else
						pass = TRUE;
					if (pass)
					{
						double quantity = active.item.values[iUser];
						if (quantity)
						{
							int n = list.size();
							if (n == 0)
								break;

							Array ar(n);

							int bid, stop;
							Transaction::Operation operation;
							MakeUserParams(iUser, bid, stop, operation);

							int iItem = 0;
							List::iterator it = list.begin();
							for (int j = 0; j < n; j++, it++)
							{
								TradeItem & item = *it;
								if (Transaction::IsActive(item.status) && (item.operation == operation))
								{
									int add = 0;
									if (Transaction::IsActionStop(item.action))
									{// Стоп-заявка:
										if (stop)
											add = 1;
									}
									else
									{// Заявка:
										if (bid)
											add = 1;
									}
									if (add)
										ar[iItem++] = ArrayItem(item.pTa, it);
								}

							} // for (j)

							if (iItem)
							{
								// Сортируем массив по времени совершения транзакции:
								for (int j = 0; j < iItem; j++)
								{
									for (int k = iItem - 1; k > j; k--)
									{
										int j1 = k - 1;
										int j2 = k;
										ArrayItem & item1 = ar[j1];
										ArrayItem & item2 = ar[j2];
										QWORD stamp1 = GetTimeStamp64(item1.pTa);
										QWORD stamp2 = GetTimeStamp64(item2.pTa);
										if (stamp2 < stamp1)
										{
											Transaction * pTa = item1.pTa;
											List::iterator it = item1.it;
											ar[j1] = ArrayItem(item2.pTa, item2.it);
											ar[j2] = ArrayItem(pTa, it);
										}
									} // for (k)
								} // for (j)
								// Теперь ищем нужную заявку:
								int iFound = -1;
								for (int j = 0; j < iItem; j++)
								{
									ArrayItem & item = ar[j];
									if (item.pTa->quantity == quantity)
									{
										iFound = j;
										break;
									}
								} // for (j)
								int iFirst, iLast;
								if (iFound >= 0)
								{
									iFirst = iFound; iLast = iFirst + 1;
								}
								else
								{
									iFirst = 0; iLast = iItem;
								}
								// Вот оно - отмена заявки!
								double q = 0;
								BOOL cancelCancel = FALSE;
								for (int j = iFirst; j < iLast; j++)
								{
									ArrayItem & item = ar[j];
									list.erase(item.it);
									if (-1 == CancelTransaction(item.pTa))
									{// Прервано пользователем
										cancelCancel = TRUE;
										break;
									}
									q += item.pTa->quantity;
									if (q >= quantity)
										break;
								} // for (j)
								if (cancelCancel)
									break;
							} // if (iItem)
						} // if (quantity)
					} // if (pass)
				} // for (i)
			} // if (n)	
		}
	}
}

void CTradeDlg::ShowGlass(int show, int autoHide)
{
	CWindow activeWindow = GetActiveWindow();

	const Settings::Presentation & presentation = theApp.m_settings.presentation;
	if (dlgGlass == NULL)
	{
		dlgGlass.SetSettings(presentation.glass);
		if (dlgGlass.Create(*this))
		{
			CREATESTRUCT cs;
			if (S_OK == theApp.LoadWindowPosition(&cs, dlgGlass.GetName()))
				dlgGlass.MoveWindow(cs.x, cs.y, cs.cx, cs.cy, TRUE);
			else
			{
				dlgGlass.PostMessage(WM_SIZE, 0, 0);
			}
		}
	}
	if (dlgGlass)
	{
		if (! show)
		if (! autoHide)
		{
			if (dlgGlass.IsWindowVisible())
				show = SW_HIDE;
			else
				show = SW_SHOW;
		}
		BOOL activeSettings = IsActiveDlgSettings(activeWindow);
		if (show)
		{
			if (activeSettings)
				show = SW_SHOWNOACTIVATE;
		}
		const Settings::Presentation::Window & glass = presentation.view.item[I_WINDOW_GLASS];
		my::wnd::SetDialogTitleBar(dlgGlass, glass.flags & F_SHOW_TITLE);
		dlgGlass.ShowWindow(show);
		if (show)
		{
			if (activeSettings)
				dlgSettings->SetFocus();
			else
				dlgGlass.SetFocus();
			dlgGlass.UpdateWindow();
		}
#if 0
		if (show)
			this->SetFocus();
#endif
	}
}

void CTradeDlg::ShowHistory(int show, int autoHide)
{
	CWindow activeWindow = GetActiveWindow();

	const Settings::Presentation & presentation = theApp.m_settings.presentation;
	if (dlgHistory == NULL)
	{
		dlgHistory.Attach(GetHistory());

		TSTRING_PATH_EX(path, size);
		_stprintf_s(path, size - 1, TEXT("%s\\%s"), TEXT("Workspace"), dlgHistory.GetName());
		int width;
		QueryIntValue(theApp.GetRegKey(), width, path, TEXT("_tree_width"), 0);
		dlgHistory.SetWidthTreeCtrl(width);

		if (dlgHistory.Create(*this))
		{
			CREATESTRUCT cs;
			if (S_OK == theApp.LoadWindowPosition(&cs, dlgHistory.GetName()))
				dlgHistory.MoveWindow(cs.x, cs.y, cs.cx, cs.cy, TRUE);
			//else
			{
				dlgHistory.PostMessage(WM_SIZE, 0, 0);
			}
		}
	}
	if (dlgHistory)
	{
		if (! show)
		if (! autoHide)
		{
			if (dlgHistory.IsWindowVisible())
				show = SW_HIDE;
			else
				show = SW_SHOW;
		}
		BOOL activeSettings = IsActiveDlgSettings(activeWindow);
		if (show)
		{
			if (activeSettings)
				show = SW_SHOWNOACTIVATE;
		}
		const Settings::Presentation::Window & history = presentation.view.item[I_WINDOW_HISTORY];
		my::wnd::SetDialogTitleBar(dlgGlass, history.flags & F_SHOW_TITLE);
		dlgHistory.ShowWindow(show);
		if (show)
		{
			LPCTSTR text;
			if (theApp.GetSettings().trading.history.modify)
				text = TEXT("История транзакций - режим редактирования");
			else
				text = TEXT("История транзакций");
			dlgHistory.SetWindowText(text);
#if 0
			activeWindow.SetFocus();
#else
			if (activeSettings)
				dlgSettings->SetFocus();
			else
				dlgHistory.SetFocus();
#endif
		}
	}
}

BOOL CTradeDlg::IsActiveDlgSettings(CWindow activeWnd) const
{
	BOOL active = FALSE;
	if (dlgSettings && my::wnd::IsVisible(*dlgSettings) && activeWnd == *dlgSettings)
		active = TRUE;
	return active;
}

void CTradeDlg::ShowDlgStop(int show, int autoHide)
{
	CWindow activeWnd = GetActiveWindow();
	
	const Settings::Presentation & presentation = theApp.m_settings.presentation;
	if (dlgStop == NULL)
	{
		if (dlgStop.Create(*this))
		{
#if 0
			CREATESTRUCT cs;
			if (S_OK == theApp.LoadWindowPosition(&cs, dlgStop.GetName()))
				dlgStop.MoveWindow(cs.x, cs.y, cs.cx, cs.cy, TRUE);
			else
			{
				dlgStop.PostMessage(WM_SIZE, 0, 0);
			}
#endif
		}
	}
	if (dlgStop)
	{
#if 0
		if (dlgStop.IsWindowVisible())
			dlgStop.SetFocus();
#else
		if (! show)
		if (! autoHide)
		{
			if (dlgStop.IsWindowVisible())
				show = SW_HIDE;
			else
				show = SW_SHOW;
		}
		BOOL activeSettings = IsActiveDlgSettings(activeWnd);
		if (show)
		{
			dlgStop.Init(theApp.GetCurrentTable(), activeWnd);
			if (activeSettings)
				show = SW_SHOWNOACTIVATE;
		}
		const Settings::Presentation::Window & stop = presentation.view.item[I_WINDOW_STOP];
		//my::wnd::SetDialogTitleBar(dlgStop, stop.flags & F_SHOW_TITLE);
		dlgStop.ShowWindow(show);
		if (show)
		{
			if (activeSettings)
				dlgSettings->SetFocus();
			else
				dlgStop.SetFocus();
		}
#endif
	}
}

void CTradeDlg::ShowLog(int show, int hide)
{
	CWindow activeWnd = GetActiveWindow();

	const Settings::Presentation & presentation = theApp.m_settings.presentation;
	if (dlgLog == NULL)
	{
		if (dlgLog.Create(*this))
		{
			CREATESTRUCT cs;
			if (S_OK == theApp.LoadWindowPosition(&cs, dlgLog.GetName()))
				dlgLog.MoveWindow(cs.x, cs.y, cs.cx, cs.cy);
		}
	}
	if (dlgLog)
	{
		if (! show)
		if (! hide)
		{
			if (dlgLog.IsWindowVisible())
				show = SW_HIDE;
			else
				show = SW_SHOW;
		}
		BOOL activeSettings = IsActiveDlgSettings(activeWnd);
		if (show)
		{
			if (activeSettings)
				show = SW_SHOWNOACTIVATE;
		}
		const Settings::Presentation::Window & log = presentation.view.item[I_WINDOW_LOG];
		my::wnd::SetDialogTitleBar(dlgGlass, log.flags & F_SHOW_TITLE);
		dlgLog.ShowWindow(show);
		if (show)
		{
			if (activeSettings)
				dlgSettings->SetFocus();
			else
				dlgLog.SetFocus();
		}
	}
}

void CTradeDlg::LogMessage(LPCTSTR msg, int code)
{
	if (my::wnd::IsValid(dlgLog))
	{
		dlgLog.PutMessage(msg, code);
	}
}

BOOL CTradeDlg::ShowProperties(LPCTSTR name, LPCTSTR seccode, const QuoteTable::Properties * pProperties, int flags)
{
	BOOL result = FALSE;
	int editCurrent = (flags == 0);
	if (dlgProperties == NULL)
	{
		dlgProperties = new CDlgProperties();
		if (dlgProperties)
		{	
			BOOL update = editCurrent;
			QuoteTable::Properties properties;
			QuoteTable * pTable = theApp.GetCurrentTable();
			if (name != NULL || pTable == NULL)
			{
				if (pProperties == NULL)
				{
					pProperties = theApp.FindTableProperty(name);
					if (pProperties == NULL)
					{
						properties.Initialize(name, seccode);
						pProperties = &properties;
					}
				}
				else
				{
					properties = *pProperties;
					SAFE_TSTRCPY(properties.name, name);
				}
#if 0
				update = FALSE;
#endif
			}
			else
				pProperties = pTable ? pTable->GetProperties() : NULL;

			if (pProperties != NULL)
			{
				dlgProperties->Init(name, pProperties, theApp.GetListOfTableProperties());
				HWND hParent;
#if 0
				hParent = my::wnd::IsValid(dlgGlass) ? static_cast<HWND>(dlgGlass) : static_cast<HWND>(*this);
#else
				hParent = *this;
#endif
				INT_PTR ret = dlgProperties->DoModal(hParent);
				if (IDOK == ret)
				{
					BOOL selectInstrument = FALSE;
					BOOL updateCaption = FALSE;
					const QuoteTable::Properties * pDlgProperties = dlgProperties->GetProperties();
					if (editCurrent && 0 != memcmp(pDlgProperties, pProperties, sizeof(QuoteTable::Properties)))
					{// Обновляем свойства таблицы:
						update = TRUE;
						if (0 != lstrcmp(pDlgProperties->name, pProperties->name))
						{
							updateCaption = TRUE;	
							if (theApp.IsSameToolName(pProperties->name))
								selectInstrument = TRUE;
							RemoveTableProperties(pProperties->name);
							theApp.UpdateListOfTableProperties(pProperties->name, pProperties, ListOfTableProperties::F_REMOVE);
						}						
					}	
					pProperties = pDlgProperties;
					SaveTableProperties(pProperties->name, pProperties);
					theApp.UpdateListOfTableProperties(pProperties->name, pProperties, ListOfTableProperties::F_UPDATE);
					if (update)
					{
						if (pTable)
							pTable->SetProperties(pProperties);							
					}
					if (updateCaption)
						UpdateCaption(pProperties);
					if (update || updateCaption)
					{
						if (my::wnd::IsValid(dlgGlass))
						{
							dlgGlass.SetQuantity(GetQuantity());
							dlgGlass.UpdateName(pProperties->name);
						}
					}
					if (selectInstrument)
						SelectInstrument(pTable);
					result = TRUE;
				} // if (IDOK == ret)
			} // if (pProperties != NULL)
			SAFE_DELETE(dlgProperties);
		} // if (dlgProperties)
	} // if (dlgProperties == NULL)
	return result;
}

void CTradeDlg::UpdateOSD()
{
	CRect rect;
	CWindow wnd = GetDlgItem(IDC_OSD);
	wnd.GetClientRect(&rect);
	InvalidateRect(&rect, FALSE);
}

QuoteTable * CTradeDlg::OnSecCode(LPCTSTR name, LPCTSTR seccode, LPCTSTR classcode, LPCTSTR clientcode, LPCTSTR account, 
	double price, double priceMin, double priceMax, 
	double demand, double supply, double percent, double priceBid, double priceOffer)
{
	QuoteTable * pTable = theApp.FindTable(name, seccode, classcode, clientcode, account);
#if 1
	if (pTable == NULL)
	{
		const QuoteTable::Properties * pProperties = theApp.FindTableProperty(name, seccode, classcode, clientcode, account);
		if (pProperties != NULL)
			pTable = theApp.CreateTable(pProperties->name, pProperties);
	}
#endif
	if (pTable != NULL)
	{						
		const QuoteTable::Properties * pProperties = pTable->GetProperties();

		if (theApp.GetCurrentTable() == NULL)
		{
			if (theApp.IsSameToolName(pProperties->name))
				SelectInstrument(pTable);
		}
		if (theApp.GetCurrentTable() == pTable)
		{
			pTable->SetCurrentPrice(price);
			pTable->SetMinMax(priceMin, priceMax);
			pTable->SetDemandAndSupply(demand, supply);
			pTable->SetCurrentPercent(percent);
			pTable->SetPricesBidOffer(
				(priceBid == 0) ? pTable->theBestBuyPrice : priceBid, 
				(priceOffer == 0) ? pTable->theBestSellPrice : priceOffer);
#if defined _DEBUG && 0
			TRACE("Current price is %f\r\n", price);
#endif
#if 1
			if (IsGlassFrozen())
			{
				QuoteTable * pTableTmp = theApp.GetTableTmp();
				CopyPrices(*pTableTmp, *pTable);
			}
#endif
		}
	} // if (pTable != NULL)
	return pTable;
}

void CTradeDlg::Resize(int cx, int cy, BOOL autoSize)
{
	const Settings::Presentation::Main & settings = theApp.GetSettings().presentation.main;
	BOOL showList = settings.show.wnd & F_MAIN_SHOW_LIST;
	BOOL showOsd = settings.show.wnd & F_MAIN_SHOW_OSD;
	BOOL showControls = settings.show.wnd & F_MAIN_SHOW_CONTROLS;

	int x, y;
	CSize offset;

	//const int cxDlgFrame = ::GetSystemMetrics(SM_CXDLGFRAME);
	//const int cyDlgFrame = ::GetSystemMetrics(SM_CYDLGFRAME);
	const int cxFrame = ::GetSystemMetrics(SM_CXFRAME);
	const int cyFrame = ::GetSystemMetrics(SM_CYFRAME);
	//const int cyCaption = ::GetSystemMetrics(SM_CYCAPTION);
	//const int cxBorder = ::GetSystemMetrics(SM_CXBORDER);
	//const int cyBorder = ::GetSystemMetrics(SM_CYBORDER);
	//const int cxEdge = ::GetSystemMetrics(SM_CXEDGE);

	int border = settings.margins;

	CWindow wnd;
	CRect rect, dlgRect, listRect, frameRect, newFrameRect, osdRect;

	this->GetWindowRect(&dlgRect);
	this->GetClientRect(&rect);

	CWindow wndOSD = GetDlgItem(IDC_OSD);
	wndOSD.GetWindowRect(&osdRect);

	CWindow wndFrame = GetDlgItem(IDC_STATIC_FRAME);
	wndFrame.GetWindowRect(&frameRect);

	if (autoSize)
	{
		if (! showList)
		{// Автоматически изменяем размер, если список сделок не отображается
			int height;
			if (showOsd || showControls)
			{
				height = (dlgRect.Height() - rect.Height()) + border;
				if (showOsd)
					height += osdRect.Height() + border;
				if (showControls)
					height += frameRect.Height() + border;
			}
			else
				height = 0;
			SetWindowPos(NULL, 0, 0, dlgRect.Width(), height, SWP_NOMOVE);					
		}
		else
		{
			CREATESTRUCT cs;
			if (S_OK == theApp.LoadWindowPosition(&cs, GetName()))
			{
				MoveWindow(cs.x, cs.y, cs.cx, cs.cy);
			}
		}
	} // if (autoSize)

	// Обновляем размеры:
	this->GetWindowRect(&dlgRect);
	this->GetClientRect(&rect);
	wndOSD.GetWindowRect(&osdRect);
	wndFrame.GetWindowRect(&frameRect);

	x = border; y = rect.bottom - border;

	int show = SW_SHOW;
	int hide = SW_HIDE;

	// Элементы управления:
	UINT items[] = {
		IDC_STATIC_PRICE, IDC_EDIT_TRADE_PRICE1, IDC_SPIN_TRADE_PRICE1,
		IDC_BUTTON_BUY, IDC_BUTTON_CANCEL, IDC_BUTTON_ZERO, IDC_BUTTON_STOP,
		IDC_STATIC_QUANTITY, IDC_EDIT_TRADE_QUANTITY, IDC_SPIN_TRADE_QUANTITY,
		IDC_BUTTON_SELL, IDC_BUTTON_CANCEL_ALL, IDC_BUTTON_REVERSE,
	};
	int offsetFlags[] = {
		-1, -1, -1, 1, 2, 3, 0,
		-1, -1, -1, 1, 2, 3,
	};
	
	if (showControls)
	{		
		y -= frameRect.Height();

		wndFrame.MoveWindow(x, y, dlgRect.Width() - 2*(border + cxFrame), frameRect.Height(), FALSE);
		wndFrame.GetWindowRect(&newFrameRect);
		wndFrame.ShowWindow((settings.show.frame & F_MAIN_SHOW_CONTROLS) ? show : hide);
#if defined _DEBUG && 0
		GetDlgItem(IDC_BUTTON_BUY).GetWindowRect(&rect);
		TRACE("IDC_BUTTON_BUY: width=%d\r\n", rect.Width());
#endif
		int gap = 2;
		int width, cx, dx;
		wnd = GetDlgItem(IDC_SPIN_TRADE_PRICE1);
		wnd.GetWindowRect(&rect);
		if (dlgRect.Width() < 571)
			width = newFrameRect.right - rect.right;
		else
			width = 405;	
		// Система уравнений: 
		//  gap + 4cx + 5dx = width 
		//  cx = k*dx;
		double k = 5;
		cx = static_cast<int>(0.5 + (double)(width - gap) / (4 + 5/k));
#if 0
		if (cx < 65)
			cx = 65;
#endif
		dx = static_cast<int>(0.5 + (double)(width - gap - 4*cx) / 5);
		
		offset.cx = 0;
		for (int i = 0; i < SIZEOF_ARRAY(items); i++)
		{
			const UINT id = items[i];
			const int flags = offsetFlags[i];
			wnd = GetDlgItem(id);
			wnd.GetWindowRect(&rect);
			rect.OffsetRect(-frameRect.left, -frameRect.top);
			
			if (flags < 0)
			{
				width = rect.Width();
				offset.cx = rect.left;
			}
			else
			{				
				width = cx;
				offset.cx += dx;
				if (flags == 1)
					offset.cx += gap;
			}
			wnd.MoveWindow(x + offset.cx, y + rect.top, width, rect.Height(), FALSE);
			wnd.ShowWindow(show);
			offset.cx += width;
		} // for (i)
		y -= border;
	} // if (showControls)
	else
	{
		frameRect.top = frameRect.bottom = 0;
		wndFrame.ShowWindow(hide);
		for (int i = 0; i < SIZEOF_ARRAY(items); i++)
		{
			wnd = GetDlgItem(items[i]);
			wnd.ShowWindow(hide);
		} // for (i)
	}

	//frameRect = newFrameRect;

	// Область информации:
	if (showOsd)
	{
		y -= osdRect.Height();
		wndOSD.MoveWindow(x, y, dlgRect.Width() - 2*(border + cxFrame), osdRect.Height(), FALSE);
		wndOSD.ShowWindow(show);
		y -= border;
	}
	else
	{
		osdRect.top = osdRect.bottom = 0;
		wndOSD.ShowWindow(hide);
	}

	// Список:
	CWindow wndListFrame = GetDlgItem(IDC_LIST_FRAME);
	CWindow wndLoading = GetDlgItem(IDC_STATIC_LOADING);
	CWindow wndList = GetDlgItem(IDC_LIST1);
	if (showList)
	{
		y -= border;		
		wndListFrame.MoveWindow(x, border, dlgRect.Width() - 2*(border + cxFrame), y, FALSE);		
		wndListFrame.ShowWindow((settings.show.frame & F_MAIN_SHOW_LIST) ? show : hide);
		wndLoading.ShowWindow(SW_HIDE);
		wndListFrame.GetWindowRect(&listRect);
		wndLoading.GetWindowRect(&rect);
		wndLoading.MoveWindow(x + (listRect.Width() - rect.Width()) / 2, border + (listRect.Height() - rect.Height()) / 2,
		rect.Width(), rect.Height(), FALSE); 		
		wndList.MoveWindow(x + 1, border + 1, dlgRect.Width() - 2*(border + cxFrame) - 1 - 1, y - 1 - 1, FALSE);		
		wndList.ShowWindow(show);
	}
	else
	{
		wndListFrame.ShowWindow(hide);
		wndList.ShowWindow(hide);
		wndLoading.ShowWindow(hide);
	}
	Invalidate();
}

BOOL CTradeDlg::IsAllowed(UINT id) const
{
	CWindow wnd = GetDlgItem(id);
	return wnd.IsWindowEnabled();
}

BOOL CTradeDlg::IsAllowedBuy() const {return IsAllowed(IDC_BUTTON_BUY);}
BOOL CTradeDlg::IsAllowedSell() const {return IsAllowed(IDC_BUTTON_SELL);}
BOOL CTradeDlg::IsAllowedCancel() const {return IsAllowed(IDC_BUTTON_CANCEL);}
BOOL CTradeDlg::IsAllowedCancelAll() const {return IsAllowed(IDC_BUTTON_CANCEL_ALL);}
BOOL CTradeDlg::IsAllowedStop() const {return IsAllowed(IDC_BUTTON_STOP);}
BOOL CTradeDlg::IsAllowedReverse() const {return IsAllowed(IDC_BUTTON_REVERSE);}
BOOL CTradeDlg::IsAllowedZero() const {return IsAllowed(IDC_BUTTON_ZERO);}

BOOL CTradeDlg::IsGlassFrozen() const
{
	return m_freeze;
}

void CTradeDlg::ToggleFreeze()
{
	BOOL frozen = m_freeze;
	BOOL freeze = !frozen;
	QuoteTable * pTable = theApp.GetCurrentTable();	
	QuoteTable * pTableTmp = theApp.GetTableTmp();
	if (freeze)
	{			
		if (pTable)
			*pTableTmp = *pTable;
	}
	if (my::wnd::IsValid(dlgGlass))
	{
		dlgGlass.SetTable(freeze ? pTableTmp : pTable);
		dlgGlass.SetFreeze(freeze);
		if (! freeze)
			dlgGlass.Update(NULL);
	}
	m_freeze = freeze;
}

BOOL CTradeDlg::InitDialog(CWindow wndFocus, LPARAM lInitParam)
{
	TRACE(__FUNCTION__": begin\r\n");

	DoDataExchange(FALSE);

	ShowWindow(SW_HIDE);

	theApp.SetMainWnd(*this);

	HICON hIconLittle = theApp.LoadSmallIcon(IDR_MAINFRAME);
	SetIcon(hIconLittle, FALSE);
	HICON hIconBig = theApp.LoadIcon(IDR_MAINFRAME);
	SetIcon(hIconBig, TRUE);

	DWORD verReg = theApp.GetVersionReg();
	if (verReg < VERSION_0_4_0)
	{
		theApp.ClearWindowPosition(CDlgAbout::GetName());
		theApp.ClearWindowPosition(CPropertySheetSettings::GetName());
		theApp.ClearWindowPosition(CDlgProperties::GetName());
	}

	InitListCtrl();

	ShowLog(0, HIDE);
#if USE_STATIC_MENU_MAIN && 1
	m_menu.LoadMenu(IDR_MENU_MAIN);
#endif

	HINSTANCE hInst = theApp.GetModuleInstance();
	int icons[] = {
		IDI_BUY_SMALL,
		IDI_SELL_SMALL,
	};
	for (int i = 0; i < SIZEOF_ARRAY(icons); i++)
		m_icons.hIcon[i] = CMyQuikApp::LoadIconEx(hInst, icons[i], m_icons.size.cx, m_icons.size.cy);

	m_hints.Create(*this);
	m_hints.AddTool (CToolInfo (TTF_SUBCLASS, GetDlgItem(IDC_BUTTON_BUY), 0, NULL, (LPTSTR)STRING_BUY));
	m_hints.AddTool (CToolInfo (TTF_SUBCLASS, GetDlgItem(IDC_BUTTON_SELL), 0, NULL, (LPTSTR)STRING_SELL));
	m_hints.AddTool (CToolInfo (TTF_SUBCLASS, GetDlgItem(IDC_BUTTON_CANCEL), 0, NULL, (LPTSTR)STRING_CANCEL_BID));
	m_hints.AddTool (CToolInfo (TTF_SUBCLASS, GetDlgItem(IDC_BUTTON_CANCEL_ALL), 0, NULL, (LPTSTR)STRING_CANCEL_ALL));
	m_hints.AddTool (CToolInfo (TTF_SUBCLASS, GetDlgItem(IDC_BUTTON_ZERO), 0, NULL, (LPTSTR)STRING_TRADE_ZERO));
	m_hints.AddTool (CToolInfo (TTF_SUBCLASS, GetDlgItem(IDC_BUTTON_REVERSE), 0, NULL, (LPTSTR)STRING_TRADE_REVERSE));
	m_hints.AddTool (CToolInfo (TTF_SUBCLASS, GetDlgItem(IDC_BUTTON_STOP), 0, NULL, (LPTSTR)STRING_STOP_ORDER_));

	m_pToolTipCtrl = new CToolTipCtrl;
	if (m_pToolTipCtrl->Create(*this))
	{
		m_listCtrl.SetToolTips(*m_pToolTipCtrl);
	}

	m_listCtrl.UpdateBkColor();

	EnableItems(TRUE);	

	BOOL autoCheck = theApp.GetSettings().common.quik.autoCheckConnection;
	m_quik.connected = !autoCheck;
	EnableTrading(!autoCheck);

	// Устанавливаем MODE_NORMAL:
	SetMode(MODE_ALTERNATIVE);
	ChangeMode();

	ShowGlass(0, HIDE);

	m_pHistory = new History(m_transCtrl.GetTransactions());
#if SHOW_HISTORY_ON_START
	ShowHistory(0, HIDE);
#endif

	CREATESTRUCT cs;
	if (S_OK == theApp.LoadWindowPosition(&cs, GetName()))
	{
		{
			MoveWindow(cs.x, cs.y, cs.cx, cs.cy, FALSE);
#if 0
			if (cs.style & WS_MAXIMIZE)
				ShowWindow(SW_MAXIMIZE);
#endif
		}
	}
	else
		CenterWindow();

	const Settings::Presentation & presentation = theApp.m_settings.presentation;
	UpdatePresentation(presentation, FALSE, TRUE);
	
	// Восстанавливаем окна:
#if SHOW_HISTORY_ON_START
	BOOL viewHistory = presentation.view.item[I_WINDOW_HISTORY].flags & F_SHOW_WINDOW;
	if (viewHistory)
		ShowHistory();
#endif
#if SHOW_STOP_ON_START
	BOOL viewStop = presentation.view.item[I_WINDOW_STOP].flags & F_SHOW_WINDOW;	
	if (viewStop)
		ShowDlgStop();
#endif
	BOOL viewLog = presentation.view.item[I_WINDOW_LOG].flags & F_SHOW_WINDOW;
	if (viewLog)
		ShowLog();
	BOOL viewGlass = presentation.view.item[I_WINDOW_GLASS].flags & F_SHOW_WINDOW;	
	if (viewGlass)
		ShowGlass();

#if 0
	PostMessage(UM_UPDATE, SW_MAXIMIZE);
#endif
	if (! this->IsHidden())
	{
		m_hActiveWnd = *this;
		ShowWindow(SW_SHOW);
	}
	else
	{
		if (! viewGlass)
			ShowGlass();
	}

	// Контроль транзакций:
	m_transCtrl.Init(*this, autoCheck);
#if 0
	// Создаём клиента для работы с DDE-сервером:
	theApp.CreateClient(this);
#else
	// Только после загрузки истории транзакций!
#endif
#if 0
	LoadTrade();
#else
	this->PostMessage(UM_LOAD_TRADE, LOAD_TRADE_RUN);
#endif	

	TRACE(__FUNCTION__": end\r\n");
	m_initialized = TRUE;

	return TRUE;
}

//
// CTradeDlg message handlers
//
BOOL CTradeDlg::OnInitDialog(CWindow wndFocus, LPARAM lInitParam)
{
	return InitDialog(wndFocus, lInitParam);
}

void CTradeDlg::Close()
{
	BOOL modify = theApp.GetModifySettings();
#if LOAD_TRADE_IN_SEPARATE_THREAD
	EndLoadTrade();
#endif
	m_transCtrl.Close();

	theApp.CloseClient();

	SaveListCtrlParams();

	int flags = my::lib::FWP_STYLE|my::lib::FWP_XY;
	if (theApp.GetSettings().presentation.main.show.wnd & F_MAIN_SHOW_LIST)
		flags |= my::lib::FWP_CXCY;
	theApp.SaveWindowPosition(*this, GetName(), flags);

	// Запоминаем положение окон:
	LPCTSTR path = TEXT("Workspace");

	Settings::Presentation & presentation = theApp.m_settings.presentation;
	
	DWORD showDeals, showGlass, showQuotes, showHistory, showMsgLog, showDlgStop;
	showDeals = showGlass = showQuotes = showHistory = showMsgLog = showDlgStop = 0;

	DWORD & flagsDeals = presentation.view.item[I_WINDOW_DEALS].flags;
	DWORD & flagsGlass = presentation.view.item[I_WINDOW_GLASS].flags;
	DWORD & flagsHistory = presentation.view.item[I_WINDOW_HISTORY].flags;
	DWORD & flagsLog = presentation.view.item[I_WINDOW_LOG].flags;
	DWORD & flagsStop = presentation.view.item[I_WINDOW_STOP].flags;

	showDeals = this->IsWindowVisible() ? F_SHOW_WINDOW : 0; 
	showDeals |= flagsDeals & F_SHOW_TITLE;	
	if (flagsDeals != showDeals)
	{
		flagsDeals = showDeals;
		modify = TRUE;
	}

	if (dlgGlass)
	{
		theApp.SaveWindowPosition(dlgGlass, dlgGlass.GetName());
		showGlass = dlgGlass.IsWindowVisible() ? F_SHOW_WINDOW : 0;		
	}
	showGlass |= flagsGlass & F_SHOW_TITLE;
	if (flagsGlass != showGlass)
	{
		flagsGlass = showGlass;
		modify = TRUE;
	}

	if (dlgHistory)
	{
		theApp.SaveWindowPosition(dlgHistory, dlgHistory.GetName());
		showHistory = dlgHistory.IsWindowVisible() ? F_SHOW_WINDOW : 0;		

		int width = dlgHistory.GetModifiedWidthTreeCtrl();
		if (width)
		{
			DEFINE_PATH_EX(text);
			_stprintf_s(text, SIZEOF_ARRAY(text) - 1, TEXT("%s\\%s"), path, dlgHistory.GetName());
			SetIntValue(theApp.GetRegKey(), width, text, TEXT("_tree_width"));
			modify = TRUE;
		}
	}
	showHistory |= flagsHistory & F_SHOW_TITLE;
	if (flagsHistory != showHistory)
	{
		flagsHistory = showHistory;
		modify = TRUE;
	}

	if (dlgLog)
	{
		theApp.SaveWindowPosition(dlgLog, dlgLog.GetName());
		showMsgLog = dlgLog.IsWindowVisible() ? F_SHOW_WINDOW : 0;
	}
	showMsgLog |= flagsLog & F_SHOW_TITLE;
	if (flagsLog != showMsgLog)
	{
		flagsLog = showMsgLog;
		modify = TRUE;
	}

	if (dlgStop)
	{
		showDlgStop = dlgStop.IsWindowVisible() ? F_SHOW_WINDOW : 0;
	}
	showDlgStop |= flagsStop & F_SHOW_TITLE;
	if (flagsStop != showDlgStop)
	{
		flagsStop = showDlgStop;
		modify = TRUE;
	}

	SaveTrade();

	theApp.SetModifySettings(NULL, modify);
}

void CTradeDlg::OnClose()
{
	this->Close();

	EndDialog(IDCANCEL);
}

void CTradeDlg::OnCancel(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	this->Close();

	EndDialog(nID);
}

void CTradeDlg::OnBnClickedButtonBuy(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	if (m_mode == MODE_NORMAL)
		OnBuy();
	else
		OnBuyMarket();
}

void CTradeDlg::OnBnClickedButtonSell(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	if (m_mode == MODE_NORMAL)
		OnSell();
	else
		OnSellMarket();
}

void CTradeDlg::OnBnClickedButtonStop(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	ShowDlgStop();
}

void CTradeDlg::OnBnClickedButtonCancel(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	CancelOrders((m_mode == MODE_NORMAL) ? CANCEL_ORDER : CANCEL_ORDERS_BUY);
}

void CTradeDlg::OnBnClickedButtonCancelAll(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	CancelOrders((m_mode == MODE_NORMAL) ? CANCEL_ORDERS_ALL : CANCEL_ORDERS_SELL);
}

void CTradeDlg::OnBnClickedButtonZero(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnTradeZero();
}

void CTradeDlg::OnBnClickedButtonReverse(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	OnTradeReverse();
}

void CTradeDlg::OnBuy() {BuySell(TRUE, OPERATION_ANY);}
void CTradeDlg::OnSell() {BuySell(FALSE, OPERATION_ANY);}
void CTradeDlg::OnBuyFixed() {BuySell(TRUE, OPERATION_FIXED);}
void CTradeDlg::OnSellFixed() {BuySell(FALSE, OPERATION_FIXED);}
void CTradeDlg::OnBuyMarket() {BuySell(TRUE, OPERATION_MARKET);}
void CTradeDlg::OnSellMarket() {BuySell(FALSE, OPERATION_MARKET);}
void CTradeDlg::OnBuyCurrent() {BuySell(TRUE, OPERATION_CURRENT);}
void CTradeDlg::OnSellCurrent() {BuySell(FALSE, OPERATION_CURRENT);}
void CTradeDlg::OnBuyTa() {BuySell(TRUE, OPERATION_FIXED|OPERATION_TA);}
void CTradeDlg::OnSellTa() {BuySell(FALSE, OPERATION_FIXED|OPERATION_TA);}

void CTradeDlg::OnStopBuy() {StopBid(TRUE, OPERATION_FIXED);}
void CTradeDlg::OnStopSell() {StopBid(FALSE, OPERATION_FIXED);}
void CTradeDlg::OnStopBuyOffset() {StopBid(TRUE, OPERATION_FIXED|OPERATION_OFFSET);}
void CTradeDlg::OnStopSellOffset() {StopBid(FALSE, OPERATION_FIXED|OPERATION_OFFSET);}
void CTradeDlg::OnStopBuyTa() {StopBid(TRUE, OPERATION_FIXED|OPERATION_TA);}
void CTradeDlg::OnStopSellTa() {StopBid(FALSE, OPERATION_FIXED|OPERATION_TA);}
void CTradeDlg::OnStopBuyOffsetTa() {StopBid(TRUE, OPERATION_FIXED|OPERATION_OFFSET|OPERATION_TA);}
void CTradeDlg::OnStopSellOffsetTa() {StopBid(FALSE, OPERATION_FIXED|OPERATION_OFFSET|OPERATION_TA);}

void CTradeDlg::OnStopBidTa() 
{
	const Transaction * pTa = GetSelectedTa();
	if (pTa)
	{
		int buy = (pTa->operation == Transaction::OPERATION_SELL) ? 1 : 0; // противоположное действие!
		StopBid(buy, 0, pTa->quantity, OPERATION_FIXED|OPERATION_OFFSET|OPERATION_TA);
	}
}

void CTradeDlg::OnChangeActiveBid() 
{
	const Transaction * pTa = GetSelectedTa();
	if (pTa && Transaction::IsActive(pTa->status))
	{
	}
}

void CTradeDlg::OnCancelOrder() {CancelOrders(CANCEL_ORDER);}
void CTradeDlg::OnCancelOrdersAll() {CancelOrders(CANCEL_ORDERS_ALL);}
void CTradeDlg::OnCancelOrdersBuy() {CancelOrders(CANCEL_ORDERS_BUY);}
void CTradeDlg::OnCancelOrdersSell() {CancelOrders(CANCEL_ORDERS_SELL);}
void CTradeDlg::OnCancelOrdersStop() {CancelOrders(CANCEL_ORDERS_STOP);}

void CTradeDlg::OnTradeZero()
{
	QuoteTable * pTable = theApp.GetCurrentTable();
	if (pTable)
	{
		Trade trade;
		if (S_OK == theApp.GetTrade(pTable->GetName(), trade))
		{
			if (trade.quantity > 0)
			{
				int buy = (trade.operation == Transaction::OPERATION_SELL) ? 1 : 0;				
				BuySell(buy, 0, trade.quantity, OPERATION_MARKET);
			}			
		}
	}
}

void CTradeDlg::OnTradeReverse()
{
	QuoteTable * pTable = theApp.GetCurrentTable();
	if (pTable)
	{
		Trade trade;
		if (S_OK == theApp.GetTrade(pTable->GetName(), trade))
		{
			if (trade.quantity > 0)
			{
				int buy = (trade.operation == Transaction::OPERATION_SELL) ? 1 : 0;				
				BuySell(buy, 0, trade.quantity * 2, OPERATION_MARKET);
			}			
		}
	}
}

void CTradeDlg::OnSize(UINT nType, CSize size)
{
#if 0
	if (! m_initialized)
		return;
#endif
	this->Resize(size.cx, size.cy);

	UpdateTextButtons();
#if 1
	if (this->IsHidden())
	{
		if (my::wnd::IsVisible(*this))
			ShowWindow(SW_HIDE);
		if (my::wnd::IsVisible(dlgGlass))
			::SetActiveWindow(dlgGlass);
	}
#endif
}

LRESULT CTradeDlg::OnLvnItemchangedList1(NMHDR *pNMHDR)
{
	LPNMLISTVIEW pNMLV = reinterpret_cast<LPNMLISTVIEW>(pNMHDR);
	if (pNMLV->uOldState == 0)
	{
#if 0
		if (pNMLV->uNewState & LVIS_SELECTED)
		{
			UpdateStateButtons(pNMLV->iItem);
		}
#else
		UpdateStateButtons();
		if (m_info)
			UpdateOSD();
#endif
	}
	return 0;
}

void CTradeDlg::OnEnChangeTradePrice(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateHintButtons();
}

void CTradeDlg::OnEnChangeTradeQuantity(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	if (my::wnd::IsValid(dlgGlass))
	{
		dlgGlass.SetQuantity(GetQuantity());
		dlgGlass.UpdateName();
	}
}

LRESULT CTradeDlg::OnDeltaposSpinTradePrice1(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);

	const QuoteTable::Properties * pProperties = theApp.GetCurrentTableProperty();
	m_edits.price.OnDeltaposSpinPrice(pNMUpDown, pProperties);

	return 0;
}

LRESULT CTradeDlg::OnDeltaposSpinTradeQuantity(NMHDR *pNMHDR)
{
	LPNMUPDOWN pNMUpDown = reinterpret_cast<LPNMUPDOWN>(pNMHDR);

	const QuoteTable::Properties * pProperties = theApp.GetCurrentTableProperty();
	m_edits.quantity.OnDeltaposSpinQuantity(pNMUpDown, pProperties);

	return 0;
}

LRESULT CTradeDlg::OnNMRClickList1(NMHDR *pNMHDR)
{
	LPNMITEMACTIVATE pNMItemActivate = reinterpret_cast<LPNMITEMACTIVATE>(pNMHDR);

	// Отображаем всплывающее меню:
	ShowMenuTrading();

	return 0;
}

LRESULT CTradeDlg::OnWndMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = TRUE;
	switch (uMsg)
	{
	case UM_RX_TABLE:
		{
#if 0
			TRACE("UM_RX_TABLE\r\n");
#endif			
			const BYTE * data = (BYTE*)wParam;
			if (data == NULL)
				break;
			const BYTE * pSrc = data;
			DWORD size;
			memcpy(&size, pSrc, sizeof(size)); pSrc += sizeof(size);
			int tableNameLength = *pSrc; pSrc += sizeof(BYTE);
			LPCTSTR tableName = (LPCTSTR)pSrc;
			const BYTE * pTableData = &pSrc[(tableNameLength + 1)*sizeof(TCHAR)];

			BOOL updateGlass = FALSE;
			
			int msg = static_cast<int>(lParam);

			switch (msg)
			{
			case UM_RX_TABLE_QUOTES:
				{
					// Формируем название котировки:
					LPCTSTR pTableName = tableName;
					for (int i = 0; i < lstrlen(tableName); i++)
					{
						if (tableName[i] == TEXT(']'))
						{
							pTableName += i + 1;
							break;
						}
					}					
					 // Обновляем котировки:
					IterateTableQuotes(pTableName, pTableData, size);
					updateGlass = TRUE;
				}
				break; // case UM_RX_TABLE_QUOTES:
			case UM_RX_TABLE_CURRENT:
				{
					 // Текущая таблица параметров:
					IterateTableCurrent(tableName, pTableData, size);
					updateGlass = TRUE;
				}
				break;
			case UM_RX_TABLE_BIDS:				
				{
					theApp.EnterCriticalSection();
					// Таблица заявок:
					IterateTableBids(tableName, pTableData, size);
					theApp.LeaveCriticalSection();
				}
				break;
			case UM_RX_TABLE_DEALS:
				{
					theApp.EnterCriticalSection();
					// Таблица сделок:
					IterateTableDeals(tableName, pTableData, size);
					theApp.LeaveCriticalSection();
				}
				break;
			case UM_RX_TABLE_STOPS:
				{				
					theApp.EnterCriticalSection();
					// Таблица стоп-заявок:
					IterateTableStops(tableName, pTableData, size);
					theApp.LeaveCriticalSection();
				}
				break;
			case UM_RX_TABLE_PORTFOLIO:
				{
					// Клиентский портфель:
					IterateTablePortfolio(tableName, pTableData, size);
				}
				break;
			case UM_RX_TABLE_LIMITS:
				{
					// Ограничения по клиентским счетам:
					IterateTableLimits(tableName, pTableData, size);
				}
				break;
			} // switch (msg)

			if (updateGlass)
			{
				if (my::wnd::IsValid(dlgGlass))
					dlgGlass.Update(NULL);
			}

			delete [] data;
		}
		break;
	case UM_RX_NEWDATA:
		{
			TRACE("UM_RX_NEWDATA\r\n");
			BYTE * data = (BYTE*)wParam;
			DWORD size = (DWORD)lParam;
			if (data != NULL)
			{
			} // if (data != NULL)
		}
		break;
	case UM_TRANSACTION:
		{
			TSTRING_STD2(str, size);
			Transaction * pTransaction = (Transaction*)wParam;
			int nd = 0;
			const QuoteTable::Properties * pProperties = (const QuoteTable::Properties*)lParam;
			if (pProperties == NULL)
			{
				const QuoteTable * pTable = theApp.GetCurrentTable();
				if (pTable != NULL)
					pProperties = pTable->GetProperties();
#if 0
				else
					pProperties = theApp.FindTableProperty(pTransaction->strSecCode, PROPERTY_SECCODE);
#endif
			}
			if (pProperties != NULL)
			{
				int updateParams = 1;
				if (updateParams)
				{
					SET_TRANSACTION_STRCODE(pTransaction->strName, pProperties->name);
					SET_TRANSACTION_STRCODE(pTransaction->strClassCode, pProperties->classcode);
					SET_TRANSACTION_STRCODE(pTransaction->strSecCode, pProperties->seccode);
					SET_TRANSACTION_STRCODE(pTransaction->strAccount, pProperties->account);
					SET_TRANSACTION_STRCODE(pTransaction->strClientCode, pProperties->clientcode);
				} // if (updateParams)
				nd = pProperties->price.nd;

				int ret;
				int ok = 1;
				BOOL checkAndCloseDlgStop = Transaction::IsActionStop(pTransaction->baseAction);
				BOOL cancel = FALSE;

				if (theApp.GetSettings().trading.confirmTransaction)
				{
					TSTRING_STD2(buf, bufSize);
					TCHAR * pDst = buf;
					int n = 0;

					LPCTSTR question;
					LPCTSTR taName = Transaction::ActionToStringRus(pTransaction->action);
					LPCTSTR name = pTransaction->strName;
					LPCTSTR className = pTransaction->strClassCode;
					LPCTSTR secName = pTransaction->strSecCode;
					LPCTSTR strOperation = (pTransaction->operation == Transaction::OPERATION_BUY) ? TEXT("Покупка") : TEXT("Продажа");

					if (Transaction::IsActionCancel(pTransaction->action))
					{
						if (theApp.GetSettings().trading.confirmCancel)
						{
							TSTRING_STD2(strQuestion, sizeStrQuestion);
							_stprintf_s(strQuestion, sizeStrQuestion, TEXT("Вы действительно желаете снять %sзаявку на %s"),
								Transaction::IsActionStop(pTransaction->baseAction) ? TEXT("стоп-") : TEXT(""),
								(pTransaction->operation == Transaction::OPERATION_BUY) ? TEXT("покупку") : TEXT("продажу"));
							n += _stprintf_s(pDst + n, SIZEOF_ARRAY(buf) - n, TEXT("%s\r\nномер %.0f, инструмент \"%s\"?\r\n"), 
								strQuestion, pTransaction->orderKey, pProperties->name); // secName
						}
					}
					else
					{
						question = TEXT("Вы действительно желаете выполнить транзакцию");
						
						TCHAR format[] = TEXT("%0.xf");
						format[3] = (nd % 10) + 0x30;

						n += _stprintf_s(pDst + n, bufSize - n, TEXT("%s?\r\nТип: %s\r\n"), question, taName);
						n += _stprintf_s(pDst + n, bufSize - n, TEXT("Класс: %s\r\nИнструмент: %s (%s)\r\n"), className, name, secName);
						n += _stprintf_s(pDst + n, bufSize - n, TEXT("Операция: %s\r\n"), strOperation);
						if (Transaction::IsActionStop(pTransaction->action))
						{
							n += _stprintf_s(pDst + n, bufSize - n, TEXT("Стоп-цена: "));
							n += _stprintf_s(pDst + n, bufSize - n, format, pTransaction->stop.price);
							n += _stprintf_s(pDst + n, bufSize - n, TEXT("\r\n"));
						}
						n += _stprintf_s(pDst + n, bufSize - n, TEXT("Цена: "));
						n += _stprintf_s(pDst + n, bufSize - n, format, pTransaction->price);
						n += _stprintf_s(pDst + n, bufSize - n, TEXT("\r\n"));
						n += _stprintf_s(pDst + n, bufSize - n, TEXT("Кол-во: %0.f\r\n"), pTransaction->quantity);
					}
					if (n > 0)
					{
						buf[n] = TEXT('\0');

						UINT mbType = MB_ICONQUESTION|MB_YESNOCANCEL;
						ret = MessageBox(buf, mbType);
						if (ret != IDYES)
						{
							ok = 0;
							// Заканчиваем цикл
							if (ret == IDCANCEL)
								cancel = TRUE;
							else if (ret == IDNO)
								checkAndCloseDlgStop = FALSE;
						}
					}
				} // if (appSettings.trading.confirmTransaction)
#if 1
				if (checkAndCloseDlgStop)
				{
					CWindow wnd = GetActiveWindow();
					if (wnd == dlgStop)
						::SendMessage(wnd, WM_CLOSE, 0, 0);
				}
#endif
				if (cancel)
					return -1;
				else if (ok)
				{					
					const QuoteTable::Properties * pProperties = theApp.FindTableProperty(pTransaction->strName, pTransaction->strSecCode, 
						pTransaction->strClassCode, pTransaction->strClientCode, pTransaction->strAccount);
					int newTransaction = 0;
					TSTRING_STD(buf);
					StrPrintTransactionParameters(pTransaction, buf, SIZEOF_ARRAY(buf), pProperties ? pProperties->price.nd : 0, 0);
					if (Transaction::IsActionCancel(pTransaction->action))
					{
						m_transCtrl.UpdateTransaction(pTransaction, updateParams);
						newTransaction = 0;
						_stprintf_s(str, SIZEOF_ARRAY(str), TEXT("Отмена заявки N %.0f (%s)"), pTransaction->orderKey, buf);
					}
					else
					{
						LPCTSTR name = (pProperties) ? pProperties->name : NULL;
						pTransaction = m_transCtrl.CreateNewTransaction(pTransaction, name, updateParams);
#if 0
						pTransaction->SetPending();
#endif
						newTransaction = 1;
						if (pTransaction->orderKey)
							_stprintf_s(str, size, TEXT("Заявка N %.0f (%s)"), pTransaction->orderKey, buf);
						else
							_stprintf_s(str, size, TEXT("Новая %sзаявка (%s)"), Transaction::IsActionStop(pTransaction->action) ? TEXT("стоп-") : TEXT(""), buf);
					}					
					LogMessage(str);
					int result;
					ret = m_transCtrl.DoTransaction(pTransaction, nd, 0x0, this, &result);
					if (ret == S_OK)
					{
						if (newTransaction)
						{
							this->AddTransaction(pTransaction, pProperties);
						}
					}
					else
						OnFailTransaction(result);
				}
			} // if (pProperties != NULL)
			else
			{
				_stprintf_s(str, size, TEXT("Неизвестный код инструмента: \"%s\"!"), pTransaction->strSecCode);
				LogMessage(str, CODE_USER_WARNING);
				MessageBox(str, MB_ICONWARNING|MB_OK);
			}
		}
		break;
	case UM_UPDATE_OSD:
		{
			//TRACE("UM_UPDATE_OSD\r\n");
			UpdateOSD();
		}
		break;
	case UM_UPDATE:
		{
			int show = static_cast<int>(wParam);
			ShowWindow(show);
		}
		break;
	case UM_TRANS2QUIK:
		{
			TRACE("UM_TRANS2QUIK\r\n");
			int result = static_cast<int>(wParam);

			BOOL wasConnected = m_quik.connected;
			BOOL connected = (result == S_OK);
			m_quik.connected = connected;
			if (theApp.GetSettings().common.quik.autoCheckConnection)
				EnableTrading(connected);

			if (result != S_OK)
			{
				CString str;
				LPCTSTR msg = TEXT("");
				DWORD mbType = MB_OK;
				USES_CONVERSION;
				if (result == -1)
				{
					msg = TEXT("Библиотека \"TRANS2QUIK.dll\" не найдена!");
					mbType |= MB_ICONERROR;
				}
				else
				{
					TRANS2QUIK::Error * pError = (TRANS2QUIK::Error*)lParam;
					msg = A2T(pError->text);						
#if 0
					str.Format(TEXT("QUIK сообщает: \"%s\""), msg);	
					msg = str;
#endif
					mbType |= MB_ICONWARNING;					
				}
				LogMessage(msg, result);
				if (wasConnected)
					LogMessage(TEXT("QUIK потерял соединение с сервером брокера"), CODE_USER_ERROR);
#if 0
				MessageBox(msg, mbType);
#else
				my::MessageBox(GetActiveWindow(), msg, mbType);
#endif
			}
			else
			{
				LogMessage(TEXT("QUIK установил соединение с сервером брокера"));
			}			
		} // case UM_TRANS2QUIK:
		break;
	case UM_TRANS2QUIK_EX:
		{
#if 0
			TRACE("UM_TRANS2QUIK_EX\r\n");
#endif
			Transaction::Answer * pAnswer = (Transaction::Answer*)wParam;
			Transaction * pTa = (Transaction*)lParam;
			
			USES_CONVERSION;
			LPCTSTR replyMessage = A2T(pAnswer->replyMessage);
			LogMessage(replyMessage, pAnswer->ext.callback ? pAnswer->replyCode : 0);

			pTa = m_transCtrl.OnAnswer(pAnswer, pTa);
			if (pTa != NULL)
			{
#if 1
				if (pAnswer->ext.callback && Transaction::IsFailed(pTa->status))
					if (theApp.m_settings.trading.flags & F_TRADING_PRINT_MESSAGE_ON_TRANSACTION_ERROR)
						MessageBox(replyMessage, MB_ICONWARNING|MB_OK);
#endif
				const QuoteTable::Properties * pProperties;
#if 0
				pProperies = theApp.GetCurrentTableProperty();
				if (pProperties == NULL)
#endif
					pProperties = theApp.FindTableProperty(pTa);
				if (pProperties != NULL)
				{
					if (S_OK == this->UpdateTransaction(pTa, pProperties))
					{
						if (pAnswer->orderNum > 0 && !pAnswer->ext.callback) // pAnswer->result != TRANS2QUIK_SUCCESS && 
						{
							if (S_OK == theApp.OnTradeAction(pProperties->name, pTa))
							{								
								if (my::wnd::IsValid(dlgGlass))
								{
									if (dlgGlass.IsUserTrade())
										SetUserTrade();
									else
										dlgGlass.Update(NULL);
								}
#if 1
								if (! IsLoading())
									CheckAndEnableTrade();
#endif
							}
						}
					}
				}
			}
			else
			{
				//Transaction::Status status = static_cast<Transaction::Status>(pAnswer->replyCode);
				if (pAnswer->ext.callback)// && Transaction::IsFailed(status))
					if (theApp.m_settings.trading.flags & F_TRADING_PRINT_MESSAGE_ON_TRANSACTION_ERROR)
						MessageBox(replyMessage, MB_ICONWARNING|MB_OK);
			}
			// Освобождаем память:
			delete pAnswer;
		}
		break;
	case UM_ACTION:
		{
			Action * pAction = reinterpret_cast<Action*>(wParam);
			HWND hWnd = reinterpret_cast<HWND>(lParam);
			return OnAction(pAction->type, pAction->flags, hWnd);
		}
		break;
	case UM_LOG:
		{
			LogMessage((LPCTSTR)wParam, static_cast<int>(lParam));
		}
		break;
	case UM_SHOW_DIALOG_STOP:
		{
			ShowDlgStop();
		}
		break;
	case UM_SHOW_PROPERTIES:
		{
			LPCTSTR name = NULL;
			const QuoteTable::Properties * pProperties = NULL;
			int flags = 0;
			ParamsShowProperties * params = reinterpret_cast<ParamsShowProperties*>(wParam);
			if (params != NULL)
			{
				name = params->name;
				pProperties = params->pProperties;
				flags = params->flags;
			}
			HWND hSrcWnd = reinterpret_cast<HWND>(lParam);
			BOOL result = ShowProperties(name, NULL, pProperties, flags);
			if (result == TRUE)
			{
				if (my::wnd::IsValid(hSrcWnd))
					::PostMessage(hSrcWnd, UM_UPDATE_INSTRUMENTS, 0, 0);
			}
			::SetFocus(hSrcWnd);
		}
		break;
	case UM_UPDATE_INSTRUMENTS:
		{
			int add = ((int)wParam > 0) ? TRUE : FALSE;
			int remove = ((int)wParam < 0) ? TRUE : FALSE;
			LPCTSTR name = (LPCTSTR)lParam;
			if (remove && name)
			{
				if (theApp.GetCurrentTable() == theApp.FindTable(name))
				{
					SelectInstrument((QuoteTable*)NULL);				
				}
			}
		}
		break;
	case UM_USER_TRADE:
		{
			//TRACE("UM_USER_TRADE\r\n");
			int userTrade = static_cast<int>(wParam);
			SetUserTrade(userTrade);
		}
		break;
	case UM_SERVER_FAULT:
		{
			TRACE("UM_SERVER_FAULT\r\n");
			CString msg;
			UINT mbType = MB_ICONERROR|MB_OK;
			msg.Format(TEXT("Сервер \"%s\" не отвечает!"), theApp.GetSettings().server.name);
			LogMessage(msg, CODE_ERROR);
#if 0
			MessageBox(msg, mbType);
#else
			my::MessageBox(GetActiveWindow(), msg, mbType);
#endif
		}
		break;
	case UM_APPLY_SETTINGS:
		{
			::Settings & appSettings = theApp.m_settings;

			BOOL modify = FALSE;
#if 0
			TRACE("UM_APPLY_SETTINGS\r\n");
#endif
			DWORD flags = (DWORD)wParam;
			if (flags & APPLY_SETTINGS_INSTRUMENT)
			{
#if defined _DEBUG && 1
				TRACE("APPLY_SETTINGS_INSTRUMENT\r\n");
#endif
				LPCTSTR name = (LPCTSTR)lParam;
				SelectInstrument(name, F_FORCE);
				modify = TRUE;
			}
			if (flags & APPLY_SETTINGS_COMMON)
			{
#if defined _DEBUG && 1
				TRACE("APPLY_SETTINGS_COMMON\r\n");
#endif
				const CPropertyPageCommon::Settings * pSettings = (const CPropertyPageCommon::Settings*)lParam;
				if (0 != memcmp(&appSettings.common, &pSettings->common, sizeof(pSettings->common)))
				{
					appSettings.common = pSettings->common;
					m_transCtrl.SetAutocheck(pSettings->common.quik.autoCheckConnection);
					if (pSettings->common.quik.autoCheckConnection == FALSE)
					{
						m_quik.connected = TRUE;
						EnableTrading(TRUE);
					}
					modify = TRUE;
				}
				if (0 != memcmp(&appSettings.log, &pSettings->log, sizeof(appSettings.log)))
				{
					appSettings.log = pSettings->log;
					modify = TRUE;
				}
			}
			if (flags & APPLY_SETTINGS_PRESENTATION_MAIN)
			{
#if defined _DEBUG && 1
				TRACE("APPLY_SETTINGS_PRESENTATION_MAIN\r\n");
#endif
				const ::Settings::Presentation * pPresentation = (const ::Settings::Presentation*)lParam;
				BOOL changedMain = (0 != memcmp(&pPresentation->main, &appSettings.presentation.main, sizeof(pPresentation->main)));
				BOOL changedView = (0 != memcmp(&pPresentation->view, &appSettings.presentation.view, sizeof(pPresentation->view)));
				if (changedMain || changedView)
				{
					Settings::Presentation::View view = appSettings.presentation.view;
					appSettings.presentation.view = pPresentation->view;
					if (changedMain || (view.item[I_WINDOW_DEALS].flags != pPresentation->view.item[I_WINDOW_DEALS].flags))
					{
						BOOL resize = (
							((appSettings.presentation.main.show.wnd ^ pPresentation->main.show.wnd) & F_MAIN_SHOW_LIST) || 
							((appSettings.presentation.main.show.wnd ^ pPresentation->main.show.wnd) & F_MAIN_SHOW_OSD) || 
							((appSettings.presentation.main.show.wnd ^ pPresentation->main.show.wnd) & F_MAIN_SHOW_CONTROLS)) ? TRUE : FALSE;
						appSettings.presentation.main = pPresentation->main;
						UpdatePresentation(*pPresentation, TRUE, resize);
						Resize(0, 0);
					}
					if (changedView)
					{				
						UpdateTransparence(*pPresentation);

						int i = I_WINDOW_GLASS;
						if (view.item[i].flags != pPresentation->view.item[i].flags)
							ShowGlass(pPresentation->view.item[i].flags & F_SHOW_WINDOW);
						i = I_WINDOW_HISTORY;
						if (view.item[i].flags != pPresentation->view.item[i].flags)
							ShowHistory(pPresentation->view.item[i].flags & F_SHOW_WINDOW);
						i = I_WINDOW_LOG;
						if (view.item[i].flags != pPresentation->view.item[i].flags)
							ShowLog(pPresentation->view.item[i].flags & F_SHOW_WINDOW);	
						i = I_WINDOW_STOP;
						if (view.item[i].flags != pPresentation->view.item[i].flags)
							//if (IsAllowedStop())
								ShowDlgStop(pPresentation->view.item[i].flags & F_SHOW_WINDOW);
					}
					modify = TRUE;
				}
			}
			if (flags & APPLY_SETTINGS_TRADING)
			{
#if defined _DEBUG && 1
				TRACE("APPLY_SETTINGS_TRADING\r\n");
#endif
				const Settings::Trading * pSettings = (const Settings::Trading*)lParam;
				appSettings.trading = *pSettings;
				modify = TRUE;
				UpdateHintButtons();
			}
			if (flags & APPLY_SETTINGS_PRESENTATION_GLASS)
			{
#if defined _DEBUG && 1
				TRACE("APPLY_SETTINGS_PRESENTATION_GLASS\r\n");
#endif
				const Settings::Presentation::Glass * pSettings = (const Settings::Presentation::Glass*)lParam;
				if (0 != memcmp(pSettings, &appSettings.presentation.glass, sizeof(*pSettings)))
				{
					if (my::wnd::IsValid(dlgGlass))
					{
						dlgGlass.SetSettings(*pSettings);
						if (dlgGlass.IsUserTrade() && 
							((pSettings->user.flags ^ appSettings.presentation.glass.user.flags) & F_USER_GROUP_BY_PRICE))
						{
							this->SetUserTrade(SET, pSettings);
						}
					}
					appSettings.presentation.glass = *pSettings;
					if (my::wnd::IsVisible(dlgGlass))
					{
						dlgGlass.Update(NULL);
					}
					modify = TRUE;
					UpdateOSD();
				}
			}
			if (flags & APPLY_SETTINGS_PRESENTATION_OSD)
			{
#if defined _DEBUG && 1
				TRACE("APPLY_SETTINGS_PRESENTATION_OSD\r\n");
#endif
				const Settings::Presentation::OSD * pSettings = (const Settings::Presentation::OSD*)lParam;
				if (0 != memcmp(&appSettings.presentation.osd, pSettings, sizeof(Settings::Presentation::OSD)))
				{
					appSettings.presentation.osd = *pSettings;
					UpdateOSD();
					modify = TRUE;
				}
			}
			if (flags & APPLY_SETTINGS_PRESENTATION_LIST)
			{
#if defined _DEBUG && 1
				TRACE("APPLY_SETTINGS_PRESENTATION_LIST\r\n");
#endif
				const Settings::Presentation::ListOfDeals * pSettings = (const Settings::Presentation::ListOfDeals*)lParam;				
				if (0 != memcmp(&pSettings->columns, &appSettings.presentation.list.columns, sizeof(pSettings->columns)))
				{					
					UpdateColumns(*pSettings, appSettings.presentation.list);
					UpdateColumnIndex();
					if (my::wnd::IsValid(dlgHistory))
					{
						dlgHistory.UpdateColumns(*pSettings, appSettings.presentation.list);
						dlgHistory.UpdateColumnIndex();
					}
					appSettings.presentation.list.columns = pSettings->columns;
					modify = TRUE;
				}
				if (0 != memcmp(&pSettings->colors, &appSettings.presentation.list.colors, sizeof(pSettings->colors)))
				{
					appSettings.presentation.list.colors = pSettings->colors;
					m_listCtrl.UpdateBkColor();
					m_listCtrl.Invalidate();
					if (my::wnd::IsValid(dlgHistory))
					{
						dlgHistory.UpdateBkColor();
						dlgHistory.Invalidate();
					}
					modify = TRUE;
				}
			}

			if (modify == TRUE)
				theApp.SetModifySettings(NULL, modify);
		}
		break;
	case UM_CLOSE_WINDOW:
		{
			DWORD id = (DWORD)wParam;
#if defined _DEBUG && 0
			TRACE("UM_CLOSE_WINDOW: id=%d\r\n", id);
#endif
			if (id == IDD_GLASS)
			{
				if (! IsHidden())
					ShowGlass(0, HIDE);
				else
					this->EndDialog(IDOK);
			}
			if (id == IDD_HISTORY)
				ShowHistory(0, HIDE);
			else if (id == IDD_LOG)
				ShowLog(0, HIDE);
		}
		break;
	case UM_OSD:
		{
			DWORD flags = (DWORD)lParam;
			m_osdCtrl.active = flags;
			UpdateOSD();
		}
		break;
	case UM_HISTORY:
		{
			ShowHistory(0, 0);
		}
		break;
	case UM_REMOVE_TRANSACTION:
		{
			const Transaction * pTa = (const Transaction*)wParam;
			int flags = static_cast<int>(lParam);
			RemoveTransaction(pTa, flags);
		}
		break;
	case UM_LOAD_TRADE:
		{
			int cmd = static_cast<int>(wParam);			
			switch (cmd)
			{
			case LOAD_TRADE_ON_BEGIN: 
				OnBeginLoadTrade();
				break;
			case LOAD_TRADE_ON_END:
				OnEndLoadTrade();	
				break;
			case LOAD_TRADE_RUN:				
				LoadTrade();				
				break;
			case LOAD_TRADE_DONE:
#if 1
				theApp.CreateClient(*this);

				CheckAndEnableTrade();
#endif
				break;
			} // switch (cmd)			
		}
		break;
	case UM_RESTORE:
		{
			int show;
		#if 0
			show = SW_RESTORE;
		#else
			show = (this->IsZoomed()) ? SW_SHOWMAXIMIZED : SW_RESTORE;
		#endif
			if (! IsHidden())
			{
				::SetForegroundWindow(*this);
				this->SetActiveWindow();
				this->ShowWindow(show);
			}
			else
			{// Главным является Стакан
			}
		}
		break;
	case UM_SHOW_WINDOW:
		{
			int show = static_cast<int>(wParam);
			ShowWindow(show);
		}
		break;
	case UM_MENU:
		{
			int what = static_cast<int>(wParam);
			if (what == F_SHOW_MENU)
			{
				CPoint point;
				GetCursorPos(&point);
				ScreenToClient(&point);
				this->SendMessage(WM_RBUTTONUP, 0, MAKELPARAM(LOWORD(point.x), LOWORD(point.y)));
			}
			else
			{
				int flags = what;
				CMenu * pMenu = (CMenu*)lParam;
				if (S_OK == MakeMainMenu(*pMenu, flags))
					return (LRESULT)pMenu;
			}
		}
		break;
	case UM_ACTIVE:
		{
#if defined _DEBUG && 0
			TRACE("UM_ACTIVE(0x%p, %d)\r\n", wParam, lParam);
#endif
			HWND hSrc = (HWND)wParam;
			int state = static_cast<int>(lParam);
#if USE_TRANSPARENCE
			int iWnd = -1;
			if (hSrc == *this)
				iWnd = I_WINDOW_DEALS;
			else if (hSrc == dlgGlass)
				iWnd = I_WINDOW_GLASS;
			else if (hSrc == dlgHistory)
				iWnd = I_WINDOW_HISTORY;
			else if (hSrc == dlgLog)
				iWnd = I_WINDOW_LOG;
			else if (hSrc == dlgStop)
				iWnd = I_WINDOW_STOP;
			if (iWnd >= 0)
			{
				const Settings::Presentation & presentation = theApp.m_settings.presentation;
				const Settings::Presentation::Window & window = presentation.view.item[iWnd];
				int opacity = -1;
				if (state == WA_INACTIVE)
					opacity = window.opacity.inactive;
				else
					opacity = window.opacity.active;
				if (opacity >= 0)
				{
					int alpha = 255 * opacity / 100;	
					my::wnd::SetOpacity(hSrc, alpha);
				}
#endif // #if USE_TRANSPARENCE
			}
		}
		break;
	default:
		bHandled = FALSE;
	} // switch (uMsg)

	return 0;
}

void CTradeDlg::OnSettings()
{
	Settings & settings = theApp.m_settings;

	if (dlgSettings == NULL)
	{
		dlgSettings = new CPropertySheetSettings();
		if (dlgSettings)
		{
			//CPropertySheetSettings dlg(TEXT("Настройки"));
			dlgSettings->Init(&settings, m_activePropertyPage);
			INT_PTR ret = dlgSettings->DoModal();
#if 0
			if (ret == IDOK)
#else
			// Может быть нажата кнопка "Применить", поэтому проверяем в любом случае.
#endif
			{
#if 0
				if (0 != memcmp(&theApp.m_settings, dlg.GetSettings(), sizeof(Settings)))
#else
				const Settings * pSettings = dlgSettings->GetSettings();
				if (0 != memcmp(&settings.common, &pSettings->common, sizeof(settings.common)) || 
					0 != memcmp(&settings.trading, &pSettings->trading, sizeof(settings.trading)) || 
					0 != memcmp(&settings.log, &pSettings->log, sizeof(settings.log)) || 
					0 != memcmp(&settings.server, &pSettings->server, sizeof(settings.server)) || 
					0 != memcmp(&settings.presentation, &pSettings->presentation, sizeof(settings.presentation)) || 
					settings.shortcuts.layouts[I_LAYOUT_CUSTOM] != pSettings->shortcuts.layouts[I_LAYOUT_CUSTOM] ||
					settings.shortcuts.iCurrentLayout != pSettings->shortcuts.iCurrentLayout)
#endif
				{
					theApp.SetModifySettings(pSettings);
				}
#if 0
				if (ret == IDOK)
#endif
					m_activePropertyPage = dlgSettings->GetLastIndex();
			}
			SAFE_DELETE(dlgSettings);
		}
	}
}

void CTradeDlg::OnGlass()
{
	if (! this->IsHidden())
	{
		ShowGlass(0, 0);
		theApp.SetModifySettings();
	}
}

void CTradeDlg::OnHistory()
{
	ShowHistory(0, 0);
	theApp.SetModifySettings();
}

void CTradeDlg::OnMsglog()
{
	ShowLog(0, 0);
	theApp.SetModifySettings();
}

void CTradeDlg::OnStop()
{
	ShowDlgStop();
#if 1
	theApp.SetModifySettings();
#endif
}

LRESULT CTradeDlg::OnCmdMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = FALSE;

	int nCode = HIWORD(wParam);
	if (nCode)
		return 0; // выход, если сообщение не от меню;

	UINT nID = LOWORD(wParam);
	if (nID >= ID_INSTRUMENT_XXX)
	{
		BOOL found = FALSE;
		int pos;
		CMenu menu0 = m_menu.GetSubMenu(0);
		CMenu menu = menu0.GetSubMenu(MMENU_INSTRUMENTS);
		pos = nID - ID_INSTRUMENT_XXX;
		if (menu)
		{
			TSTRING_STD2(name, size);
			menu.GetMenuString(pos, name, size, MF_BYPOSITION);
			menu0.Detach(); menu.Detach();
			ExtractInstrumentName(name, size);
			SelectInstrument(name);
		}		
		bHandled = TRUE;
	}
	else if (nID >= IDM_PRICE1 && nID <= IDM_PRICE10)
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
			QuoteTable * pTable = theApp.GetCurrentTable();
			if (pTable != NULL)
				KeepUserValues(pTable);
#endif
		}

		menu.Detach();
#endif // USE_STATIC_MENU_USER_VALUES
		bHandled = TRUE;
	}
	else
	{
		bHandled = TRUE;
		switch (nID)
		{
		case ID_BUY: OnBuy(); break;
		case ID_SELL: OnSell(); break;
		case ID_BUY_FIXED: OnBuyFixed(); break;
		case ID_SELL_FIXED: OnSellFixed(); break;
		case ID_BUY_MARKET: OnBuyMarket(); break;
		case ID_SELL_MARKET: OnSellMarket(); break;
		case ID_BUY_CURRENT: OnBuyCurrent(); break;
		case ID_SELL_CURRENT: OnSellCurrent(); break;
		case ID_BUY_TA: OnBuyTa(); break;
		case ID_SELL_TA: OnSellTa(); break;
		case ID_STOP_ORDER: OnStop(); break;
		case ID_STOP_BID_BUY: OnStopBuy(); break;
		case ID_STOP_BID_SELL: OnStopSell(); break;
		case ID_STOP_BID_BUY_OFFSET: OnStopBuyOffset(); break;
		case ID_STOP_BID_SELL_OFFSET: OnStopSellOffset(); break;
		case ID_STOP_BID_BUY_TA: OnStopBuyTa(); break;
		case ID_STOP_BID_SELL_TA: OnStopSellTa(); break;
		case ID_STOP_BID_BUY_OFFSET_TA: OnStopBuyOffsetTa(); break;
		case ID_STOP_BID_SELL_OFFSET_TA: OnStopSellOffsetTa(); break;
		case ID_STOP_BID_TA: OnStopBidTa(); break;
		case ID_CHANGE_ACTIVE_BID: OnChangeActiveBid(); break;			
		case ID_CANCEL_ORDER: OnCancelOrder(); break;
		case ID_CANCEL_ORDER_GLASS: CancelOrderGlass(); break;
		case ID_CANCEL_ALL_ORDERS: OnCancelOrdersAll(); break;
		case ID_CANCEL_ALL_ORDERS_BUY: OnCancelOrdersBuy(); break;
		case ID_CANCEL_ALL_ORDERS_SELL: OnCancelOrdersSell(); break;
		case ID_CANCEL_ALL_ORDERS_STOP: OnCancelOrdersStop(); break;
		case ID_TRADE_ZERO: OnTradeZero(); break;
		case ID_TRADE_REVERSE: OnTradeReverse(); break;

		case ID_INSTRUMENT_PREVIOS: OnInstrumentPrevios(); break;
		case ID_INSTRUMENT_NEXT: OnInstrumentNext(); break;

		case ID_TRANSACTION_INFO: OnTransactionInfo(); break;

		case ID_USER_TRADE: OnUserTrade(); 
			break;
		case ID_GLASS_STYLE_1:
		case ID_GLASS_STYLE_2:
		case ID_GLASS_STYLE_3:
		case ID_GLASS_BUY_ABOVE:
		case ID_GLASS_FLIP_HORIZONTAL:
		case ID_GLASS_RAREFY:
		case ID_GLASS_NEUTRAL_ZONE:
		case ID_GLASS_GRID_LINES_VERTICAL:
		case ID_GLASS_GRID_LINES_HORIZONTAL:
		case ID_GLASS_BORDER:
		case ID_GLASS_BORDER_LINES_BORDERS:
		case ID_GLASS_BORDER_LINES_MEDIAN:
		case ID_GLASS_MARKER:
		case ID_GLASS_MARKER_FOLLOW_PRICE:
		case ID_GLASS_MARKER_FOLLOW_CURSOR:
		case ID_GLASS_MARKER_FOLLOW_MENU:
		case ID_USER_SHOW_ACTIVE_BIDS:
		case ID_USER_SHOW_ACTIVE_STOPS:
		case ID_USER_SHOW_ACTIVE_DEALS:
		case ID_USER_SHOW_ICONS:
		case ID_GLASS_USER_SEPARATE_COLUMN:
		case ID_GLASS_USER_GROUP_PRICE:
			OnViewGlass(nID);
			break;

		case ID_SHOW_MAIN: OnShowMain(); break;
		case ID_SHOW_GLASS: OnGlass(); break;
		case ID_SHOW_HISTORY: OnHistory(); break;
		case ID_SHOW_MSGLOG: OnMsglog(); break;

		case ID_SHOW_LISTOFDEALS: OnShowListOfDeals(); break;
		case ID_SHOW_OSD: OnShowOsd(); break;
		case ID_SHOW_CONTROLS: OnShowControls(); break;

		case ID_SET_BASE: OnSetBase(); break;
		case ID_SET_EXTENDED: OnSetExtended(); break;

		case ID_SHOW_TOPMOST: OnTopmost(); break;

		case ID_SETTINGS: OnSettings(); break;

		case ID_ABOUT: OnAbout(); break;

#ifdef _DEBUG
#endif // _DEBUG

		case ID_PROPERTIES: 
			ShowProperties(); 
			break;

		default:
			bHandled = FALSE;
		} // switch (nID)
	}

	return 0;
}

void CreateFont(CDC * pDC, CFont ** ppFont, CHARFORMAT & format)
{
	BOOL bold = (format.dwEffects & CFE_BOLD) ? TRUE : FALSE;
	BOOL italic = (format.dwEffects & CFE_ITALIC) ? TRUE : FALSE;
	int pts = format.yHeight / 20;
	int lfHeight = -MulDiv(pts, pDC->GetDeviceCaps(LOGPIXELSY), 72);
	(*ppFont)->CreateFont(lfHeight, 0, 0, 0, bold ? FW_BOLD : FW_NORMAL, italic, FALSE, 0, DEFAULT_CHARSET, 
		OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH|FF_SWISS,
		format.szFaceName);	
}

void CTradeDlg::OnPaint(CDCHandle )
{
	CPaintDC dc(*this);

	CWindow wnd;
	CDC * pDC;
	CRect rect;

	double price, curPrice, priceMin, priceMax, prevPrice;
	double demand, supply;
	double percent;
	double limit, limitPrev, profit;

	curPrice = price = priceMin = priceMax = prevPrice = 0;
	demand = supply = 0;
	percent = 0;
	limit = limitPrev = profit = 0;

	Trade trade;
	Trader::ResetTrade(trade);

	Account acnt;
	const Account * pAccount = NULL;
	QuoteTable table;
	const QuoteTable::Properties * pProperties = NULL;
	const QuoteTable * pTable = theApp.GetCurrentTable();
	if (pTable != NULL)
	{
		curPrice = price = pTable->GetCurrentPrice();
		prevPrice = pTable->GetPreviosPrice();
		if (prevPrice == price)
			prevPrice = pTable->GetPreviosChangePrice();
		pTable->GetMinMax(priceMin, priceMax);
		pTable->GetDemandAndSupply(demand, supply);
		percent = pTable->GetCurrentPercent();
		pProperties = pTable->GetProperties();

		pAccount = pTable->GetAccount();
		if (pAccount != NULL)
			pAccount->Get(limit, limitPrev, profit);

		theApp.GetTrade(pTable->GetName(), trade, curPrice);
	} // if (pTable != NULL)

	if (m_osdCtrl.active)
	{
#if defined _DEBUG && 0
		priceMin = 138520; priceMax = 141030; price = 138500;
#endif
		if (price == 0) price = 1234;
		if (priceMin == 0) priceMin = 1000;
		if (priceMax == 0) priceMax = 1500;	
		if (percent == 0) percent = 5.67;
		if (supply == 0) supply = 50;
		if (demand == 0) demand = 50;
		if (limit == 0) limit = 999999;
		if (limitPrev == 0) limitPrev = limit;
		if (profit == 0) profit = 1;		

		if (pTable == NULL)
		{
			table.Create(TEXT("Инструмент"), TEXT("DEF02"), TEXT("CLASSXXX"), NULL, NULL, 0);
			pTable = &table;
			pProperties = pTable->GetProperties();
		}
		if (pAccount == NULL)
		{
			memset(&acnt, 0, sizeof(acnt));
			pAccount = &acnt;
		}
		curPrice = price;
		theApp.GetTrade(pTable->GetName(), trade, curPrice);
	} // if (m_osdCtrl.active)

	wnd = GetDlgItem(IDC_OSD);
	Settings::Presentation::OSD & settings = theApp.m_settings.presentation.osd;

	CFont * pFont;
	TCHAR text[1024];
	TCHAR format[] = TEXT("%0.xf");
	int n;

	size_t textSize = SIZEOF_ARRAY(text);

	CSize size;
	int x0, y0, x, y;
	int yMax, yMin;
	int cx, cy;
	int cxMax, cxMin, cyMax;

	CFont font;

	wnd.GetClientRect(&rect);

	CDC wndDC = wnd.GetDC();

	CDC _dc;
	_dc.CreateCompatibleDC(wndDC);

	CBitmap bitmap;
	bitmap.CreateCompatibleBitmap(wndDC, rect.Width() + 1, rect.Height() + 1);
	_dc.SelectBitmap(bitmap);

	pDC = &_dc;//

	CBrush brush;
	brush.CreateSolidBrush(settings.backgrnd.color);	
#if 0
	rect.DeflateRect(1, 1);
	// Закрашиваем область:
	pDC->FillRect(&rect, brush);
#else
	if (theApp.m_settings.presentation.main.show.frame & F_MAIN_SHOW_OSD)
	{
		CPen pen;
		pen.CreatePen(PS_SOLID, 1, ::GetSysColor(COLOR_BTNSHADOW));
		pDC->SelectPen(pen);
		pDC->SelectBrush(brush);
		pDC->Rectangle(&rect);
	}
	else
		pDC->FillRect(&rect, brush);
#endif		

	CPen penFrame;
	penFrame.CreatePen(PS_SOLID, 1, settings.frame.cf.crTextColor);

	pDC->SetBkMode(TRANSPARENT);

	int nd = (pProperties) ? pProperties->price.nd : 0;
	format[3] = (nd % 10) + 0x30;

	if (m_selectInstrument || (m_osdCtrl.active & (OSD_INFO_INSTRUMENT_NAME|OSD_INFO_INSTRUMENT_CODE)))
	{
		if (pTable != NULL || m_osdCtrl.active)
		{
			CFont fontName, fontSeccode, fnt, fontArrow;

			QuoteTable * pTablePrev = theApp.GetPreviosTable();
			QuoteTable * pTableNext = theApp.GetNextTable();
			QuoteTable tablePrev, tableNext;
			if (m_osdCtrl.active)
			{
				if (pTablePrev == NULL)
				{
					tablePrev.Create(TEXT("Предыдущий"), TEXT("ABC01"), TEXT("CLASSXXX"));
					pTablePrev = &tablePrev;
				}
				if (pTableNext == NULL)
				{
					tableNext.Create(TEXT("Следующий"), TEXT("GHI03"), TEXT("CLASSXXX"));
					pTableNext = &tableNext;
				}
			} // if (m_osdCtrl.active)

			CHARFORMAT cf = settings.foreground.cf;
			cf.yHeight = 10 * 20;
			pFont = &fontArrow;
			CreateFont(pDC, &pFont, cf);
			pDC->SelectFont(*pFont);
			n = _stprintf_s(text, textSize, TEXT("_"));
			pDC->GetTextExtent(text, n, &size);
			int cx = size.cx;

			int spacing = size.cy;
			int border = size.cy/2;
			y = (rect.Height() - size.cy) / 2;
			// Стрелка влево:
			x = border;
			pDC->SetTextColor(settings.foreground.cf.crTextColor);
			n = _stprintf_s(text, textSize, TEXT("<"));
			pDC->ExtTextOut(x, y, ETO_CLIPPED, &rect, text, n, NULL);
			// Стрелка вправо:
			n = _stprintf_s(text, textSize, TEXT(">"));
			pDC->GetTextExtent(text, n, &size);
			x = rect.Width() - border - size.cx + 2;
			pDC->SetTextColor(settings.foreground.cf.crTextColor);				
			pDC->ExtTextOut(x, y, ETO_CLIPPED, &rect, text, n, NULL);
				
			CHARFORMAT cf2 = settings.foreground.cf;
			cf2.yHeight = ((cf2.yHeight / 20) + 2) * 20;
			pFont = &fnt;
			CreateFont(pDC, &pFont, cf2);

			pFont = &font;
			CreateFont(pDC, &pFont, settings.foreground.cf);
			pDC->SelectFont(*pFont);

			if (pTablePrev != NULL)
			{
				pProperties = pTablePrev->GetProperties();
				n = _stprintf_s(text, textSize, TEXT("%s"), pProperties->name);
				pDC->SelectFont(fnt);
				pDC->GetTextExtent(text, n, &size);

				spacing = size.cy;
				x = cx + (rect.Width() * 1/3 - (size.cx)) / 2;
				y = (rect.Height() - size.cy - spacing) / 2;

				pDC->SetTextColor(settings.foreground.cf.crTextColor);
				pDC->ExtTextOut(x, y, ETO_CLIPPED, &rect, text, n, NULL);

				n = _stprintf_s(text, textSize, TEXT("%s"), pProperties->seccode);
				pDC->SelectFont(font);
				pDC->GetTextExtent(text, n, &size);

				x = cx + (rect.Width() * 1/3 - (size.cx)) / 2;
				y += spacing;

				pDC->SetTextColor(settings.foreground.cf.crTextColor);
				pDC->ExtTextOut(x, y, ETO_CLIPPED, &rect, text, n, NULL);
			} // if (pTablePrev != NULL)

			if (pTableNext != NULL)
			{
				pProperties = pTableNext->GetProperties();
				n = _stprintf_s(text, textSize, TEXT("%s"), pProperties->name);
				pDC->SelectFont(fnt);
				pDC->GetTextExtent(text, n, &size);

				spacing = size.cy;
				x = rect.Width() * 2/3 - cx + (rect.Width() * 1/3 - (size.cx)) / 2;
				y = (rect.Height() - size.cy - spacing) / 2;

				pDC->SetTextColor(settings.foreground.cf.crTextColor);
				pDC->ExtTextOut(x, y, ETO_CLIPPED, &rect, text, n, NULL);

				n = _stprintf_s(text, textSize, TEXT("%s"), pProperties->seccode);
				pDC->SelectFont(font);
				pDC->GetTextExtent(text, n, &size);

				x = rect.Width() * 2/3 - cx + (rect.Width()*1/3 - (size.cx)) / 2;
				y += spacing;

				pDC->SetTextColor(settings.foreground.cf.crTextColor);
				pDC->ExtTextOut(x, y, ETO_CLIPPED, &rect, text, n, NULL);
			} // if (pTableNext != NULL)

			pProperties = pTable->GetProperties();
			n = _stprintf_s(text, textSize, TEXT("%s"), pProperties->name);
			pFont = &fontName;
			CreateFont(pDC, &pFont, settings.instrument.cf);
			pDC->SelectFont(*pFont);

			pDC->GetTextExtent(text, n, &size);
				
			spacing = size.cy;
			x = (rect.Width() - (size.cx)) / 2;
			y = ((rect.Height() - size.cy - spacing)) / 2;

			int X = x + settings.instrument.offset.cx;
			int Y = y + settings.instrument.offset.cy;
			if (m_osdCtrl.active & OSD_INFO_INSTRUMENT_NAME)
			{
				pDC->SelectPen(penFrame);
				pDC->Rectangle(X - 2, Y + 1, X + 2 + size.cx, Y + size.cy);
			}
			pDC->SetTextColor(settings.seccode.cf.crTextColor);
			pDC->ExtTextOut(X, Y, ETO_CLIPPED, &rect, text, n, NULL);

			n = _stprintf_s(text, textSize, TEXT("%s"), pProperties->seccode);
			if (pProperties->classcode[0] != '\0')
				n += _stprintf_s(text + n, textSize - n, TEXT(" / %s"), pProperties->classcode);
#if 0
			if (pProperties->account[0] != '\0')
				n += _stprintf_s(text + n, textSize - n, TEXT(" / %s"), pProperties->account);
#endif
			pFont = &fontSeccode;
			CreateFont(pDC, &pFont, settings.seccode.cf);
			pDC->SelectFont(*pFont);

			y += 0 + spacing; // size.cy
#if 0
			x = x + size.cx;
			pDC->GetTextExtent(text, n, &size);
			x -= size.cx;
#else
			pDC->GetTextExtent(text, n, &size);
			x = (rect.Width() - (size.cx)) / 2;
#endif
			X = x + settings.seccode.offset.cx;
			Y = y + settings.seccode.offset.cy;
			if (m_osdCtrl.active & OSD_INFO_INSTRUMENT_CODE)
			{
				pDC->SelectPen(penFrame);
				pDC->Rectangle(X - 2, Y + 1, X + 2 + size.cx, Y + size.cy);
			}
			pDC->SetTextColor(settings.seccode.cf.crTextColor);
			pDC->ExtTextOut(X, Y, ETO_CLIPPED, &rect, text, n, NULL);
		}
	}
	else if ((m_info == INFO_TRANSACTION || (m_osdCtrl.active & (OSD_INFO_TRANSACTIONS))) && 
		!(m_osdCtrl.active & (~OSD_INFO_TRANSACTIONS)))
	{// Режим просмотра информации о транзакциях
		int iIcon = -1;

		pFont = &font;
		CreateFont(pDC, &pFont, settings.foreground.cf);
		pDC->SelectFont(*pFont);		

		int iItem = my::ctrl::GetFocusedItem(m_listCtrl);
		if (iItem >= 0)
		{
			const Transaction * pTa = (const Transaction*)m_listCtrl.GetItemData(iItem);
			if (pTa != NULL)
			{
				n = MakeDescription(pTa, text, textSize - 1, nd);

				CFont fontTransaction;
				pFont = &fontTransaction;
				CreateFont(pDC, &pFont, settings.transaction.cf);
				pDC->SelectFont(*pFont);

				pDC->GetTextExtent(text, n, &size);
				
				x = (rect.Width() - size.cx) / 2;
				y = (rect.Height() - size.cy) / 2;

				iIcon = CMyTradeListCtrl::GetIconIndex(pTa->baseAction, pTa->status, pTa->operation);
			} // if (pTa != NULL)
		}
		else
		{
			n = _stprintf_s(text, textSize, TEXT("Режим просмотра иформации о транзакциях"));
			pDC->GetTextExtent(text, n, &size);
			
			x = (rect.Width() - size.cx) / 2;
			y = (rect.Height() - size.cy) / 2;
		}
		int X = x + settings.transaction.offset.cx;
		int Y = y + settings.transaction.offset.cy;
		if (m_osdCtrl.active & OSD_INFO_TRANSACTIONS)
		{
			pDC->SelectPen(penFrame);
			pDC->Rectangle(X - 3, Y - 1, X + 2 + size.cx, Y + 2 + size.cy);
		}
		pDC->SetTextColor(settings.transaction.cf.crTextColor);
		pDC->ExtTextOut(X, Y, ETO_CLIPPED, &rect, text, n, NULL);
#if 0
		if (iIcon >= 0)
		{
			int xIcon, yIcon;
			int cxIcon = 16;
			int cyIcon = 9;
			xIcon = X - cxIcon; yIcon = Y - 1;
			CImageList imgList = m_listCtrl.GetImageList(LVSIL_SMALL);
			HICON hIcon = imgList.GetIcon(iIcon);
			::DrawIconEx(*pDC, xIcon, yIcon, hIcon, cxIcon, cxIcon, 0, NULL, DI_NORMAL);
		}
#endif
	} // if (m_info == INFO_TRANSACTION)
	else if (pTable != NULL || m_osdCtrl.active)
	{
		const COLORREF colorBuy = theApp.m_settings.presentation.glass.items[GLASS_ITEM_BUY].backgrnd.color;
		const COLORREF colorSell = theApp.m_settings.presentation.glass.items[GLASS_ITEM_SELL].backgrnd.color;
		
		// Текст:
		CFont fontPrice, fontPriceMin, fontPriceMax, fontPercent;
		CFont fontLimit, fontIncome;
		CFont fontTradeBasic, fontTradeProfit, fontTradeLast;

		COLORREF colorGreen = RGB(0, 200, 0);
		COLORREF colorRed = RGB(200, 0, 0);
		COLORREF colorPrice;
		if (price > prevPrice)
			colorPrice = colorGreen;//colorBuy;
		else if (price < prevPrice)
			colorPrice = colorRed;//colorSell;
		else 
			colorPrice = settings.prices.current.cf.crTextColor;

		// Текущая цена:
		n = _stprintf_s(text, textSize, format, price);
		if (settings.prices.current.groupDigits)
			n = my::str::GroupDigits(text, n, text, textSize);

		pFont = &fontPrice;
		CreateFont(pDC, &pFont, settings.prices.current.cf);
		pDC->SelectFont(*pFont);

		pDC->GetTextExtent(text, n, &size);

		int border = 10;//size.cy/2;
#if 0
		x0 = x = (rect.Width() / 4 - size.cx) / 2;
		y0 = y = (rect.Height() - size.cy) / 2;
#else
		x0 = x = 0 + border;
		y0 = y = rect.top + 1;
#endif
		cx = size.cx; cy = size.cy;
		if (price != 0)
		{
			int X = x + settings.prices.current.offset.cx;
			int Y = y + settings.prices.current.offset.cy;
			if (m_osdCtrl.active & OSD_PRICE_CURRENT)
			{
				pDC->SelectPen(penFrame);
				pDC->Rectangle(X - 2, Y + 1, X + 2 + size.cx, Y + size.cy);
			}
			pDC->SetTextColor(settings.prices.current.cf.crTextColor);
			pDC->ExtTextOut(X, Y, ETO_CLIPPED, &rect, text, n, NULL);
		}
		
		int rightPriceMax = 0;

		// Максимальная цена:
		n = _stprintf_s(text, textSize, format, priceMax);
		if (settings.prices.maximum.groupDigits)
			n = my::str::GroupDigits(text, n, text, textSize);

		pFont = &fontPriceMax;
		CreateFont(pDC, &pFont, settings.prices.maximum.cf);
		pDC->SelectFont(*pFont);		
		
		pDC->GetTextExtent(text, n, &size);

		x = x0 + 1;
#if 0
		y = y0 + cy + 2;
#else
		y += cy;
#endif
		yMax = y; cxMax = size.cx; cyMax = size.cy;
		rightPriceMax = x + cxMax;
		if (price != 0)
		{
			int X = x + settings.prices.maximum.offset.cx;
			int Y = y + settings.prices.maximum.offset.cy;
			if (m_osdCtrl.active & OSD_PRICE_MAXIMUM)
			{
				pDC->SelectPen(penFrame);
				pDC->Rectangle(X - 2, Y + 1, X + 2 + size.cx, Y + size.cy);
			}
			pDC->SetTextColor(settings.prices.maximum.cf.crTextColor);
			pDC->ExtTextOut(X, Y, ETO_CLIPPED, &rect, text, n, NULL);
		}

		// Минимальная цена:
		n = _stprintf_s(text, textSize, format, priceMin);
		if (settings.prices.minimum.groupDigits)
			n = my::str::GroupDigits(text, n, text, textSize);

		pFont = &fontPriceMin;
		CreateFont(pDC, &pFont, settings.prices.minimum.cf);
		pDC->SelectFont(*pFont);		
#if 0
		y = y0 - size.cy - 2;
#else
		y += size.cy - 1;
#endif
		pDC->GetTextExtent(text, n, &size);
#if 0
		x = x0 + 1;
#else
		x = rightPriceMax - size.cx;
#endif
		yMin = y; cxMin = size.cx;
		if (price != 0)
		{
			int X = x + settings.prices.minimum.offset.cx;
			int Y = y + settings.prices.minimum.offset.cy;
			if (m_osdCtrl.active & OSD_PRICE_MINIMUM)
			{
				pDC->SelectPen(penFrame);
				pDC->Rectangle(X - 2, Y + 1, X + 2 + size.cx, Y + size.cy);
			}
			pDC->SetTextColor(settings.prices.minimum.cf.crTextColor);
			pDC->ExtTextOut(X, Y, ETO_CLIPPED, &rect, text, n, NULL);				
		}

		if (price != 0 && priceMax != 0 && priceMin != 0)
		{// Рисуем индикатор текущей цены:			
			x += 6 + max(cxMax, cxMin);
			y = yMax + settings.prices.maximum.offset.cy;
			int X = x + settings.indicators.price.offset.cx;
			int Y = y + settings.indicators.price.offset.cy;
			int y1 = Y + 2; int y2 = yMin + settings.prices.minimum.offset.cy + size.cy - 1;
			if (m_osdCtrl.active & OSD_INDICATOR_PRICE)
			{
				CRect rect(X - 2, y1, X + 3, y2);
				pDC->SelectPen(penFrame);
				pDC->Rectangle(rect.left - 2, rect.top - 3, rect.left + 2 + rect.Width(), rect.top + 3 + rect.Height());
			}
			CPen pen;
			pen.CreatePen(PS_SOLID, 1, colorPrice);//settings.prices.minimum.cf.crTextColor);
			pDC->SelectPen(pen);
			pDC->MoveTo(X, y1);
			pDC->LineTo(X, y2);
			// Маркер:
			int y3 = (int)(0.5 + (double)(y2 - y1)*(price - priceMin)/(priceMax - priceMin));
			pDC->MoveTo(X - 2, y2 - y3);
			pDC->LineTo(X + 2 + 1, y2 - y3);
		}			

		// Процент изменения:
		n = _stprintf_s(text, textSize, TEXT("%0.2f%s"), percent, TEXT("%"));
		if (settings.percent.groupDigits)
			n = my::str::GroupDigits(text, n, text, textSize);

		pFont = &fontPercent;
		CreateFont(pDC, &pFont, settings.percent.cf);
		pDC->SelectFont(*pFont);

		pDC->GetTextExtent(text, n, &size);
		
		x = x0 + cx + 1*cy;
#if 0
		y = y0;
#else
		y = y0 + (cy - size.cy) / 2;
#endif
		if (price != 0)
		{
			int X = x + settings.percent.offset.cx;
			int Y = y + settings.percent.offset.cy;
			if (m_osdCtrl.active & OSD_PERCENT)
			{
				pDC->SelectPen(penFrame);
				pDC->Rectangle(X - 2, Y + 1, X + 2 + size.cx, Y + size.cy);
			}
			pDC->SetTextColor(settings.percent.cf.crTextColor);
			pDC->ExtTextOut(X, Y, ETO_CLIPPED, &rect, text, n, NULL);		
		}

		if (demand > 0 || supply > 0)
		{// Спрос/предложение (индикатор):
			int width = 50;
			CRect rect(x, yMax + cyMax - 2, x + width, yMax + cyMax - 2 + 6);
			rect.OffsetRect(settings.indicators.demandSupply.offset.cx, settings.indicators.demandSupply.offset.cy);
			CPen pen;

			if (m_osdCtrl.active & OSD_INDICATOR_DEMANDSUPPLY)
			{
				pDC->SelectPen(penFrame);
				pDC->Rectangle(rect.left - 2, rect.top - 3, rect.left + 2 + rect.Width(), rect.top + 3 + rect.Height());
			}

			pen.CreatePen(PS_SOLID, 1, settings.foreground.cf.crTextColor);
			pDC->SelectPen(pen);
			pDC->Rectangle(&rect);

			// Маркер:
			x = rect.left;
			x += (int)(0.5 + (double)(width)*(demand)/(demand + supply));
			y = rect.top;
			pDC->MoveTo(x, y - 1);
			pDC->LineTo(x, y + 1 + rect.Height());

			CRect rectBuy(rect.left + 1 + 1, rect.top + 1 + 1, x, rect.bottom - 1 - 1);
			CBrush brushBuy;
			brushBuy.CreateSolidBrush(colorBuy);
			if (rectBuy.Width() > 0)
				pDC->FillRect(&rectBuy, brushBuy);

			CRect rectSell(x + 1, rect.top + 1 + 1, rect.right - 1 - 1, rect.bottom - 1 - 1);
			CBrush brushSell;
			brushSell.CreateSolidBrush(colorSell);
			if (rectSell.Width() > 0)
				pDC->FillRect(&rectSell, brushSell);
		}

		if (trade.quantity > 0 || trade.last.valid || (m_osdCtrl.active & (OSD_TRADE_PRICE|OSD_TRADE_PROFIT|OSD_TRADE_LAST)))
		{// Трэйд:
			TxtFormat & fmt1 = settings.trade.basic;
			TxtFormat & fmt2 = settings.trade.profit;

			int cxIcon = 9; int cyIcon = 9;

			if ((trade.last.valid && trade.quantity == 0) || (m_osdCtrl.active & OSD_TRADE_LAST))
			{// Предыдущий трэйд:
				n = _stprintf_s(text, textSize, format, trade.last.profit);
				if (settings.trade.last.groupDigits)
					n = my::str::GroupDigits(text, n, text, textSize);

				pFont = &fontTradeLast;
				CreateFont(pDC, &pFont, settings.trade.last.cf);
				pDC->SelectFont(*pFont);

				pDC->GetTextExtent(text, n, &size);
				x = (rect.Width() - (size.cx - cxIcon)) / 2;
				y = ((rect.Height() - size.cy)*2/3) / 2;

				int X = x + settings.trade.last.offset.cx;
				int Y = y + settings.trade.last.offset.cy;
				if (m_osdCtrl.active & OSD_TRADE_LAST)
				{
					pDC->SelectPen(penFrame);
					pDC->Rectangle(X - 2, Y + 1, X + 2 + size.cx, Y + size.cy);
				}
				pDC->SetTextColor(settings.trade.last.cf.crTextColor);
				pDC->ExtTextOut(X, Y, ETO_CLIPPED, &rect, text, n, NULL);
			} // if (trade.last.valid || (m_osdCtrl.active & OSD_TRADE_LAST))
			else
			{
				pFont = &fontTradeBasic;
				CreateFont(pDC, &pFont, fmt1.cf);
				pDC->SelectFont(*pFont);

				TSTRING_SMALL(str1);
				TSTRING_SMALL(str2);
				n = _stprintf_s(str1, SIZEOF_ARRAY(str1), TEXT("%d"), static_cast<int>(trade.quantity));
				if (settings.trade.basic.groupDigits)
					n = my::str::GroupDigits(str1, n, str1, SIZEOF_ARRAY(str1));
				n = _stprintf_s(str2, SIZEOF_ARRAY(str2), format, (trade.quantity > 0) ? trade.price2 : curPrice);
				if (settings.trade.basic.groupDigits)
					n = my::str::GroupDigits(str2, n, str2, SIZEOF_ARRAY(str2));
				n = _stprintf_s(text, textSize, TEXT("%s x %s"), str1, str2);
				pDC->GetTextExtent(text, n, &size);
				
				int xIcon, yIcon;
				int spacing = size.cy;
				x = (rect.Width() - (size.cx - cxIcon)) / 2; // m_icons.size.cx
#if 0
				y = y0 + 0;				
#else
				y = ((rect.Height() - size.cy - spacing)*2/3) / 2;
#endif
				int X = x + fmt1.offset.cx;
				int Y = y + fmt1.offset.cy;
				if (m_osdCtrl.active & OSD_TRADE_PRICE)
				{
					pDC->SelectPen(penFrame);
					pDC->Rectangle(X - 2, Y + 1, X + 2 + size.cx, Y + size.cy);
				}
				pDC->SetTextColor(fmt1.cf.crTextColor);
				pDC->ExtTextOut(X, Y, ETO_CLIPPED, &rect, text, n, NULL);	
				
				if (trade.quantity > 0)
				{// Выводим значок проводимой операции:
					xIcon = x - cxIcon - 2;
					yIcon = y + (int)((double)(size.cy - cyIcon)/2 + 0.5);
					int iIcon = (trade.operation == Transaction::OPERATION_BUY) ? 0 : 1; 
					::DrawIconEx(pDC->m_hDC, xIcon + fmt1.offset.cx, yIcon + fmt1.offset.cy, m_icons.hIcon[iIcon], 
						m_icons.size.cx, m_icons.size.cy, 0, NULL, DI_NORMAL);
				}

				n = _stprintf_s(text, textSize, format, trade.profit);
				if (settings.trade.profit.groupDigits)
					n = my::str::GroupDigits(text, n, text, textSize);

				pFont = &fontTradeProfit;
				CreateFont(pDC, &pFont, fmt2.cf);
				pDC->SelectFont(*pFont);

				y += 0 + spacing; // size.cy
				x = x + size.cx;
				pDC->GetTextExtent(text, n, &size);
				x -= size.cx;

				X = x + fmt2.offset.cx;
				Y = y + fmt2.offset.cy;
				if (price != 0)
				{
					if (m_osdCtrl.active & OSD_TRADE_PROFIT)
					{
						pDC->SelectPen(penFrame);
						pDC->Rectangle(X - 2, Y + 1, X + 2 + size.cx, Y + size.cy);
					}
					pDC->SetTextColor(fmt2.cf.crTextColor);
					pDC->ExtTextOut(X, Y, ETO_CLIPPED, &rect, text, n, NULL);
				}
			}
		}

		{// Лимит и доход:
			TxtFormat & fmt1 = settings.account.limit;
			TxtFormat & fmt2 = settings.account.profit;
			n = _stprintf_s(text, textSize, TEXT("%0.2f"), limit);				
			if (settings.account.limit.groupDigits)
				n = my::str::GroupDigits(text, n, text, textSize);
			pFont = &fontLimit;
			CreateFont(pDC, &pFont, fmt1.cf);
			pDC->SelectFont(*pFont);

			pDC->GetTextExtent(text, n, &size);
			int spacing = size.cy;
			x = rect.right - size.cx - border;
#if 0
			y = y0;
#else
			y = ((rect.Height() - size.cy - spacing)*1/2) / 2;
#endif
			int X = x + fmt1.offset.cx;
			int Y = y + fmt1.offset.cy;
			if (pAccount != NULL)
			{
				if (m_osdCtrl.active & OSD_LIMIT)
				{
					pDC->SelectPen(penFrame);
					pDC->Rectangle(X - 2, Y + 1, X + 2 + size.cx, Y + size.cy);
				}
				pDC->SetTextColor(fmt1.cf.crTextColor);
				pDC->ExtTextOut(X, Y, ETO_CLIPPED, &rect, text, n, NULL);		
			} // if (pAccount != NULL)

			n = _stprintf_s(text, textSize, TEXT("%0.2f"), profit);
			if (settings.account.profit.groupDigits)
				n = my::str::GroupDigits(text, n, text, textSize);
			pFont = &fontIncome;
			CreateFont(pDC, &pFont, fmt2.cf);
			pDC->SelectFont(*pFont);

			y += 0 + spacing;//(rect.Height() - size.cy) / 2;
			pDC->GetTextExtent(text, n, &size);
			x = rect.right - size.cx - border;

			X = x + fmt2.offset.cx;
			Y = y + fmt2.offset.cy - 1;
			if (pAccount != NULL)
			{
				if (m_osdCtrl.active & OSD_PROFIT)
				{
					pDC->SelectPen(penFrame);
					pDC->Rectangle(X - 2, Y + 1, X + 2 + size.cx, Y + size.cy);
				}
				pDC->SetTextColor(fmt2.cf.crTextColor);
				pDC->ExtTextOut(X, Y, ETO_CLIPPED, &rect, text, n, NULL);
			} // if (pAccount != NULL)
		}				
	} // if (pTable != NULL)
	else
	{
		if (theApp.GetNrInstruments() > 0)
			n = _stprintf_s(text, textSize, TEXT("Выберите рабочий инструмент!"));
		else
			n = _stprintf_s(text, textSize, TEXT("")); // "Нет данных"

		pFont = &font;
		CreateFont(pDC, &pFont, settings.foreground.cf);
		pDC->SelectFont(*pFont);

		pDC->GetTextExtent(text, n, &size);
		
		x = (rect.Width() - size.cx) / 2;
		y = (rect.Height() - size.cy) / 2;

		pDC->SetTextColor(settings.foreground.cf.crTextColor);
		pDC->ExtTextOut(x, y, ETO_CLIPPED, &rect, text, n, NULL);
	}

	//pDC->SelectObject(pOldFont);
	pDC = &wndDC;
	pDC->SetStretchBltMode(COLORONCOLOR);
	pDC->BitBlt(rect.left, rect.top, rect.Width(), rect.Height(), _dc, rect.left, rect.top, SRCCOPY);

	wnd.ReleaseDC(*pDC);
}

void CTradeDlg::OnSetBase()
{
	if (m_mode == MODE_ALTERNATIVE)
		ChangeMode();
}

void CTradeDlg::OnSetExtended()
{
	if (m_mode == MODE_NORMAL)
		ChangeMode();
}

void CTradeDlg::SetUserTrade(int set, const Settings::Presentation::Glass * pSettings)
{
	QuoteTable * pTable;
	if (set != 0)
	{
		if (pSettings == NULL)
			pSettings = &theApp.m_settings.presentation.glass;

		QuoteTable * pCurrentTable = theApp.GetCurrentTable();
		if (pCurrentTable != NULL)
			theApp.SetCurrentTrade(pCurrentTable->GetName());

		pTable = &m_userTable;
		pTable->Create(TEXT("UserTrade"));
		
		pTable->SetProperties(theApp.GetCurrentTableProperty());
#if 1
		QuoteTable::Array * pArrayBuy = pTable->GetArray(QuoteTable::I_BUY);
		QuoteTable::Array * pArraySell = pTable->GetArray(QuoteTable::I_SELL);
#else
		QuoteTable::Array * pArrayGen = pTable->GetArray(QuoteTable::I_NEUTRAL);
#endif
		QuoteTable::Array * pArray;

		double price;
		double volumeMin, volumeMax;
		volumeMin = DBL_MAX; volumeMax = 0;

		double quantity;
#if 1
		int operations[] = {Transaction::OPERATION_SELL, Transaction::OPERATION_BUY};
#endif	
		int iItem;
		const TradeItem * pTradeItem;
		QuoteTable::Array::Item * pItem;

		iItem = 0;

		const int maxSize = 50;
#if 1
		if (pArrayBuy->IsEmpty())
			pArrayBuy->Create(maxSize);
		if (pArraySell->IsEmpty())
			pArraySell->Create(maxSize);
#else
		if (pArrayGen->IsEmpty())
			pArrayGen->Create(2*maxSize);
#endif
		int full = 1;

		int search = F_SEARCH_ANY;
		int operation = Transaction::OPERATION_UNKNOWN;
#if 1
		pArrayBuy->Clear(full); pArraySell->Clear(full);
		pArray = pArrayBuy;

		search |= F_SEARCH_OPERATION;
		for (int op = 0; op < 2; op++)
#else
		pArrayGen->Clear(full);
		pArray = pArrayGen;
#endif
		{
			theApp.BeginFindItem();
#if 1
			operation = operations[op];
#endif
			for (;;)
			{
				pTradeItem = theApp.FindItem(search, 0, 0, 0, operation, 0, 0);
				if (pTradeItem != NULL)
				{
					price = pTradeItem->price;
					quantity = pTradeItem->quantity;

					pItem = NULL;
#if 1
					if (pSettings->user.flags & F_USER_GROUP_BY_PRICE)
					{
						// Проверяем есть ли уже такая цена:
						for (int i = 0; i < iItem; i++)
						{
							QuoteTable::Array::Item * p = pArray->GetItem(i);
							if (p->values[QuoteTable::ITEM_PRICE] == price)
							{
								pItem = p;
								break;
							}
						} // for (i)
					}
#endif
					if (pItem == NULL)
						pItem = pArray->GetItem(iItem++);
					if (iItem == pArray->GetCapacity())
						break;
					
					pItem->values[QuoteTable::ITEM_PRICE] = price;
					double & volume = pItem->values[QuoteTable::ITEM_VOLUME];							
					volume += quantity;
					
					if (volume < volumeMin)
						volumeMin = volume;
					if (volume > volumeMax)
						volumeMax = volume;

					int iUser = -1;
					if (pTradeItem->action == Transaction::NEW_STOP_ORDER)
						iUser = QuoteTable::ITEM_USER_STOP_BUY;
					else if (Transaction::IsActive(pTradeItem->status))
						iUser = QuoteTable::ITEM_USER_BID_BUY;
					else
						iUser = QuoteTable::ITEM_USER_DEAL_BUY;
					if (iUser >= 0)
					{
						pItem->values[iUser + operation] += quantity;
						pItem->user += quantity;
#if _DEBUG && 0
						pItem->values[QuoteTable::ITEM_USER_STOP_BUY + operation] += quantity;
						pItem->user += quantity;
#endif
					}
				}
				else
					break;
			} // for (;;)
			pArray->count = iItem;
			iItem = 0;
#if 1
			++pArray;
#endif
		} // for (op)		
#if 1
		// Теперь сортировка:
		iItem = 0;
		const int iPrice = QuoteTable::ITEM_PRICE;
		const int iVolume = QuoteTable::ITEM_VOLUME;
#if 1
		pArray = pArrayBuy;
		for (int op = 0; op < 2; op++)
#endif
		{
			for (int i = 0; i < pArray->count; i++)
			{
				for (int j = pArray->count - 1; j > i; j--)
				{
					int i1 = j - 1;
					int i2 = j;
					double * pVal1 = &pArray->items[i1].values[iPrice];
					double * pVal2 = &pArray->items[i2].values[iPrice];
					BOOL swap = FALSE;
#if 1
					if (op == 0)
					{
						if (*pVal1 < *pVal2)
							swap = TRUE;
					}
					else
#endif
					{
						if (*pVal1 > *pVal2)
							swap = TRUE;
					}
					if (swap)
					{
						double val;						
						// Цена:
						val = *pVal1; *pVal1 = *pVal2; *pVal2 = val;
						// Объём:
						pVal1 = &pArray->items[i1].values[iVolume];
						pVal2 = &pArray->items[i2].values[iVolume];
						val = *pVal1; *pVal1 = *pVal2; *pVal2 = val;

						int iIt1 = iItem + i1;
						int iIt2 = iItem + i2;

						for (int iUser = QuoteTable::ITEM_USER_FIRST; iUser <= QuoteTable::ITEM_USER_LAST; iUser++)
						{
							pVal1 = &pArray->items[iIt1].values[iUser];
							pVal2 = &pArray->items[iIt2].values[iUser];
							val = *pVal1; *pVal1 = *pVal2; *pVal2 = val;
						} // for (iUser)

					}
				} // for (j)
			} // for (i)
#if 1
			++pArray;
#endif
		} // for (op)
#endif
		pTable->SetVolumeMinMax(volumeMin, volumeMax);
	}
	else
		pTable = NULL;

	theApp.EndFindItem();

	if (dlgGlass)
	{
		dlgGlass.SetUserTrade(set, pTable);
		dlgGlass.Update(NULL);
	}
}

void CTradeDlg::UpdateUserTrade(int flags)
{
	if (my::wnd::IsValid(dlgGlass))
	{
		if (dlgGlass.IsUserTrade())
			SetUserTrade();
	}
}

void CTradeDlg::OnLButtonDown(UINT nFlags, CPoint point)
{
	m_listCtrl.SetFocus();
#if 0
	if (m_pToolTipCtrl)
	{
		m_pToolTipCtrl->Activate(TRUE);
		m_pToolTipCtrl->UpdateTipText(TEXT("This is Tool Tip"), *this);
		m_pToolTipCtrl->Popup();
	}
#endif
	my::MouseCapturer::Capture(*this, nFlags, point);
}

void CTradeDlg::OnRButtonUp(UINT nFlags, CPoint point)
{
	my::MouseCapturer::Release();

	UINT flags = TPM_LEFTALIGN|TPM_RIGHTBUTTON;
	CWindow wnd = ChildWindowFromPoint(point);
	UINT id = wnd.GetDlgCtrlID();
	switch (id)
	{
	case IDC_BUTTON_BUY:
	case IDC_BUTTON_SELL:
	case IDC_BUTTON_CANCEL:
	case IDC_BUTTON_CANCEL_ALL:
	case IDC_BUTTON_ZERO:
	case IDC_BUTTON_REVERSE:
	case IDC_BUTTON_STOP:
		{
			CMenu menu;
			if (TRUE == menu.CreatePopupMenu())
			{
				ModifyMenuButton(menu, id);
				if (menu.GetMenuItemCount() > 0)
				{
					CPoint point;
					GetCursorPos(&point);
					menu.TrackPopupMenu(flags, point.x, point.y, *this);
				}
			}
		}
		break;
	case IDC_EDIT_TRADE_PRICE1:
	case IDC_EDIT_TRADE_QUANTITY:
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
	default:
		{
			CRect rect;
			m_listCtrl.GetWindowRect(rect);
			ScreenToClient(rect);
			if (rect.PtInRect(point) && m_listCtrl == GetFocus())
				ShowMenuTrading();
			else
			{
				CMenu mainMenu;
				MakeMainMenu(mainMenu);
				if (mainMenu)
				{
					CPoint point;
					GetCursorPos(&point);
					mainMenu.TrackPopupMenu(flags, point.x, point.y, *this);
				}
				mainMenu.Detach();
			}
		}
	}
}

void CTradeDlg::OnMouseMove(UINT nFlags, CPoint point)
{
	my::MouseCapturer::OnMouseMove(nFlags, point);
}

void CTradeDlg::OnTransactionInfo()
{
	if (m_info)
	{

		m_info = INFO_DISABLED;
	}
	else
	{

		m_info = INFO_TRANSACTION;
	}
	UpdateOSD();
}

void CTradeDlg::OnUserTrade()
{
	if (my::wnd::IsValid(dlgGlass))
		dlgGlass.OnUserTrade();
}

void CTradeDlg::OnViewGlass(UINT id)
{
	if (my::wnd::IsValid(dlgGlass))
	{
		Settings::Presentation::Glass & settings = theApp.m_settings.presentation.glass;		
		switch (id)
		{
		case ID_GLASS_BUY_ABOVE:
			settings.view.flags ^= F_GLASS_VIEW_BUY_UP;
			break;
		case ID_GLASS_FLIP_HORIZONTAL:
			settings.view.flipHorizontal ^= TRUE;
			break;
		case ID_GLASS_RAREFY:
			settings.view.flags ^= F_GLASS_VIEW_RAREFIED;
			break;
		case ID_GLASS_NEUTRAL_ZONE:
			settings.view.flags ^= F_GLASS_VIEW_NEUTRAL_ZONE;
			break;
		case ID_GLASS_STYLE_1:
		case ID_GLASS_STYLE_2:
		case ID_GLASS_STYLE_3:
			settings.view.style = id - ID_GLASS_STYLE_1;
			break;
		//case ID_GLASS_GRID:
		//	settings.grid.show ^= TRUE;
		//	break;
		case ID_GLASS_GRID_LINES_VERTICAL:
		case ID_GLASS_GRID_LINES_HORIZONTAL:		
			{
				Settings::Presentation::Glass::Grid & grid = settings.grid;
				if (id == ID_GLASS_GRID_LINES_VERTICAL)
					grid.lines ^= GRID_LINES_VERTICAL;
				else
					grid.lines ^= GRID_LINES_HORIZONTAL;
			}
			break;
		case ID_GLASS_BORDER:
			settings.border.show ^= TRUE;
			break;
		case ID_GLASS_BORDER_LINES_BORDERS:
		case ID_GLASS_BORDER_LINES_MEDIAN:		
			{
				Settings::Presentation::Glass::Border & border = settings.border;
				if (id == ID_GLASS_BORDER_LINES_BORDERS)
					border.lines ^= BORDER_LINES_BORDERS;
				else
					border.lines ^= BORDER_LINES_MEDIAN;
			}
			break;
		case ID_GLASS_MARKER:
			settings.marker.show ^= TRUE;
			break;
		case ID_GLASS_MARKER_FOLLOW_PRICE:
		case ID_GLASS_MARKER_FOLLOW_CURSOR:
		case ID_GLASS_MARKER_FOLLOW_MENU:
			{
				Settings::Presentation::Glass::Marker & marker = settings.marker;
				if (id == ID_GLASS_MARKER_FOLLOW_PRICE)
					marker.follow ^= MARKER_FOLLOW_PRICE;
				else if (id == ID_GLASS_MARKER_FOLLOW_CURSOR)
					marker.follow ^= MARKER_FOLLOW_CURSOR;
				else
					marker.follow ^= MARKER_FOLLOW_MENU;
			}
			break;
		case ID_USER_SHOW_ACTIVE_BIDS: 
			settings.user.flags ^= F_USER_SHOW_ACTIVE_BIDS;
			break;
		case ID_USER_SHOW_ACTIVE_STOPS:
			settings.user.flags ^= F_USER_SHOW_ACTIVE_STOPS;
			break;
		case ID_USER_SHOW_ACTIVE_DEALS:
			settings.user.flags ^= F_USER_SHOW_ACTIVE_DEALS;
			break;
		case ID_USER_SHOW_ICONS:
			settings.user.flags ^= F_USER_SHOW_ICONS;
			break;
		case ID_GLASS_USER_SEPARATE_COLUMN:
			settings.user.flags ^= F_USER_SHOW_USER_COLUMN;
			break;
		case ID_GLASS_USER_GROUP_PRICE:
			settings.user.flags ^= F_USER_GROUP_BY_PRICE;
			if (dlgGlass.IsUserTrade())
				this->SetUserTrade(SET, &settings);
			break;
		} // switch (id)
		
		theApp.SetModifySettings();

		dlgGlass.SetSettings(settings);
		dlgGlass.Update(NULL);
	}
}

int CTradeDlg::MessageBox(LPCTSTR lpszText, UINT nType, UINT nIDHelp)
{
	return my::MessageBox((this->IsHidden()) ? (HWND)NULL : GetActiveWindow(), lpszText, nType); // *this
}

LRESULT CTradeDlg::OnHdnItemclickList1(NMHDR *pNMHDR)
{
	LPNMHEADER phdr = reinterpret_cast<LPNMHEADER>(pNMHDR);
	
	m_listCtrl.DoSort();
	
	return 0;
}

void CTradeDlg::OnAbout()
{
	CDlgAbout dlg;
	dlg.DoModal();
}


void CTradeDlg::OnInstrumentPrevios()
{
	QuoteTable * pTable = theApp.GetPreviosTable();
	if (pTable != NULL && pTable != theApp.GetCurrentTable())
	{
		SelectInstrument(pTable);
	}						
}

void CTradeDlg::OnInstrumentNext()
{
	QuoteTable * pTable = theApp.GetNextTable();
	if (pTable != NULL && pTable != theApp.GetCurrentTable())
	{
		SelectInstrument(pTable);
	}						
}

int CTradeDlg::SaveWindowPosition()
{
	int flags = my::lib::FWP_CX;
	int save = FALSE;
	Settings::Presentation::Main & settings = theApp.m_settings.presentation.main;
	if (! (settings.show.wnd & F_MAIN_SHOW_LIST))
		flags |= my::lib::FWP_CY;
#if 0
	else
	{
		if (settings.show.OSD == FALSE || settings.show.controls == FALSE)
			flags |= my::lib::FWP_CY;
	}
#endif
#if 1
	save = TRUE;
#endif
	if (save)
		theApp.SaveWindowPosition(*this, GetName(), flags);
	return S_OK;
}

void CTradeDlg::OnShowListOfDeals()
{
	Settings::Presentation & settings = theApp.m_settings.presentation;
	settings.main.show.wnd ^= F_MAIN_SHOW_LIST;
#if 1
	SaveWindowPosition();
#endif
	UpdatePresentation(settings, TRUE, TRUE);
	theApp.SetModifySettings();
}

void CTradeDlg::OnShowOsd()
{
	Settings::Presentation & settings = theApp.m_settings.presentation;
	settings.main.show.wnd ^= F_MAIN_SHOW_OSD;
#if 1
	SaveWindowPosition();
#endif
	UpdatePresentation(settings, TRUE, TRUE);
	theApp.SetModifySettings();
}

void CTradeDlg::OnShowControls()
{
	Settings::Presentation & settings = theApp.m_settings.presentation;
	settings.main.show.wnd ^= F_MAIN_SHOW_CONTROLS;
#if 1
	SaveWindowPosition();
#endif
	UpdatePresentation(settings, TRUE, TRUE);
	theApp.SetModifySettings();
}

void CTradeDlg::OnTopmost()
{
	theApp.m_settings.presentation.main.flags ^= F_VIEW_ALLWAYS_ON_TOP;
	UpdatePresentation(theApp.m_settings.presentation, TRUE, TRUE);
	theApp.SetModifySettings();
}

void CTradeDlg::OnShowMain()
{
	theApp.m_settings.presentation.view.item[I_WINDOW_DEALS].flags ^= F_SHOW_WINDOW;
	UpdatePresentation(theApp.m_settings.presentation, TRUE, TRUE);
	theApp.SetModifySettings();
}

#ifdef _DEBUG
#endif // _DEBUG

void CTradeDlg::OnTimer(UINT_PTR nIDEvent)
{
	if (nIDEvent == TIMER_CONNECTION)
	{
#if defined _DEBUG && 0
		TRACE("TIMER_CONNECTION\r\n");
#endif
		m_transCtrl.CheckConnection();
	}
}

void CTradeDlg::OnShowWindow(BOOL bShow, UINT nStatus)
{
#if defined _DEBUG && 1
	TRACE("--- %s window (status=%d) ---\r\n", bShow ? "Show" : "Hide", nStatus);
#endif
}

void CTradeDlg::OnActivate(UINT nState, BOOL bMinimized, CWindow wndOther)
{
#if 0
	if (! this->IsHidden())
#endif
		::SendMessage(*this, UM_ACTIVE, (WPARAM)this->m_hWnd, (LPARAM)nState);
}

void CTradeDlg::OnActivateApp(BOOL bActive, DWORD dwThreadID)
{
	TRACE(__FUNCTION__" (bActive=%d)\r\n", bActive);
	ResetShortcutsLL();
#if 1
	if (m_initialized)
	{
		if (bActive)
		{
			if (m_hActiveWnd != NULL)
				::SetActiveWindow(m_hActiveWnd);
		}
		else
			m_hActiveWnd = ::GetActiveWindow();
	}
#endif
}

void CTradeDlg::OnWindowPosChanging(LPWINDOWPOS lpWndPos)
{
	if (this->IsHidden())
		lpWndPos->flags &= ~SWP_SHOWWINDOW;
}
