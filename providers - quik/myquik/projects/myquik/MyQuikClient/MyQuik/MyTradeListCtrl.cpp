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
 *  MyTradeListCtrl.cpp
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "MyTradeListCtrl.h"
#include <color.h>

//
// class CMyTradeListCtrl
//

CMyTradeListCtrl::CMyTradeListCtrl()
{
	this->sort.type = SORT_BY_DATE_AND_TIME;
	this->sort.descending = 0;	
	this->sort.iItem = -1;

	this->flags = 0x0;
	SetNoSort(FALSE);
	SetNoScroll(FALSE);
}

void CMyTradeListCtrl::UpdateBkColor()
{
	COLORREF crBkColor = theApp.m_settings.presentation.list.colors.item[I_COLOR_NORMAL].backgrnd;//::GetSysColor(COLOR_3DFACE);
	SetBkColor(crBkColor);
}

LRESULT CMyTradeListCtrl::OnLvnColumnclick (NMHDR* phdr)
{
	LPNMLISTVIEW pNMLV = reinterpret_cast<LPNMLISTVIEW>(phdr);

	int iItem = pNMLV->iSubItem;
	OnColumnClick(iItem);

	return 0;
}

void CMyTradeListCtrl::OnColumnClick(int iCol)
{
	CHeaderCtrl header = GetHeader();
	HDITEM hdi;
	hdi.mask = HDI_FORMAT|HDI_LPARAM;
	int iItem = iCol;
	if (TRUE == header.GetItem(iItem, &hdi))
	{
		int index = static_cast<int>(hdi.lParam);
		if (index >= COL_FIRST && index != COL_INDEX)
		{
			if (sort.iItem >= COL_FIRST)
			{
				HDITEM prev;
				prev.mask = HDI_FORMAT;
				if (TRUE == header.GetItem(sort.iItem, &prev))
				{
					prev.fmt &= ~(HDF_SORTDOWN|HDF_SORTUP);
					header.SetItem(sort.iItem, &prev);
				}
			}
			if (hdi.fmt & (HDF_SORTUP))
			{
				hdi.fmt &= ~(HDF_SORTDOWN|HDF_SORTUP);
				hdi.fmt |= HDF_SORTDOWN;
				sort.descending = 1;
			}
			else
			{
				hdi.fmt &= ~(HDF_SORTDOWN|HDF_SORTUP);
				hdi.fmt |= HDF_SORTUP;
				sort.descending = 0;
			}
			header.SetItem(iItem, &hdi);
			sort.iItem = iItem;

			switch (index)
			{
			case COL_ICON: sort.type = SORT_BY_ICON;
				break;
			case COL_NUMBER: sort.type = SORT_BY_NUMBER;
				break;
			case COL_DATE: sort.type = SORT_BY_DATE|SORT_BY_TIME;
				break;
			case COL_TIME:
				{// Если есть колонка с датой, то сортируем только по времени!
					sort.type = SORT_BY_TIME;
					int found = 0;
					for (int i = 0; i < header.GetItemCount(); i++)
					{
						HDITEM hdi;
						hdi.mask = HDI_LPARAM;
						header.GetItem(i, &hdi);
						int index = static_cast<int>(hdi.lParam);
						if (index == COL_DATE)
						{
							found = 1;						
							break;
						}
					} // for (i)
					if (! found)
						sort.type |= SORT_BY_DATE;
				}
				break;
			case COL_NAME: sort.type = SORT_BY_NAME;
				break;
			case COL_SECCODE: sort.type = SORT_BY_SECCODE;
				break;
			case COL_OPERATION: sort.type = SORT_BY_OPERATION;
				break;
			case COL_PRICE: sort.type = SORT_BY_PRICE;
				break;
			case COL_PRICE2: sort.type = SORT_BY_PRICE2;
				break;
			case COL_QUANTITY: sort.type = SORT_BY_QUANTITY;
				break;
			case COL_VOLUME: sort.type = SORT_BY_VOLUME;
				break;
			case COL_STATUS: sort.type = SORT_BY_STATUS;
				break;
			case COL_CLASS: sort.type = SORT_BY_CLASSCODE;
				break;
			case COL_CLIENT: sort.type = SORT_BY_CLIENTCODE;
				break;
			case COL_ACCOUNT: sort.type = SORT_BY_ACCOUNT;
				break;
			}
		}
	}
#if 0
	this->SetSelectedColumn(iItem);
#endif
}

void CMyTradeListCtrl::DrawItem (UINT idCtrl, LPDRAWITEMSTRUCT lpDrawItemStruct)
{
	int iItem = lpDrawItemStruct->itemID;
	if (iItem < 0)
		return;

	CDC dc;
	dc.Attach(lpDrawItemStruct->hDC);

	// Save these value to restore them when done drawing.
	COLORREF crOldTextColor = dc.GetTextColor();
	COLORREF crOldBkColor = dc.GetBkColor();
	COLORREF colorHighlight, colorInactive;
	COLORREF backgrnd, color;
	double opacity;

	const Settings::Presentation & settings = theApp.m_settings.presentation;

	colorHighlight = settings.list.colors.item[I_COLOR_FRAME].text;
	GetHighlightParams(*this, colorHighlight, colorInactive, colorHighlight, opacity, F_VALID_COLOR_HIGHLIGHT);
	color = crOldTextColor; backgrnd = crOldBkColor;

	CHeaderCtrl header = GetHeader();
	HDITEM hdi;
	hdi.mask = HDI_LPARAM|HDI_FORMAT;
	header.GetItem(COL_INDEX, &hdi);
	int iColIndex = static_cast<int>(hdi.lParam); // всегда 0!
	if (iColIndex > 0)
		iColIndex = -1;

	LV_COLUMN lvc;
	lvc.mask = LVCF_FMT|LVCF_WIDTH;

	LVITEM lvi;
	lvi.mask = LVIF_PARAM|LVIF_STATE;
	lvi.iItem = iItem;
	lvi.stateMask = LVIS_SELECTED;
	GetItem(&lvi);

	int cxIndex = 0;

	Transaction * pTa = (Transaction*)lvi.lParam;
	if (pTa != NULL)
	{		
		const Settings::Presentation::ListOfDeals::Color * pColor = NULL;
		// Цвет текста:
		color = settings.list.colors.item[I_COLOR_NORMAL].text;
		// Цвет фона:
		backgrnd = settings.list.colors.item[I_COLOR_NORMAL].backgrnd;
#if 0
		if (pTa->operation == Transaction::OPERATION_BUY)
			backgrnd = settings.glass.colors.item[GLASS_COLOR_SELL];
		else
			backgrnd = settings.glass.colors.item[GLASS_COLOR_BUY];
#else
		dc.SetBkMode(TRANSPARENT);

		CRect rect;
		this->GetClientRect(rect);
		//dc.FillSolidRect(&rect, backgrnd);
#endif
		const int operation = pTa->operation;
		if (Transaction::IsFailed(pTa->status))
		{
			pColor = &settings.list.colors.item[I_COLOR_FAILED_BUY + operation];
		}
		else
		{
			if (pTa->baseAction == Transaction::NEW_STOP_ORDER)
			{
				if (Transaction::IsCanceled(pTa->status))
					pColor = &settings.list.colors.item[I_COLOR_CANCELED_STOP_BUY + operation];
				else if (Transaction::IsExecuted(pTa->status))
					pColor = &settings.list.colors.item[I_COLOR_EXECUTED_STOP_BUY + operation];
				else
					pColor = &settings.list.colors.item[I_COLOR_ACTIVE_STOP_BUY + operation];
			}
			else
			{
				if (Transaction::IsCanceled(pTa->status))
					pColor = &settings.list.colors.item[I_COLOR_CANCELED_BID_BUY + operation];
				else if (Transaction::IsExecuted(pTa->status))
					pColor = &settings.list.colors.item[I_COLOR_BUY + operation];
				else
					pColor = &settings.list.colors.item[I_COLOR_ACTIVE_BID_BUY + operation];
			}
		}
		if (pColor != NULL)
		{
			if (settings.list.colors.useDifferentColors)
				backgrnd = pColor->backgrnd;
			if (settings.list.colors.useDifferentColorsText)
				color = pColor->text;
		}

		rect.left = lpDrawItemStruct->rcItem.left;
		rect.top = lpDrawItemStruct->rcItem.top;
		rect.bottom = lpDrawItemStruct->rcItem.bottom;	

		//rect.top += 1;
		//rect.bottom += 1;

		if (iColIndex >= 0)
		{
			GetColumn(iColIndex, &lvc);
			cxIndex = lvc.cx;
			rect.left += cxIndex;
		}
		dc.FillSolidRect(&rect, backgrnd);
		dc.SetTextColor(color);
	}
	
	CRect frameRect(lpDrawItemStruct->rcItem);
#if 1
	WINDOWINFO wi;
	wi.cbSize = sizeof(wi);
	if (TRUE == ::GetWindowInfo(this->m_hWnd, &wi))
	{
		if (! (wi.dwExStyle & WS_EX_CLIENTEDGE))
			frameRect.left += 1;
	}
#endif
	if ((lpDrawItemStruct->itemAction | (ODA_SELECT|ODA_FOCUS)) &&
		(lpDrawItemStruct->itemState & (ODS_SELECTED|ODS_FOCUS)))
	{
		COLORREF bkgrnd;
		bkgrnd = my::lib::SumColorsAlpha(backgrnd, colorHighlight, opacity);
		dc.FillSolidRect(&frameRect, bkgrnd);
	}

	CImageList imgList = GetImageList(LVSIL_SMALL);
	CImageList * pImageList = &imgList;

	CRect rcItem(lpDrawItemStruct->rcItem);
	rcItem.right = rcItem.left;
	TSTRING_STD(text);

	const int margin = header.GetBitmapMargin();

	for (int iCol = COL_FIRST; GetColumn(iCol, &lvc); iCol++)
	{
		rcItem.left = rcItem.right;
		rcItem.right += lvc.cx;

		header.GetItem(iCol, &hdi);
		int col = static_cast<int>(hdi.lParam);
		int format = static_cast<int>(hdi.fmt & LVCFMT_JUSTIFYMASK);

		if (col == COL_ICON)
		{	
			int iIcon;
#if 0
			LVITEM lvi;
			lvi.mask = LVIF_IMAGE;
			lvi.iItem = iItem;
			lvi.iSubItem = iCol;
			GetItem(&lvi);
			iIcon = lvi.iImage;
#else
			TCHAR text[4];
			this->GetItemText(iItem, iCol, text, SIZEOF_ARRAY(text) - 1);
			iIcon = static_cast<int>((text[0] - 0x30));
#endif
			CPoint point;
			if (format == ALIGNMENT_LEFT)
				point.x = rcItem.left;
			else if (format == ALIGNMENT_RIGHT)
				point.x = rcItem.right - 16 - 1;
			else if (format == ALIGNMENT_CENTER)
				point.x += rcItem.left + (rcItem.Width() - 16) / 2;
			point.y = rcItem.top;

			if (pImageList)
				pImageList->Draw(dc, iIcon, point, ILD_TRANSPARENT);
		}
		else
		{
			CRect rectText(rcItem);			

			int dtFormat = DT_SINGLELINE|DT_VCENTER;
			if (format == ALIGNMENT_LEFT)
			{
				dtFormat |= DT_LEFT;
				rectText.DeflateRect(margin, 0, 0, 0);
			}
			else if (format == ALIGNMENT_RIGHT)
			{
				dtFormat |= DT_RIGHT;
				rectText.DeflateRect(0, 0, margin, 0);
			}
			else if (format == ALIGNMENT_CENTER)
				dtFormat |= DT_CENTER;

			int len = GetItemText(iItem, iCol, text, sizeof(text));
			if (col == COL_INDEX)
			{
				if (settings.list.colors.useDifferentColors)
					dc.FillSolidRect(rcItem.left, rcItem.top, rcItem.Width(), rcItem.Height(), settings.list.colors.item[I_COLOR_INDEX].backgrnd);
				dc.SetTextColor(settings.list.colors.item[I_COLOR_INDEX].text);
				dc.DrawText(text, len, &rectText, dtFormat);
			}
			else
			{
				dc.SetTextColor(color);
				dc.DrawText(text, len, &rectText, dtFormat);
			}
		}
	} // for (iCol)

	// If this item has the focus, draw a red frame around the item's rect.
	if ((lpDrawItemStruct->itemAction | ODA_FOCUS) && (lpDrawItemStruct->itemState & ODS_FOCUS))
	{
		CBrush br;
		br.CreateSolidBrush(colorHighlight);
		dc.FrameRect(&frameRect, br);
	}

	// Reset the backgrnd color and the text color back to their
	// original values.
	dc.SetTextColor(crOldTextColor);
	dc.SetBkColor(crOldBkColor);

	dc.Detach();
}

LPCTSTR CMyTradeListCtrl::ColumnIndexToName(int index)
{
	static LPCTSTR names[] = {
		TEXT(""),
		TEXT("    "),
		TEXT(" * "),
		TEXT("Номер"),
		TEXT("Дата"),
		TEXT("Время"),
		TEXT("Название"),
		TEXT("Код"),
		TEXT("Класс"),
		TEXT("Операция"),
		TEXT("Цена"),
		TEXT("Цена2"),
		TEXT("Кол-во"),
		TEXT("Объём"),
		TEXT("Состояние"),
		TEXT("Клиент"),
		TEXT("Счёт"),
	};
	if (index >= 0 && index < SIZEOF_ARRAY(names))
		return names[index];
	else
		return NULL;
}

int CMyTradeListCtrl::GetColumnWidth(int index, LPCTSTR columnName)
{
	LPCTSTR names[] = {
		TEXT(""),
		TEXT("_____"),
		TEXT("____"),
		TEXT("_1234567890_"),
		TEXT("_00.00.0000_"),
		TEXT("_00.00.00_"),
		TEXT("Abcdefghijklmn"),
		TEXT("_________"),
		TEXT("_________"),
		TEXT("___________"),
		TEXT("123456789"),
		TEXT("123456789"),
		TEXT("12345678"),
		TEXT("_1234567890_"),
		TEXT("_____________"),
		TEXT("_____________"),
		TEXT("_____________"),
	};
	if (index >= 0 && index < SIZEOF_ARRAY(names))
		return LoadColumnWidth(GetParentName(), columnName, this->GetStringWidth(names[index]));
	else
		return 10;
}

int CMyTradeListCtrl::LoadColumnWidth(LPCTSTR name, LPCTSTR colName, int defaultWidth)
{
	int width;
	TSTRING_PATH(path, size);
	_stprintf_s(path, size, TEXT("Workspace\\%s\\List\\Columns"), name);
	QueryIntValue(theApp.GetRegKey(), width, path, colName, defaultWidth);
	return width;
}

int CMyTradeListCtrl::SaveColumnWidth(LPCTSTR name, LPCTSTR colName, int width)
{
	TSTRING_PATH(path, size);
	_stprintf_s(path, size, TEXT("Workspace\\%s\\List\\Columns"), name);
	SetIntValue(theApp.GetRegKey(), width, path, colName);
	return 0;
}

void CMyTradeListCtrl::SaveListCtrlParams()
{
	LPCTSTR name = GetParentName();
	for (int i = COL_FIRST; ; i++)
	{
		LVCOLUMN col;
		TSTRING_SMALL(buf);
		col.mask = LVCF_WIDTH|LVCF_TEXT;
		col.pszText = buf;
		col.cchTextMax = SIZEOF_ARRAY(buf) - 1;
		if (TRUE == this->GetColumn(i, &col))
		{
			SaveColumnWidth(name, col.pszText, col.cx);
		}
		else
			break;
	} // for (i)
}

static int DResult2Result(double d1, double d2)
{
	int result;
	double dresult = d1 - d2;
	if (dresult)
	{
		if (dresult > 0)
			result = 1;
		else
			result = -1;
	}
	else
		result = static_cast<int>(dresult);
	return result;
}

int CALLBACK FuncCompareItems(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
{
	int result = 1;
	const Transaction * pTa1 = (const Transaction*)(lParam1);
	const Transaction * pTa2 = (const Transaction*)(lParam2);
#if 1
	if (pTa1 == NULL || pTa2 == NULL)
		return result;
#endif
	CMyTradeListCtrl::Sort * pSorting = (CMyTradeListCtrl::Sort *)lParamSort;
	int sort = pSorting->type;
	int descending = pSorting->descending;

	if (sort & SORT_BY_NUMBER)
	{
		double N1 = pTa1->orderKey;
		double N2 = pTa2->orderKey;
		if (N1 > N2)
			result = 1;
		else if (N1 < N2)
			result = -1;
		else
			result = 0;
	} // if (sort & SORT_BY_NUMBER)
	else if (sort & SORT_BY_PRICE)
		result = DResult2Result(pTa1->price, pTa2->price);
	else if (sort & SORT_BY_PRICE2)
		result = DResult2Result(pTa1->stop.price, pTa2->stop.price);
	else if (sort & SORT_BY_VOLUME)
		result = DResult2Result(pTa1->volume, pTa2->volume);
	else if (sort & SORT_BY_QUANTITY)
		result = DResult2Result(pTa1->quantity, pTa2->quantity);
	else if (sort & SORT_BY_OPERATION)
		result = (pTa1->operation - pTa2->operation);
	else if (sort & SORT_BY_STATUS)
		result = pTa1->status - pTa2->status;
	else if (sort & SORT_BY_NAME)
		result = -(lstrcmp(pTa1->strName, pTa2->strName));
	else if (sort & SORT_BY_SECCODE)
		result = -(lstrcmp(pTa1->strSecCode, pTa2->strSecCode));
	else if (sort & SORT_BY_CLASSCODE)
		result = -(lstrcmp(pTa1->strClassCode, pTa2->strClassCode));
	else if (sort & SORT_BY_CLIENTCODE)
		result = -(lstrcmp(pTa1->strClientCode, pTa2->strClientCode));
	else if (sort & SORT_BY_ACCOUNT)
		result = -(lstrcmp(pTa1->strAccount, pTa2->strAccount));
	else if (sort & SORT_BY_ICON)
	{
		int icon1 = CMyTradeListCtrl::GetIconIndex(pTa1->baseAction, pTa1->status, pTa1->operation);
		int icon2 = CMyTradeListCtrl::GetIconIndex(pTa2->baseAction, pTa2->status, pTa2->operation);
		result = icon1 - icon2;
	}

	if (result == 0)
	{
		sort |= (SORT_BY_DATE|SORT_BY_TIME);
		result = 1;
	}
	if (sort & (SORT_BY_DATE|SORT_BY_TIME))
	{
		// Сортировка по времени:
		DWORD date1 = (pTa1->time.wYear << 16) | (pTa1->time.wMonth << 8) | (pTa1->time.wDay);
		DWORD time1 = (pTa1->time.wHour << 16) | (pTa1->time.wMinute << 8) | (pTa1->time.wSecond);

		DWORD date2 = (pTa2->time.wYear << 16) | (pTa2->time.wMonth << 8) | (pTa2->time.wDay);
		DWORD time2 = (pTa2->time.wHour << 16) | (pTa2->time.wMinute << 8) | (pTa2->time.wSecond);

		if (sort & SORT_BY_DATE)
		{
			if (date2 != date1)
			{
				result = date1 - date2;
				goto end;
			}
		}		
		if (sort & SORT_BY_TIME)
		{
			if (time2 > time1)
			{
				result = -1;
			}
			else if (time2 == time1)
			{
				if (pTa1->line != 0 && pTa2->line != 0)
					result = pTa1->line - pTa2->line;
				else
				{
					// Стоп-заявка должна стоять выше исполненной по ней заявки
					if (Transaction::IsActionStop(pTa1->action) && Transaction::IsExecuted(pTa1->status))
					{
						result = -1;
					}
				}
			}
		}
	} // if (sort & (SORT_BY_DATE|SORT_BY_TIME))
end:	 
	if (descending)
		result = -result;
	return result;
}

int CMyTradeListCtrl::UpdateColumn(int iCol)
{
	const QuoteTable * pTable = theApp.GetCurrentTable();

	int count = this->GetItemCount();
	for (int iItem = 0; iItem < count; iItem++)
	{
		const Transaction * pTa = (const Transaction*)this->GetItemData(iItem);
		const QuoteTable::Properties * pProperties = theApp.FindTableProperty(pTa);
		UpdateColumns(iItem, pTa, pProperties);
	} // for (iItem)

	return 0;
}

int CMyTradeListCtrl::UpdateColumnIndex()
{
	CHeaderCtrl header = GetHeader();
	HDITEM hdi;
	hdi.mask = HDI_LPARAM;
	int index;
	int iCol = -1;

	for (int i = COL_FIRST; i < NR_COLS_MAX; i++)
	{
		header.GetItem(i, &hdi);
		index = static_cast<int>(hdi.lParam);
		if (index >= 0)
		{
			if (index == COL_INDEX)
			{
				iCol = i;
				break;
			}
		}
		else
			break;
	}

	if (iCol >= COL_FIRST)
	{
		int count = this->GetItemCount();
		for (int iItem = 0; iItem < count; iItem++)
		{
			UpdateColumnItemIndex(iCol, iItem);
		}
	}

	return 0;
}

int CMyTradeListCtrl::UpdateColumnItem(int iCol, int iItem, const Transaction * pTransaction, const QuoteTable::Properties * pProperties)
{
	int index = ColumnToIndex(iCol);	
	return UpdateColumnItemByIndex(iCol, iItem, index, pTransaction, pProperties);
}

int CMyTradeListCtrl::UpdateColumnItemByIndex(int iCol, int iItem, int index, const Transaction * pTransaction, 
	const QuoteTable::Properties * pProperties)
{
	TSTRING_STD2(str, size);
	LPCTSTR pStr = NULL;

	int nd = (pProperties != NULL) ? pProperties->price.nd : 0;
	TCHAR format[] = TEXT("%0.xf");
	format[3] = (nd % 10) + 0x30;

	if (index >= 0)
	{
		switch (index)
		{
#if 0
		case COL_INDEX:
			{
				int count = this->GetItemCount();
				str.Format(TEXT("%d"), count);
				pStr = str;
			}
			break;
#endif
		case COL_ICON:
			{// Значок:
				int icon = GetIconIndex(pTransaction->baseAction, pTransaction->status, pTransaction->operation);
				// Устанавливаем соответвстующий значок:
#if 0
					this->SetItem(iItem, iCol, LVIF_IMAGE, NULL, icon, 0, 0, 0);
#else
					// Почему-то значок устанавливается только для 0-й колонки?!
					// Поэтому храним значок в виде текста:
					str[0] = TCHAR(0x30 + icon); str[1] = 0;
					pStr = str;
#endif
			}
			break;
		case COL_NUMBER:
			{// Номер заявки:
				if (pTransaction->orderKey == 0)
					_stprintf_s(str, size, TEXT(""));
				else
					_stprintf_s(str, size, TEXT("%.0f"), pTransaction->orderKey);
				pStr = str;					
			}
			break;
		case COL_DATE:
			{// Дата:
				_stprintf_s(str, size, TEXT("%02d.%02d.%04d"), 
					pTransaction->time.wDay, pTransaction->time.wMonth, pTransaction->time.wYear);
				pStr = str;
			}
			break;
		case COL_TIME:
			{// Время:
				_stprintf_s(str, size, TEXT("%02d:%02d:%02d"), 
					pTransaction->time.wHour, pTransaction->time.wMinute, pTransaction->time.wSecond);
				pStr = str;
			}
			break;
		case COL_NAME:
			{// Название инструмента:
				pStr = pTransaction->strName;
			}
			break;
		case COL_SECCODE:
			{// Код инструмента:
				pStr = pTransaction->strSecCode;
			}
			break;
		case COL_CLASS:
			{// Код класса:
				pStr = pTransaction->strClassCode;
			}
			break;
		case COL_CLIENT:
			{// Код клиента:
				pStr = pTransaction->strClientCode;
			}
			break;
		case COL_ACCOUNT:
			{// Счёт:
				pStr = pTransaction->strAccount;
			}
			break;
		case COL_OPERATION:
			{// Операция:
				pStr = (pTransaction->operation == Transaction::OPERATION_BUY) ? TEXT("Купля") : TEXT("Продажа");
			}
			break;
		case COL_PRICE:
			{// Цена:
				_stprintf_s(str, size, format, pTransaction->price);
				pStr = str;
			}
			break;
		case COL_PRICE2:
			{// Цена2 (стоп-цена):
				if (Transaction::IsActionStop(pTransaction->baseAction))
					_stprintf_s(str, size, format, pTransaction->stop.price);
				else
					_stprintf_s(str, size, TEXT(""));
				pStr = str;
			}
			break;
		case COL_QUANTITY:
			{// Количество:
				_stprintf_s(str, size, TEXT("%.0f"), pTransaction->quantity);
				pStr = str;
			}
			break;
		case COL_VOLUME:
			{// Объем:
				//pTransaction->volume = pTransaction->price * pTransaction->quantity;
				_stprintf_s(str, size, format, pTransaction->price * pTransaction->quantity);
				pStr = str;
			}
			break;
		case COL_STATUS:
			{// Состояние:
				pStr = Transaction::StatusToString(pTransaction->status);
			}
			break;
		} // switch (index)

		if (pStr)
			this->SetItemText(iItem, iCol, pStr);
	}
	return 0;
}

int CMyTradeListCtrl::UpdateColumnItemIndex(int iCol, int iItem)
{
	TSTRING_SMALL2(str, size);
	_stprintf_s(str, size, TEXT("%d"), iItem + 1);
	this->SetItemText(iItem, iCol, str);
	return 0;
}

int CMyTradeListCtrl::UpdateColumns(int iItem, const Transaction * pTransaction, const QuoteTable::Properties * pProperties)
{
	for (int iCol = COL_FIRST; iCol < NR_COLS_MAX; iCol++)
	{
		UpdateColumnItem(iCol, iItem, pTransaction, pProperties);
	}
	return 0;
}

void CMyTradeListCtrl::UpdateColumns(const Settings::Presentation::ListOfDeals & settings, const Settings::Presentation::ListOfDeals & prev)
{
	CHeaderCtrl header = GetHeader();
	int count;
	int index;
	int iCol, col;
	int size = SIZEOF_ARRAY(settings.columns.item);
#if defined _DEBUG && 1
	USES_CONVERSION;
	for (int i = 0; i < size; i++)
	{
		const Settings::Presentation::ListOfDeals::Column & column = prev.columns.item[i];
		TRACE("%d:%d:%d ", column.index, column.visible, column.format);
	} // for (i)
	TRACE("\r\n");
	for (int i = 0; i < size; i++)
	{
		const Settings::Presentation::ListOfDeals::Column & column = settings.columns.item[i];
		TRACE("%d:%d:%d ", column.index, column.visible, column.format);
	} // for (i)
	TRACE("\r\n");	
#endif
	HDITEM hdi;
	hdi.mask = HDI_LPARAM;

	// Сначала удаляем столбцы:

	for (int i = COL_FIRST; i < size; i++)
	{
		const Settings::Presentation::ListOfDeals::Column & prevColumn = prev.columns.item[i];
		const Settings::Presentation::ListOfDeals::Column & column = settings.columns.item[i];
		int remove = 0;
		count = header.GetItemCount();
		for (iCol = COL_FIRST; iCol < count; iCol++)
		{
			header.GetItem(iCol, &hdi);
			index = static_cast<int>(hdi.lParam);
			if (index == column.index)
			{
				if (!column.visible)					
					++remove;
				else if (column.format != prevColumn.format && (column.visible && prevColumn.visible))
				{
					hdi.mask |= HDI_FORMAT;
					hdi.fmt = column.format|HDF_STRING;
					header.SetItem(iCol, &hdi);
					hdi.mask &= ~HDI_FORMAT;
#if defined _DEBUG && 1
					LPCTSTR name = ColumnIndexToName(column.index);
					TRACE("UpdateColumn(%d, %s)\r\n", iCol, T2A(name));
#endif
					UpdateColumn(iCol);
				}
				if (remove)
				{
#if defined _DEBUG && 1
					LPCTSTR name = ColumnIndexToName(column.index);
					TRACE("1: DeleteColumn(%d, %s)\r\n", iCol, T2A(name));
#endif
					if (sort.iItem == iCol)
					{// очищаем сортировку:
						sort.iItem = -1;
					}
					this->DeleteColumn(iCol);
				}
				break;
			}
		} // for (iCol)
	} // for (i)

	// Перемещение стобцов:

	col = COL_FIRST;
	for (int i = col; i < size; i++)
	{
		const Settings::Presentation::ListOfDeals::Column & prevColumn = prev.columns.item[i];
		const Settings::Presentation::ListOfDeals::Column & column = settings.columns.item[i];
		if (column.visible)
		{
			header.GetItem(col, &hdi);
			if (column.index != static_cast<int>(hdi.lParam))
			{
				if (prevColumn.visible)
				{
#if defined _DEBUG && 1
					LPCTSTR name = ColumnIndexToName(column.index);
					TRACE("2: DeleteColumn(%d, %s)\r\n", col, T2A(name));
#endif
					this->DeleteColumn(col);
				}
			}
			else
				++col;
		} // if (column.visible)
	} // for (i)

	// Теперь добавляем столбцы:

	col = COL_FIRST;
	for (int i = col; i < size; i++)
	{
		const Settings::Presentation::ListOfDeals::Column & column = settings.columns.item[i];
		if (column.visible)
		{
			header.GetItem(col, &hdi);
			index = static_cast<int>(hdi.lParam);
			if (index != column.index)
			{
				LPCTSTR name = ColumnIndexToName(column.index);
				if (name != NULL)
				{
					int width = GetColumnWidth(column.index, name);
#if defined _DEBUG && 1
					TRACE("InsertColumn(%d, %s)\r\n", col, T2A(name));
#endif
					this->InsertColumn(col, name, column.format, width);
					count = header.GetItemCount();
					hdi.lParam = static_cast<LPARAM>(column.index);
					header.SetItem(col, &hdi);
					
					// Добавляем элементы в новую колонку:
					UpdateColumn(col);
				}
			}
			++col;
		} // if (column.visible)
	} // for (i)

	// Обновляем горизонтальную прокрутку:
	this->Update(0);
}

int CMyTradeListCtrl::GetIconIndex(int action, int status, int operation)
{
	int icon = -1;
	if (status == Transaction::STATUS_SENT)
	{
		if (action == Transaction::NEW_STOP_ORDER)
			icon = (operation == Transaction::OPERATION_BUY) ? I_ICON_STOP_BUY_GRAY : I_ICON_STOP_SELL_GRAY;
		else
			icon = (operation == Transaction::OPERATION_BUY) ? I_ICON_BUY_GRAY : I_ICON_SELL_GRAY;
	}
	else if (action == Transaction::NEW_ORDER)
	{
		switch (status)
		{
		case Transaction::STATUS_EX_BID_ACTIVE:
			icon = (operation == Transaction::OPERATION_BUY) ? I_ICON_ACTIVE_BUY : I_ICON_ACTIVE_SELL;
			break;	
		case Transaction::STATUS_REJECTED:
		case Transaction::STATUS_WRONG_PARAMETERS:
		case Transaction::STATUS_WRONG_LIMITS:
			icon = I_ICON_CANCEL;
			break;
		case Transaction::STATUS_EX_BID_CANCELED:
			icon = (operation == Transaction::OPERATION_BUY) ? I_ICON_CANCEL_BUY : I_ICON_CANCEL_SELL;
			break;
		case Transaction::STATUS_EX_BID_EXECUTED:
		case Transaction::STATUS_EXECUTED:
			icon = (operation == Transaction::OPERATION_BUY) ? I_ICON_BUY : I_ICON_SELL;
			break;
		} // switch (status)
	} // if (action == Transaction::NEW_ORDER)
	else if (action == Transaction::NEW_STOP_ORDER)
	{
		switch (status)
		{
		case Transaction::STATUS_EX_BID_ACTIVE:
			icon = (operation == Transaction::OPERATION_BUY) ? I_ICON_ACTIVE_STOP_BUY : I_ICON_ACTIVE_STOP_SELL;
			break;	
		case Transaction::STATUS_REJECTED:
		case Transaction::STATUS_WRONG_PARAMETERS:
		case Transaction::STATUS_WRONG_LIMITS:
			icon = I_ICON_CANCEL_STOP;
			break;
		case Transaction::STATUS_EX_BID_CANCELED:
			icon = (operation == Transaction::OPERATION_BUY) ? I_ICON_CANCEL_STOP_BUY : I_ICON_CANCEL_STOP_SELL;
			break;
		case Transaction::STATUS_EX_BID_EXECUTED:
		case Transaction::STATUS_EXECUTED:
			icon = (operation == Transaction::OPERATION_BUY) ? I_ICON_STOP_BUY : I_ICON_STOP_SELL;
			break;
		} // switch (status)
	} // if (action == Transaction::NEW_ORDER)
	return icon;
}

void CMyTradeListCtrl::SetItemIcon(int iItem, int action, int status, int operation)
{
	int icon = GetIconIndex(action, status, operation);
	// Устанавливаем соответвстующий значок:
	this->SetItem(iItem, COL_ICON, LVIF_IMAGE, NULL, icon, 0, 0, 0); // вместо COL_ICON нужно подставлять реальный номер колонки!
}

void CMyTradeListCtrl::DoSort(const CMyTradeListCtrl::Sort * pSort, BOOL updateIndex)
{
	if (this->flags & CMyTradeListCtrl::F_SORT)
	{
		if (pSort != NULL)
			this->SetSort(*pSort);
		this->SortItems(FuncCompareItems, (DWORD_PTR)&this->GetSorting());
		if (updateIndex)
			UpdateColumnIndex();
	}
}

void CMyTradeListCtrl::DoSort(int iItem, int descending)
{
	if (iItem >= 0)
	{
		iItem = IndexToColumn(iItem);
		if (sort.iItem != iItem)
		{
			OnColumnClick(iItem);
			DoSort();
		}
	}
	if (descending >= 0)
	{
		if (sort.descending != descending)
		{
			OnColumnClick(sort.iItem);
			DoSort();
		}
	}
}

void CMyTradeListCtrl::DoScroll()
{
	if (this->flags & CMyTradeListCtrl::F_SCROLL)
	{
		// Делаем прокрутку на добавленный элемент:
		RECT rc;
		int count = this->GetItemCount();
		this->GetItemRect(0, &rc, LVIR_BOUNDS);
		this->PostMessage(LVM_SCROLL , 0, LPARAM((rc.bottom-rc.top)*(count-1)));
	}
}

void CMyTradeListCtrl::SetNoSort(int set)
{
	if (set)
		this->flags &= ~CMyTradeListCtrl::F_SORT;
	else
		this->flags |= CMyTradeListCtrl::F_SORT;
}

void CMyTradeListCtrl::SetNoScroll(int set)
{
	if (set)
		this->flags &= ~CMyTradeListCtrl::F_SCROLL;
	else
		this->flags |= CMyTradeListCtrl::F_SCROLL;
}

int CMyTradeListCtrl::AddTransaction(const Transaction * pTransaction, const QuoteTable::Properties * pProperties, int sort, int scroll)
{
	int n = 0;
	int iItem;

	int count = this->GetItemCount();
	iItem = count;
	iItem = this->InsertItem(iItem, TEXT(""));
	this->SetItemData(iItem, (DWORD_PTR)pTransaction);

	UpdateColumns(iItem, pTransaction, pProperties);

	if (sort)
		DoSort();

	if (scroll)
		DoScroll();

	return n;
}

int CMyTradeListCtrl::UpdateTransaction(const Transaction * pTransaction, const QuoteTable::Properties * pProperties, int sort, int scroll)
{
	int status = E_INVALIDARG;
	int iItem = -1;
	int count = this->GetItemCount();
	for (int i = 0; i < count; i++)
	{
		Transaction * pTa = (Transaction*)this->GetItemData(i);
		if (pTa == pTransaction)
		{
			iItem = i;
			break;
		}
	} // for (i)

	if (iItem < 0)
		return status;

	for (int iCol = COL_FIRST; iCol < NR_COLS_MAX; iCol++)
	{
		int index = ColumnToIndex(iCol);
		UpdateColumnItemByIndex(iCol, iItem, index, pTransaction, pProperties);
	} // for (iCol)

	if (sort)
		DoSort();

	status = S_OK;

	return status;
}

void CMyTradeListCtrl::GetSort(Sort & sort) const 
{
	sort = this->sort; 
}

void CMyTradeListCtrl::SetSort(const Sort & sort)
{
	this->sort = sort;
}

int CMyTradeListCtrl::IndexToColumn(int index) const
{
	int column = -1;
	CHeaderCtrl header = GetHeader();
	int count = header.GetItemCount();
	for (int iCol = COL_FIRST; iCol < count; iCol++)
	{
		int col = ColumnToIndex(iCol);
		if (col == index)
		{
			column = iCol;
			break;
		}
	}
	return column;
}

int CMyTradeListCtrl::ColumnToIndex(int column) const
{
	CHeaderCtrl header = GetHeader();
	HDITEM hdi;
	hdi.mask = HDI_LPARAM;
	header.GetItem(column, &hdi);
	int index = static_cast<int>(hdi.lParam);
	return index;
}
