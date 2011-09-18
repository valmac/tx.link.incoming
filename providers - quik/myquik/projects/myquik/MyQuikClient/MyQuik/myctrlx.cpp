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
 *  myctrlx.cpp
 */

#include "stdafx.h"
#include "myctrlx.h"
#include "resource.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

//
// class CMyEdit
//

CMyEdit::CMyEdit()
{
}

CMyEdit::~CMyEdit()
{
}

void CMyEdit::Push(ListOfUserValues * pList)
{
	TSTRING_SMALL2(str, size);
	TCHAR * pEnd;
	if (this->GetWindowText(str, size))
	{
		double d = _tcstod(str, &pEnd);
		pList->Add(d);
	}
}

void CMyEdit::Pop(ListOfUserValues * pList, const QuoteTable::Properties * pProperties)
{
	TSTRING_SMALL2(str, size);	

	double d = pList->GetCurrentValue();
	if (d > 0)
		FormatString(str, size, d, pProperties ? pProperties->price.nd : 0);
	else
		_stprintf_s(str, size, TEXT(""));
	
	this->SetWindowText(str);		
	this->SetSel(this->LineLength(), -1);
}
#if 0
int CMyEdit::QueryMenu(CMenu & menu)
{
#if USE_STATIC_MENU_USER_VALUES
	CMenu & root = this->menu;
	if (! root.IsMenu())
		root.LoadMenu(IDR_MENU_USER_VALUES);
	menu = root.GetSubMenu(0);
#endif
	return 0;
}

int CMyEdit::ReleaseMenu(CMenu & menu)
{
#if USE_STATIC_MENU_USER_VALUES
	menu.Detach();
#endif
	return 0;
}
#endif
void CMyEdit::OnDeltaposSpinPrice(LPNMUPDOWN pNMUpDown, const QuoteTable::Properties * pProperties)
{
	if (pProperties != NULL)
	{
		TSTRING_SMALL2(str, size);
		GetWindowText(str, size);
		TCHAR * pEnd;
		double d = _tcstod(str, &pEnd);
		double step = pProperties->price.step;

		if (pNMUpDown->iDelta < 0)
			d += step;
		else
		{
			if (d - step > 0)
				d -= step;
		}
		FormatString(str, size, d, pProperties->price.nd);
		SetWindowText(str);
	} // if (pProperties != NULL)
}

void CMyEdit::OnDeltaposSpinQuantity(LPNMUPDOWN pNMUpDown, const QuoteTable::Properties * pProperties)
{
	TSTRING_SMALL2(str, size);
	TCHAR * pEnd;
	double d;
	int len = GetWindowText(str, size);
	if (len == 0 && pProperties != NULL)
		d = pProperties->trading.quantity;
	else
	{
		d = _tcstod(str, &pEnd); // StrToInt
		if (pNMUpDown->iDelta < 0)
			d++;
		else
		{
			if (d > 1)
				d--;
		}
	}
	_stprintf_s(str, size, TEXT("%.0f"), d);
	SetWindowText(str);
}

void CMyEdit::OnRButtonUp(UINT nFlags, CPoint point)
{
	CWindow wnd = GetParent();
	GetCursorPos(&point);
	wnd.ScreenToClient(&point);
	::SendMessage(wnd, WM_RBUTTONUP, 0, MAKELPARAM(LOWORD(point.x), LOWORD(point.y)));	
}

//
// class CMyButton
//

CMyButton::CMyButton()
{
}

CMyButton::~CMyButton()
{
}

void CMyButton::OnRButtonUp(UINT nFlags, CPoint point)
{
	CWindow wnd = GetParent();
	GetCursorPos(&point);
	wnd.ScreenToClient(&point);
	::SendMessage(wnd, WM_RBUTTONUP, 0, MAKELPARAM(LOWORD(point.x), LOWORD(point.y)));	
}
