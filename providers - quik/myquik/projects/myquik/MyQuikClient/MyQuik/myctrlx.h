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
 *  myctrlx.h
 */

#pragma once

#include "tables.h"

//
// class CMyEdit
//
class CMyEdit : public CWindowImpl<CMyEdit, CEdit>
{
// Constructors
public:
	CMyEdit();
	~CMyEdit();

	void Push(ListOfUserValues * pList);
	void Pop(ListOfUserValues * pList, const QuoteTable::Properties * pProperties);
#if 0
	int QueryMenu(CMenu & menu);
	int ReleaseMenu(CMenu & menu);
#endif
	void OnDeltaposSpinPrice(LPNMUPDOWN pNMUpDown, const QuoteTable::Properties * pProperties);
	void OnDeltaposSpinQuantity(LPNMUPDOWN pNMUpDown, const QuoteTable::Properties * pProperties);

protected:
#if USE_STATIC_MENU_USER_VALUES && 0
	CMenu menu;
#endif

protected:
	BEGIN_MSG_MAP_EX(CMyEdit)
		MSG_WM_RBUTTONUP(OnRButtonUp)
	END_MSG_MAP()

	void OnRButtonUp(UINT nFlags, CPoint point);
};

//
// class CMyButton
//
class CMyButton : public CWindowImpl<CMyButton, CButton>
{
// Constructors
public:
	CMyButton();
	~CMyButton();

protected:
	BEGIN_MSG_MAP_EX(CMyButton)
		MSG_WM_RBUTTONUP(OnRButtonUp)
	END_MSG_MAP()

	void OnRButtonUp(UINT nFlags, CPoint point);
};
