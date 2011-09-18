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
 *  MyTradeListCtrl.h
 */

#pragma once

#include "tables.h"
#include "transactions.h"

//
// class CMyTradeListCtrl
//

class CMyTradeListCtrl : public CWindowImpl<CMyTradeListCtrl, CListViewCtrl>
{
	enum Flags {
		F_SORT = 0x1,
		F_SCROLL = 0x2,
	};

// Construction
public:
	struct Sort {
		int type;
		int descending;
		int iItem;
	};

	CMyTradeListCtrl();

	const Sort & GetSorting() const { return this->sort; }

	LPCTSTR ColumnIndexToName(int index);
	int GetColumnWidth(int index, LPCTSTR columnName);

	static int LoadColumnWidth(LPCTSTR name, LPCTSTR colName, int defaultWidth);
	static int SaveColumnWidth(LPCTSTR name, LPCTSTR colName, int width);

	void SaveListCtrlParams();

	int UpdateColumn(int col);
	int UpdateColumnItem(int col, int iItem, const Transaction * pTransaction, const QuoteTable::Properties * pProperties);
	int UpdateColumnItemByIndex(int col, int iItem, int index, const Transaction * pTransaction, const QuoteTable::Properties * pProperties);
	int UpdateColumns(int iItem, const Transaction * pTransaction, const QuoteTable::Properties * pProperties);
	void UpdateColumns(const Settings::Presentation::ListOfDeals & settings, const Settings::Presentation::ListOfDeals & prev);

	int UpdateColumnIndex();
	int UpdateColumnItemIndex(int iCol, int iItem);

	void UpdateBkColor();

	int AddTransaction(const Transaction * pTransaction, const QuoteTable::Properties * pProperties, int sort = 1, int scroll = 1);
	int UpdateTransaction(const Transaction * pTransaction, const QuoteTable::Properties * pProperties, int sort = 1, int scroll = 1);

	void DoSort(const CMyTradeListCtrl::Sort * pSort = NULL, BOOL updateIndex = TRUE);
	void DoSort(int iItem, int descending);
	void DoScroll();

	void SetNoSort(int set = 1);
	void SetNoScroll(int set = 1);

	void SetItemIcon(int iItem, int action, int status, int operation);
	static int GetIconIndex(int action, int status, int operation);

	void SetParentName(LPCTSTR name) { m_parent.name = CString(name); }
	LPCTSTR GetParentName() const { return m_parent.name; }

	void GetSort(Sort & sort) const;
	void SetSort(const Sort & sort);

	int IndexToColumn(int index) const;
	int ColumnToIndex(int column) const;

protected:
	void OnColumnClick(int iCol);

protected:
	Sort sort;
	struct Parent {
		CString name;
	} m_parent;

	int flags;

protected:
	BEGIN_MSG_MAP_EX (CMyTradeListCtrl)
		REFLECTED_NOTIFY_CODE_HANDLER_EX (LVN_COLUMNCLICK, OnLvnColumnclick)
		MSG_OCM_DRAWITEM (DrawItem)
		DEFAULT_REFLECTION_HANDLER ()
	END_MSG_MAP ()

	LRESULT OnLvnColumnclick (NMHDR* phdr);
	void DrawItem (UINT idCtrl, LPDRAWITEMSTRUCT lpDrawItemStruct);
};
