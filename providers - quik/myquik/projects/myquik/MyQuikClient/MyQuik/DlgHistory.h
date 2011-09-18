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
 *  DlgHistory.h
 */

#pragma once

#include "MyTradeListCtrl.h"

class CTradeDlg;
class History;

enum {
	I_TREE_ROOT,
	I_TREE_TODAY,
	I_TREE_YESTERDAY,
	I_TREE_2DAYS,
	I_TREE_7DAYS,
	I_TREE_31DAYS,
	I_TREE_MONTH,
	I_TREE_YEAR,
	I_TREE_LASTYEAR,
	I_TREE_OLDEST,
	I_TREE_FULL,
	I_TREE_LAST,
	I_TREE_NODATA,
};

//
// class CHistoryTreeCtrl
//
class CHistoryTreeCtrl : public CWindowImpl<CHistoryTreeCtrl, CTreeViewCtrlEx>
{
// Constructors
public:
	CHistoryTreeCtrl();

protected:
	BEGIN_MSG_MAP_EX(CHistoryTreeCtrl)
		MSG_WM_MOUSEMOVE(OnMouseMove)
		MSG_WM_LBUTTONUP(OnLButtonUp)
	END_MSG_MAP()

	void OnMouseMove(UINT nFlags, CPoint point);
	void OnLButtonUp(UINT nFlags, CPoint point);
};

//
// class CHistoryListCtrl
//
class CHistoryListCtrl : public CMyTradeListCtrl
{
// Constructors
public:
	CHistoryListCtrl();

protected:
	BEGIN_MSG_MAP_EX(CHistoryListCtrl)
		MSG_WM_MOUSEMOVE(OnMouseMove)
		MSG_WM_LBUTTONUP(OnLButtonUp)
		CHAIN_MSG_MAP(CMyTradeListCtrl)
	END_MSG_MAP()

	void OnMouseMove(UINT nFlags, CPoint point);
	void OnLButtonUp(UINT nFlags, CPoint point);
};

//
// class CMyDelimiter
//
class CMyDelimiter : public CWindowImpl<CMyDelimiter, CStatic>
{
// Constructors
public:
	CMyDelimiter();

	int GetWidth() const;

protected:
	BEGIN_MSG_MAP_EX(CMyDelimiter)
		MSG_WM_MOUSEMOVE(OnMouseMove)
		MSG_WM_LBUTTONDOWN(OnLButtonDown)
		MSG_WM_LBUTTONUP(OnLButtonUp)
		MSG_WM_PAINT(OnPaint)
	END_MSG_MAP()

	void OnMouseMove(UINT nFlags, CPoint point);
	void OnLButtonDown(UINT nFlags, CPoint point);
	void OnLButtonUp(UINT nFlags, CPoint point);
	void OnPaint(CDCHandle );
};

//
// class CHistoryEdit
//
class CHistoryEdit : public CWindowImpl<CHistoryEdit, CEdit>
{
// Constructors
public:
	CHistoryEdit();
	~CHistoryEdit();

protected:
	BEGIN_MSG_MAP_EX(CHistoryEdit)
		MSG_WM_MOUSEMOVE(OnMouseMove)
		MSG_WM_LBUTTONUP(OnLButtonUp)
	END_MSG_MAP()

	void OnMouseMove(UINT nFlags, CPoint point);
	void OnLButtonUp(UINT nFlags, CPoint point);
};

//
// CDlgHistory dialog
//

class CDlgHistory : public CDialogImpl<CDlgHistory>, public CWinDataExchange<CDlgHistory>
{
	enum Mode {
		MODE_NORMAL,
		MODE_REMOVE,
		MODE_INIT,
	};

public:
	CDlgHistory();
	virtual ~CDlgHistory();

// Dialog Data
	enum { IDD = IDD_HISTORY };

	void Attach(History * history);

	LPCTSTR GetName() const { return TEXT("DlgHistory"); };

	int Update(const Transaction * pTa, int flags);
	int Remove(const Transaction * pTa, int iItem);

	void DoSort();
	void DoScroll();

	void SetNoSort(int set = 1);
	void SetNoScroll(int set = 1);

	void SetWidthTreeCtrl(int width);
	int GetModifiedWidthTreeCtrl() const;

	void UpdateBkColor();

	void UpdateColumns(const Settings::Presentation::ListOfDeals & settings, const Settings::Presentation::ListOfDeals & prev);
	int UpdateColumnIndex();

protected:
	void InitTreeCtrl();
	void InitListCtrl();

	void UpdateTreeCtrl();
	void UpdateDescription(int iItem = -1);
	void UpdateDescription(LPCTSTR msg);

	LPCTSTR UpdateItemText(HTREEITEM hItem, LPTSTR text, int size, int inc);
	LPCTSTR UpdateItemTextEx(HTREEITEM hItem, LPTSTR text, int size, int inc, int sync, int & val);

	void UpdateMenuTree(CMenu & menu);
	void UpdateMenuList(CMenu & menu);
	void UpdateMenuSort(CMenu & menu);

	LPCTSTR GetTreeText(int index);

	void Resize(int cx = 0, int cy = 0, int width = 0);

	int RemoveSelectedItems(int flags = 0);

	void SelectAll();
	int CheckListSingleSelection() const;

	void OnSelectFilter(int index);

	History * GetHistory() { return m_pHistory; }

	void SetNoData(int nodata = TRUE) { m_treeCtrlCtrl.nodata = nodata; }
	BOOL HaveData() const { return !m_treeCtrlCtrl.nodata; }

	void SetNoFilters();
	void SetFilter(int index, BOOL set = TRUE);
	int CheckFilter(int index) const;

	void SetSizing(BOOL set = TRUE);

	void Set(Mode mode) { m_mode = mode; }

	int SelectByDate(const SYSTEMTIME & st);

	void ShowTreeMenu();
	void ShowListMenu();

protected:
	History * m_pHistory;

	struct TreeCtrlCtrl {
		int resizing;
		int modified;
		int width;
		int nodata;
	} m_treeCtrlCtrl;

	struct FilterCtrl {
		DWORD mask;
	} m_filters;

	CHistoryTreeCtrl m_treeCtrl;
	CHistoryListCtrl m_listCtrl;
	CMyDelimiter m_delimiter;
	CHistoryEdit m_edit;

	CImageList m_imgList;

	BOOL m_initialized;

	Mode m_mode; 

protected:	
	BEGIN_MSG_MAP_EX(CDlgHistory)
		MSG_WM_INITDIALOG(OnInitDialog)
		MESSAGE_HANDLER(WM_CLOSE, OnClose)
		MSG_WM_DESTROY(OnDestroy)
		MSG_WM_SIZE(OnSize)
		MSG_WM_MOUSEMOVE(OnMouseMove)
		MSG_WM_LBUTTONUP(OnLButtonUp)
		COMMAND_ID_HANDLER_EX(IDCANCEL, OnCancel)
		NOTIFY_HANDLER_EX(IDC_TREE1, TVN_SELCHANGED, OnTvnSelchangedTree1)
		NOTIFY_HANDLER_EX(0, HDN_ITEMCLICK, OnHdnItemclickList1)
		COMMAND_HANDLER_EX(IDC_EDIT1, EN_SETFOCUS, OnEnSetfocusEdit1)
		NOTIFY_HANDLER_EX(IDC_LIST1, LVN_ITEMCHANGED, OnLvnItemchangedList1)
		COMMAND_ID_HANDLER_EX(ID_HISTORY_LIST_REMOVE_ITEM, OnHistoryListRemoveItem)
		NOTIFY_HANDLER_EX(IDC_LIST1, NM_RCLICK, OnNMRClickList1)
		NOTIFY_HANDLER_EX(IDC_LIST1, LVN_KEYDOWN, OnLvnKeydownList1)
		NOTIFY_HANDLER_EX(IDC_TREE1, NM_RCLICK, OnNMRClickTree1)
		NOTIFY_HANDLER_EX(IDC_TREE1, NM_CLICK, OnNMClickTree1)
		COMMAND_ID_HANDLER_EX(ID_HISTORY_TREE_REMOVE, OnHistoryTreeRemove)
		NOTIFY_HANDLER_EX(IDC_TREE1, TVN_KEYDOWN, OnTvnKeydownTree1)
		COMMAND_ID_HANDLER_EX(ID_HISTORY_TREE_EXPAND, OnHistoryTreeExpand)
		COMMAND_ID_HANDLER_EX(ID_HISTORY_TREE_COLLAPSE, OnHistoryTreeCollapse)
		NOTIFY_HANDLER_EX(IDC_DATETIMEPICKER, DTN_DATETIMECHANGE, OnDtnDatetimechangeDatetimepicker)
		MESSAGE_HANDLER(WM_COMMAND, OnCmdMsg)
		MESSAGE_RANGE_HANDLER(UM_BASE + 1, UM_LAST - 1, OnWndMsg)
		MSG_WM_ACTIVATE(OnActivate)
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	BEGIN_DDX_MAP(CDlgHistory)
		DDX_CONTROL(IDC_TREE1, m_treeCtrl);
		DDX_CONTROL(IDC_LIST1, m_listCtrl);
		DDX_CONTROL(IDC_EDIT1, m_edit);
		DDX_CONTROL(IDC_DELIMITER, m_delimiter);
	END_DDX_MAP()

protected:
	BOOL OnInitDialog(CWindow wndFocus, LPARAM lInitParam);

	void OnSize(UINT nType, CSize size);
	void OnMouseMove(UINT nFlags, CPoint point);
	void OnCancel(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnDestroy();
	LRESULT OnClose(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);

	LRESULT OnTvnSelchangedTree1(NMHDR *pNMHDR);
	LRESULT OnHdnItemclickList1(NMHDR *pNMHDR);
	void OnEnSetfocusEdit1(UINT uNotifyCode, int nID, CWindow wndCtl);
	
	LRESULT OnLvnItemchangedList1(NMHDR *pNMHDR);
	LRESULT OnNMRClickList1(NMHDR *pNMHDR);
	LRESULT OnLvnKeydownList1(NMHDR *pNMHDR);	
	LRESULT OnNMRClickTree1(NMHDR *pNMHDR);
	LRESULT OnNMClickTree1(NMHDR *pNMHDR);
	LRESULT OnTvnKeydownTree1(NMHDR *pNMHDR);

	void OnHistoryTreeExpand(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnHistoryTreeCollapse(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnHistoryTreeRemove(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnHistoryListRemoveItem(UINT uNotifyCode, int nID, CWindow wndCtl);

	LRESULT OnWndMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
	LRESULT OnCmdMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);

	void OnLButtonUp(UINT nFlags, CPoint point);
	LRESULT OnDtnDatetimechangeDatetimepicker(NMHDR *pNMHDR);

	void OnActivate(UINT nState, BOOL bMinimized, CWindow wndOther);
};
