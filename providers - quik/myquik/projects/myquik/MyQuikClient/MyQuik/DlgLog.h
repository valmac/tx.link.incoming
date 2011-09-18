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
 *  DlgLog.h
 */

#pragma once

//
// class CMyListBoxLog
//
class CMyListBoxLog : public CWindowImpl<CMyListBoxLog, CListBox>
{
// Constructors
public:
	CMyListBoxLog();

protected:
	BEGIN_MSG_MAP_EX(CMyListBoxLog)
		MSG_WM_RBUTTONUP(OnRButtonUp)
		MSG_OCM_DRAWITEM(DrawItem)
		DEFAULT_REFLECTION_HANDLER()
	END_MSG_MAP()

	void OnRButtonUp(UINT nFlags, CPoint point);

	void DrawItem(int nIDCtl, LPDRAWITEMSTRUCT lpDrawItemStruct);
};

//
// CDlgLog dialog
//
class CDlgLog : public CDialogImpl<CDlgLog>, public CWinDataExchange<CDlgLog>
{
public:
	CDlgLog();
	virtual ~CDlgLog();

// Dialog Data
	enum { IDD = IDD_LOG };

	LPCTSTR GetName() const { return TEXT("DlgLog"); }

	void PutMessage(LPCTSTR msg, int code = S_OK);

protected:
	HANDLE m_hLog;
	CMyListBoxLog m_listBox;

protected:
	void Resize();
	void UpdateHorizontalExtent();

protected:
	BEGIN_MSG_MAP_EX(CDlgLog)
		MSG_WM_INITDIALOG(OnInitDialog)
		MSG_WM_SIZE(OnSize)
		MESSAGE_HANDLER(WM_CLOSE, OnClose)
		MSG_WM_DESTROY(OnDestroy)
		COMMAND_ID_HANDLER_EX(IDCANCEL, OnCancel)
		COMMAND_ID_HANDLER_EX(ID_LOG_CLOSE, OnLogClose)
		MESSAGE_RANGE_HANDLER(UM_BASE + 1, UM_LAST - 1, OnWndMsg)
		MSG_WM_ACTIVATE(OnActivate)
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	BEGIN_DDX_MAP(CDlgLog)
		DDX_CONTROL(IDC_LIST1, m_listBox)
	END_DDX_MAP()

protected:
	BOOL OnInitDialog(CWindow wndFocus, LPARAM lInitParam);

	void OnSize(UINT nType, CSize size);	
	void OnLogClose(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnCancel(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnDestroy();
	LRESULT OnClose(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);

	LRESULT OnWndMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);

	void OnActivate(UINT nState, BOOL bMinimized, CWindow wndOther);
};
