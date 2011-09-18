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
 *  PropertyPageInstruments.h
 */

#pragma once

#define USE_MYLISTBOX 0

#if USE_MYLISTBOX
class CMyListBox : public CWindowImpl<CMyListBox, CListBox>
#else
class CMyListBox : public CWindowImpl<CMyListBox, CListViewCtrl>
#endif
{
// Constructors
public:
	CMyListBox();

	void SetCurrent(int cur) { this->current = cur; }
	int GetCurrent() const { return this->current; }

protected:
	int current;

public:
	BEGIN_MSG_MAP_EX(CMyListBox)
#if USE_MYLISTBOX		
		MSG_WM_RBUTTONUP(OnRButtonUp)
#endif
		MESSAGE_HANDLER(WM_KEYUP, OnKeyUp)
		MSG_OCM_DRAWITEM(DrawItem)
		DEFAULT_REFLECTION_HANDLER()
	END_MSG_MAP()

#if USE_MYLISTBOX
	void OnRButtonUp(UINT nFlags, CPoint point);
#endif
	LRESULT OnKeyUp(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
	void DrawItem(int nIDCtl, LPDRAWITEMSTRUCT lpDrawItemStruct);
};

// CPropertyPageInstruments dialog

class CPropertyPageInstruments : public CPropertyPageImpl<CPropertyPageInstruments>, 
	public CWinDataExchange<CPropertyPageInstruments>
{
public:
	CPropertyPageInstruments(LPCTSTR name);
	virtual ~CPropertyPageInstruments();

// Dialog Data
	enum { IDD = IDD_SETTINGS_INSTRUMENTS };

protected:
	BOOL m_initialized;
	CString m_name;	
	CMyListBox m_listBox;
	CToolTipCtrl m_hints;

protected:
	int AddStringToListBox(LPCTSTR name, LPCTSTR seccode);
	void SelectInstrument(LPCTSTR name, const QuoteTable::Properties * pProperties, int select = 1);
	void UpdateListOfInstruments(BOOL clear = TRUE);
	void UpdateButtons(int iSel = -1);

	int GetCurSel() const;
	void SetCurSel(int iSel);
	int GetItemCount() const;
	int GetItemText(int i, LPTSTR dst, size_t size);

protected:
	BEGIN_MSG_MAP_EX(CPropertyPageInstruments)
		MSG_WM_INITDIALOG(OnInitDialog)
#if USE_MYLISTBOX
		COMMAND_HANDLER_EX(IDC_LIST1, LBN_SELCHANGE, OnLbnSelchangeList1)
		COMMAND_HANDLER_EX(IDC_LIST1, LBN_DBLCLK, OnLbnDblclkList1)
#else
		NOTIFY_HANDLER_EX(IDC_LIST2, LVN_ITEMCHANGED, OnLvnItemchangedList2)
		NOTIFY_HANDLER_EX(IDC_LIST2, NM_DBLCLK, OnNMDblClickList2)
		NOTIFY_HANDLER_EX(IDC_LIST2, NM_RCLICK, OnNMRClickList2)
#endif
		COMMAND_HANDLER_EX(IDC_BUTTON_MODIFY, BN_CLICKED, OnBnClickedButtonModify)
		COMMAND_HANDLER_EX(IDC_BUTTON_SELECT, BN_CLICKED, OnBnClickedButtonSelect)
		COMMAND_HANDLER_EX(IDC_BUTTON_ADD, BN_CLICKED, OnBnClickedButtonAdd)
		COMMAND_ID_HANDLER_EX(ID_ADD_INSTRUMENT, OnAddInstrument)
		COMMAND_ID_HANDLER_EX(ID_REMOVE_INSTRUMENT, OnRemoveInstrument)
		COMMAND_ID_HANDLER_EX(ID_INSTRUMENT_PROPERTIES, OnInstrumentProperties)
		COMMAND_ID_HANDLER_EX(ID_SELECT_INSTRUMENT, OnSelectInstrument)
		COMMAND_HANDLER_EX(IDC_BUTTON_REMOVE, BN_CLICKED, OnBnClickedButtonRemove)
		COMMAND_HANDLER_EX(IDC_BUTTON_EXPORT, BN_CLICKED, OnBnClickedButtonExport)
		COMMAND_HANDLER_EX(IDC_BUTTON_IMPORT, BN_CLICKED, OnBnClickedButtonImport)
		COMMAND_ID_HANDLER_EX(ID_REMOVE_INSTRUMENTS_ALL, OnRemoveInstrumentsAll)
		COMMAND_ID_HANDLER_EX(ID_EXPORT_INSTRUMENTS, OnExportInstruments)
		COMMAND_ID_HANDLER_EX(ID_IMPORT_INSTRUMENTS, OnImportInstruments)
		COMMAND_HANDLER_EX(IDC_BUTTON_ADD_COPY, BN_CLICKED, OnBnClickedButtonAddCopy)
		COMMAND_ID_HANDLER_EX(ID_ADD_INSTRUMENT_COPY, OnAddInstrumentCopy)
		MESSAGE_RANGE_HANDLER(UM_BASE + 1, UM_LAST - 1, OnWndMsg)
		CHAIN_MSG_MAP(CPropertyPageImpl<CPropertyPageInstruments>)
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	BEGIN_DDX_MAP(CPropertyPageInstruments)
#if USE_MYLISTBOX
		DDX_CONTROL(IDC_LIST1, m_listBox)
#else
		DDX_CONTROL(IDC_LIST2, m_listBox)
#endif
	END_DDX_MAP()

	BOOL OnInitDialog(CWindow wndFocus, LPARAM lInitParam);

	void OnBnClickedButtonModify(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonSelect(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonAdd(UINT uNotifyCode, int nID, CWindow wndCtl);

	void OnAddInstrument(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnRemoveInstrument(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnRemoveInstrumentsAll(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnInstrumentProperties(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnSelectInstrument(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonRemove(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonExport(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonImport(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnExportInstruments(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnImportInstruments(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonAddCopy(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnAddInstrumentCopy(UINT uNotifyCode, int nID, CWindow wndCtl);
#if USE_MYLISTBOX
	void OnLbnSelchangeList1(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnLbnDblclkList1(UINT uNotifyCode, int nID, CWindow wndCtl);
#else
	LRESULT OnLvnItemchangedList2(NMHDR *pNMHDR);
	LRESULT OnNMDblClickList2(NMHDR *pNMHDR);
	LRESULT OnNMRClickList2(NMHDR *pNMHDR);
#endif
	LRESULT OnWndMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);

	BOOL OnApply();
	BOOL OnQueryCancel();

};

typedef CPropertyPageImpl<CPropertyPageInstruments> CPropertyPageInstrumentsBase;
