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
 *  PropertyPageKeys.h
 */

#pragma once

#include "shortcuts.h"


class CEditableListCtrl : public CWindowImpl<CEditableListCtrl, CListViewCtrl>
{
	friend class CPropertyPageKeys;
public:
	CEditableListCtrl();

	int GetSelectedItem() const;
	int GetEditingItem() const;

	BOOL IsEditing() const;
	
	int BeginEdit(DWORD shortcut);
	int EndEdit();

protected:
	struct Edit {
		int iItem;
		int iSubItem;
	} m_edit;

	struct Shortcut {
		DWORD current;
		DWORD previos;
	} m_shortcut;

	CFont m_boldFont;

public:
	BEGIN_MSG_MAP_EX(CEditableListCtrl)
		MESSAGE_RANGE_HANDLER(WM_KEYDOWN, WM_KEYUP, OnKey)
		MESSAGE_RANGE_HANDLER(WM_SYSKEYDOWN, WM_SYSKEYUP, OnKey)
		MESSAGE_RANGE_HANDLER(WM_MOUSEFIRST+1, WM_MOUSELAST, OnMouse)
		MSG_OCM_DRAWITEM (DrawItem)
		DEFAULT_REFLECTION_HANDLER ()
	END_MSG_MAP()
	
	LRESULT OnKey(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
	LRESULT OnMouse(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);

	void OnSysCommand(UINT nID, LPARAM lParam);

	void DrawItem (UINT idCtrl, LPDRAWITEMSTRUCT lpDrawItemStruct);
};

// CPropertyPageKeys dialog

class CPropertyPageKeys : public CPropertyPageImpl<CPropertyPageKeys>, public CWinDataExchange<CPropertyPageKeys>
{
public:
	CPropertyPageKeys(const Settings::Shortcuts & settings);
	virtual ~CPropertyPageKeys();

	enum { IDD = IDD_SETTINGS_SHORTCUTS };

protected:
	CEditableListCtrl m_listOfShortcuts;
	Settings::Shortcuts m_settings;

protected:
	void InitCombo();
	int InitListOfShortcuts();
	void InitListOfShortcuts(my::ShortcutMap & layout, BOOL clear = FALSE);

	int BeginEdit();
	int EndEdit();

	BOOL IsEditingNow() const;

	void ClearShortcut();

	my::ShortcutMap & GetCurrentLayout() {return m_settings.layouts[m_settings.iCurrentLayout];}

protected:
	void SetModified(BOOL bChanged = TRUE);

protected:
	BEGIN_MSG_MAP_EX(CPropertyPageKeys)
		MSG_WM_INITDIALOG(OnInitDialog)
		COMMAND_HANDLER_EX(IDC_BUTTON_RESET_SHORTCUTS, BN_CLICKED, OnBnClickedButtonResetShortcuts)
		COMMAND_HANDLER_EX(IDC_BUTTON_EDIT, BN_CLICKED, OnBnClickedButtonEdit)
		COMMAND_HANDLER_EX(IDC_BUTTON_CLEAR, BN_CLICKED, OnBnClickedButtonClear)
		NOTIFY_HANDLER_EX(IDC_LIST_SHORTCUTS, NM_DBLCLK, OnNMDblclkListShortcuts)
		NOTIFY_HANDLER_EX(IDC_LIST_SHORTCUTS, NM_CLICK, OnNMClickListShortcuts)
		NOTIFY_HANDLER_EX(IDC_LIST_SHORTCUTS, NM_RCLICK, OnNMRClickListShortcuts)
		COMMAND_ID_HANDLER_EX(ID_SHORTCUTKEYS_EDIT, OnShortcutkeysEdit)
		COMMAND_ID_HANDLER_EX(ID_SHORTCUTKEYS_CLEAR, OnShortcutkeysClear)
		COMMAND_ID_HANDLER_EX(ID_SHORTCUTKEYS_DEFAULT, OnShortcutkeysDefault)
		NOTIFY_HANDLER_EX(IDC_LIST_SHORTCUTS, LVN_ITEMCHANGED, OnLvnItemchangedListShortcuts)
		COMMAND_HANDLER_EX(IDC_COMBO_SHORTCUTS, CBN_SELCHANGE, OnCbnSelchangeComboShortcuts)
		MESSAGE_HANDLER(UM_EDIT, OnWndMsg)
		CHAIN_MSG_MAP(CPropertyPageImpl<CPropertyPageKeys>)
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	BEGIN_DDX_MAP(CPropertyPageKeys)
		DDX_CONTROL(IDC_LIST_SHORTCUTS, m_listOfShortcuts)
	END_DDX_MAP()

	BOOL OnInitDialog(CWindow wndFocus, LPARAM lInitParam);

	void OnBnClickedButtonResetShortcuts(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonEdit(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonClear(UINT uNotifyCode, int nID, CWindow wndCtl);

	LRESULT OnNMDblclkListShortcuts(NMHDR *pNMHDR);
	LRESULT OnNMClickListShortcuts(NMHDR *pNMHDR);
	LRESULT OnNMRClickListShortcuts(NMHDR *pNMHDR);	
	LRESULT OnLvnItemchangedListShortcuts(NMHDR *pNMHDR);

	void OnCbnSelchangeComboShortcuts(UINT uNotifyCode, int nID, CWindow wndCtl);

	void OnShortcutkeysEdit(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnShortcutkeysClear(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnShortcutkeysDefault(UINT uNotifyCode, int nID, CWindow wndCtl);

	LRESULT OnWndMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);

	BOOL OnApply();
	BOOL OnQueryCancel();
	BOOL OnKillActive();
};

typedef CPropertyPageImpl<CPropertyPageKeys> CPropertyPageKeysBase;
