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
 *  PropertyPageListOfDeals.h
 */

#pragma once

#include "paint.h"

class CColorListBox : public CWindowImpl<CColorListBox, CListViewCtrl>
{
public:
	CColorListBox();

	void SetColors(Settings::Presentation::ListOfDeals::Colors & colors) { m_pColors = &colors; }

protected:
	const Settings::Presentation::ListOfDeals::Colors * m_pColors;

public:
	BEGIN_MSG_MAP_EX (CMyTradeListCtrl)
		MSG_OCM_DRAWITEM (DrawItem)
		DEFAULT_REFLECTION_HANDLER ()
	END_MSG_MAP ()

	void DrawItem (UINT idCtrl, LPDRAWITEMSTRUCT lpDrawItemStruct);
};

// CPropertyPageListOfDeals dialog

class CPropertyPageListOfDeals : public CPropertyPageImpl<CPropertyPageListOfDeals>, 
	public CWinDataExchange<CPropertyPageListOfDeals>
{
public:
	CPropertyPageListOfDeals(const Settings::Presentation::ListOfDeals & settings);
	virtual ~CPropertyPageListOfDeals();

// Dialog Data
	enum { IDD = IDD_SETTINGS_LIST };

	static LPCTSTR GetPropertyName(unsigned int index);

protected:
	Settings::Presentation::ListOfDeals m_settings;

	CColorListBox m_listColors;
	CListViewCtrl m_listColumns;

	int m_initialized;
	int m_updating;

protected:
	CToolTipCtrl m_hints;

protected:
	void InitListColumns();	
	void InitListColors();
	void InitComboAlignment();

	void SetModified(BOOL bChanged = TRUE);

	void UpdateSettings(int set = SET);
	void UpdateColumns(int set);
	void UpdateColumnsInv(int set, int iItem);

	void EnableButtonUp(BOOL enable = TRUE);
	void EnableButtonDown(BOOL enable = TRUE);
	void EnableButtonUpDown(int iItem);
	
	void EnableBackground(BOOL enable);
	void EnableColorText(BOOL enable);

	void SwapItems(int i1, int i2, int iSel);

	void ApplyColors(int index);
	void ResetColors(int index = -1);

	void OnColor(DWORD & color);

protected:
	enum {
		F_DRAW_LIST = 0x1,
		F_DRAW_TEXT_COLOR = 0x2,
		F_DRAW_BACKGROUND = 0x4,
		F_DRAW_ALL = F_DRAW_LIST|F_DRAW_TEXT_COLOR|F_DRAW_BACKGROUND,
	};	
	void Invalidate2() { m_painter.Invalidate(F_DRAW_ALL, FALSE); }
	void Invalidate2(HWND hWnd, int flags = F_DRAW_ALL) { m_painter.Invalidate(hWnd, flags, FALSE); }

protected:
	my::Painter m_painter;

protected:
	BEGIN_MSG_MAP_EX(CPropertyPageListOfDeals)
		MSG_WM_INITDIALOG(OnInitDialog)
		COMMAND_HANDLER_EX(IDC_BUTTON_UP, BN_CLICKED, OnBnClickedButtonUp)
		COMMAND_HANDLER_EX(IDC_BUTTON_DOWN, BN_CLICKED, OnBnClickedButtonDown)
		NOTIFY_HANDLER_EX(IDC_LIST_COLUMNS, NM_DBLCLK, OnNMDblclkListColumns)
		NOTIFY_HANDLER_EX(IDC_LIST_COLUMNS, NM_CLICK, OnNMClickListColumns)
		NOTIFY_HANDLER_EX(IDC_LIST_COLUMNS, LVN_ITEMCHANGED, OnLvnItemchangedListColumns)
		MSG_WM_PAINT(OnPaint)
		COMMAND_HANDLER_EX(IDC_COLOR_BACKGROUND, STN_CLICKED, OnStnClickedColorBackground)
		COMMAND_HANDLER_EX(IDC_COLOR_TEXT, STN_CLICKED, OnStnClickedColorText)
		COMMAND_HANDLER_EX(IDC_CHECK_USE_DIFFERENT_COLORS, BN_CLICKED, OnBnClickedCheckUseDifferentColors)
		NOTIFY_HANDLER_EX(IDC_LIST_COLORS, LVN_ITEMCHANGED, OnLvnItemchangedListColors)
		NOTIFY_HANDLER_EX(IDC_LIST_COLORS, NM_RCLICK, OnNMRClickListColors)
		COMMAND_HANDLER_EX(IDC_CHECK_USE_DIFFERENT_COLORS_TEXT, BN_CLICKED, OnBnClickedCheckUseDifferentColorsText)
		COMMAND_HANDLER_EX(IDC_COMBO1, CBN_SELCHANGE, OnCbnSelchangeComboAlignment)
		MESSAGE_HANDLER(WM_COMMAND, OnCmdMsg)
		CHAIN_MSG_MAP(CPropertyPageImpl<CPropertyPageListOfDeals>)
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	BEGIN_DDX_MAP(CPropertyPageListOfDeals)
	//	DDX_CONTROL(IDC_LIST_COLUMNS, m_listColumns)
		DDX_CONTROL(IDC_LIST_COLORS, m_listColors)
	END_DDX_MAP()

	BOOL OnInitDialog(CWindow wndFocus, LPARAM lInitParam);

	LRESULT OnNMDblclkListColumns(NMHDR *pNMHDR);
	LRESULT OnNMClickListColumns(NMHDR *pNMHDR);
	LRESULT OnLvnItemchangedListColumns(NMHDR *pNMHDR);
	void OnPaint(CDCHandle );
	void OnStnClickedColorBackground(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnStnClickedColorText(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckUseDifferentColors(UINT uNotifyCode, int nID, CWindow wndCtl);
	LRESULT OnLvnItemchangedListColors(NMHDR *pNMHDR);
	LRESULT OnNMRClickListColors(NMHDR *pNMHDR);
	void OnBnClickedCheckUseDifferentColorsText(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonUp(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonDown(UINT uNotifyCode, int nID, CWindow wndCtl);

	void OnCbnSelchangeComboAlignment(UINT uNotifyCode, int nID, CWindow wndCtl);

	LRESULT OnCmdMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);

	BOOL OnApply();
	BOOL OnQueryCancel();
	BOOL OnSetActive();
};

typedef CPropertyPageImpl<CPropertyPageListOfDeals> CPropertyPageListOfDealsBase;
