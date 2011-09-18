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
 *  PropertyPageOSD.h
 */

#pragma once

#include "paint.h"

////
//// class CMyListBoxOSD
////
//class CMyListBoxOSD : public CWindowImpl<CMyListBoxOSD, CListBox>
//{
//// Constructors
//public:
//	CMyListBoxOSD();
//
//protected:
//	BEGIN_MSG_MAP_EX(CMyListBoxOSD)
//		MSG_OCM_DRAWITEM(DrawItem)
//		DEFAULT_REFLECTION_HANDLER()
//	END_MSG_MAP()
//
//	void DrawItem(int nIDCtl, LPDRAWITEMSTRUCT lpDrawItemStruct);
//};

//
// CPropertyPageOSD dialog
//
class CPropertyPageOSD : public CPropertyPageImpl<CPropertyPageOSD>
{
public:
	CPropertyPageOSD(const Settings::Presentation::OSD & settings);
	virtual ~CPropertyPageOSD();

// Dialog Data
	enum { IDD = IDD_SETTINGS_OSD };

protected:
	Settings::Presentation::OSD m_settings;
	CListBox m_listBox;
	int m_initialized;

protected:
	void UpdateSettings(int set = SET);
	void UpdateFont(int set = SET);
	void UpdateOffsetX(int set = SET);
	void UpdateOffsetY(int set = SET);
	void UpdateOffset(int id, int set);
	void UpdateVertical(int set);

	void EnableSelectFont(BOOL enable, BOOL enableColor, BOOL enableGroupDigits);
	void EnableOffset(BOOL enable);
	void EnableVertical(BOOL enable);

	void InitList();
	void EnumerateSystemFonts();
	void SetModified(BOOL bChanged = TRUE);

	TxtFormat * GetCurrentFormat();

	void OnColor(DWORD & color);

protected:
	enum {
		F_DRAW_EXAMPLE = 0x1,
		F_DRAW_TEXT_COLOR = 0x2,
		F_DRAW_ALL = F_DRAW_EXAMPLE|F_DRAW_TEXT_COLOR,
	};	
	void Invalidate2() { m_painter.Invalidate(F_DRAW_ALL, FALSE); }
	void Invalidate2(HWND hWnd, int flags = F_DRAW_ALL) { m_painter.Invalidate(hWnd, flags, FALSE); }

protected:
	my::Painter m_painter;

protected:
	BEGIN_MSG_MAP_EX(CPropertyPageOSD)
		MSG_WM_INITDIALOG(OnInitDialog)
		MSG_WM_PAINT(OnPaint)
		COMMAND_HANDLER_EX(IDC_COMBO_FONT_NAME, CBN_SELCHANGE, OnCbnSelchangeComboFontName)
		COMMAND_HANDLER_EX(IDC_COMBO_FONT_STYLE, CBN_SELCHANGE, OnCbnSelchangeComboFontStyle)
		COMMAND_HANDLER_EX(IDC_COMBO_FONT_SIZE, CBN_SELCHANGE, OnCbnSelchangeComboFontSize)
		COMMAND_HANDLER_EX(IDC_FONT_COLOR, STN_CLICKED, OnStnClickedFontColor)
		COMMAND_HANDLER_EX(IDC_EXAMPLE, STN_CLICKED, OnStnClickedExample)
		COMMAND_HANDLER_EX(IDC_COMBO_FONT_SIZE, CBN_EDITCHANGE, OnCbnEditchangeComboFontSize)
		COMMAND_HANDLER_EX(IDC_BUTTON_BACKGROUND_COLOR, BN_CLICKED, OnBnClickedButtonBackgroundColor)
		COMMAND_HANDLER_EX(IDC_LIST1, LBN_SELCHANGE, OnLbnSelchangeList1)
		COMMAND_HANDLER_EX(IDC_EDIT_OFFSET_X, EN_CHANGE, OnEnChangeEditOffsetX)
		COMMAND_HANDLER_EX(IDC_EDIT_OFFSET_Y, EN_CHANGE, OnEnChangeEditOffsetY)
		NOTIFY_HANDLER_EX(IDC_SPIN_OFFSET_X, UDN_DELTAPOS, OnDeltaposSpinOffsetX)
		NOTIFY_HANDLER_EX(IDC_SPIN_OFFSET_Y, UDN_DELTAPOS, OnDeltaposSpinOffsetY)
		COMMAND_HANDLER_EX(IDC_CHECK_VERTICAL, BN_CLICKED, OnBnClickedCheckVertical)
		COMMAND_HANDLER_EX(IDC_CHECK_GROUP_DIGITS, BN_CLICKED, OnBnClickedCheckGroupDigits)
		CHAIN_MSG_MAP(CPropertyPageImpl<CPropertyPageOSD>)
	END_MSG_MAP()

	BOOL OnInitDialog(CWindow wndFocus, LPARAM lInitParam);

	void OnPaint(CDCHandle );
	void OnCbnSelchangeComboFontName(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnCbnSelchangeComboFontStyle(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnCbnSelchangeComboFontSize(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnStnClickedFontColor(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnStnClickedExample(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnCbnEditchangeComboFontSize(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonBackgroundColor(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnLbnSelchangeList1(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnEnChangeEditOffsetX(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnEnChangeEditOffsetY(UINT uNotifyCode, int nID, CWindow wndCtl);
	LRESULT OnDeltaposSpinOffsetX(NMHDR *pNMHDR);
	LRESULT OnDeltaposSpinOffsetY(NMHDR *pNMHDR);
	void OnBnClickedCheckVertical(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckGroupDigits(UINT uNotifyCode, int nID, CWindow wndCtl);

	BOOL OnApply();
	BOOL OnQueryCancel();

	BOOL OnKillActive();
	BOOL OnSetActive();
};

typedef CPropertyPageImpl<CPropertyPageOSD> CPropertyPageOSDBase;
