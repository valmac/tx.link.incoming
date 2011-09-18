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
 *  PropertyPageGlass.h
 */

#pragma once

#include "paint.h"

class CColumnSizer : public CWindowImpl<CColumnSizer, CStatic>
{
public:
	CColumnSizer();

	void Init(Settings::Presentation::Glass::Columns * pParams);

	BOOL IsEnabled() const;
	BOOL IsSizing() const;

	void SetSizing(BOOL sizing = TRUE) { m_sizing = sizing; }

	int GetActiveMarker() const { return m_markers.active; }
	void SetActiveMarker(int i) { m_markers.active = i; }

	int GetPerc(int & w1, int & w2, int & w3) const;
	int Get(int & cx1, int & cx2, int & cx3) const;
	int Get(CPoint * pLeft, CPoint * pRight, CRect * pMarker0, CRect * pMarker1, CRect * pMarker2, CRect * pActive1, CRect * pActive2) const;

	int SetMarker(const CPoint * pPoint);

protected:
	Settings::Presentation::Glass::Columns * m_pParams;
	int m_length;
	BOOL m_sizing;
	struct Markers {
		int active;
	} m_markers;

protected:
	BEGIN_MSG_MAP_EX(CColumnSizer)
	END_MSG_MAP()
};

//
// CPropertyPageGlass dialog
//
class CPropertyPageGlass : public CPropertyPageImpl<CPropertyPageGlass>, public CWinDataExchange<CPropertyPageGlass>
{
public:
	CPropertyPageGlass(const Settings::Presentation::Glass & settings);
	virtual ~CPropertyPageGlass();

// Dialog Data
	enum { IDD = IDD_SETTINGS_GLASS };

protected:
	Settings::Presentation::Glass m_settings;
	int m_initialized;
	struct Icons {
		HICON hIcon[2];
	} m_icons;

protected:

	BOOL InitComboBoxOutline(UINT id);
	BOOL InitComboBoxMargins(UINT id);
	BOOL InitComboBoxColumns(UINT id);
	BOOL InitComboBoxAlignment(UINT id);

	void UpdateSettings(int set = SET);
	void UpdateItemSettings(int set, int index = -1);

	void UpdateSettingsText(int set, Settings::Presentation::GlassItem::Text & text);
	void UpdateSettingsBackgrnd(int set, Settings::Presentation::GlassItem::Background & backgrnd);

	void UpdateSettingsGroupDigits(int set, Settings::Presentation::GlassItem::Text & text);

	void UpdateSettingsText1(int set, int i);
	void UpdateSettingsBackgrnd1(int set, int i);

	void UpdateSettingsGrid(int set);
	void UpdateSettingsGridLines(int set);
	void UpdateSettingsMargins(int set);
	void UpdateSettingsBorder(int set);	
	void UpdateSettingsMarker(int set);	
	void UpdateSettingsUser(int set);
	void UpdateSettingsAlignment(int set, int i);
	void UpdateSettingsAlignment(int set, DWORD & alignment, BOOL enable);
	void UpdateSettingsVolumeIndication(int set, int i);
	void UpdateSettingsVolumeIndication(int set, Settings::Presentation::GlassItem & item);

	void UpdateSettingsView(int set);
	void UpdateSettingsStyle(int set, int i = -1);

	void UpdateTextBuyAbove(int style);

	void EnableItems(int set, int i = -1);
	void EnableModify(BOOL enable = TRUE);

	void EnumerateSystemFonts();

	void OnColor(DWORD & color);

	void SetModified(BOOL bChanged = TRUE);

	int GetCurrentIndex() const;

	Settings::Presentation::GlassItem::Text * GetCurrentText();
	Settings::Presentation::GlassItem::Background * GetCurrentBackgrnd(int i = -1);
	COLORREF * GetCurrentColor(int backgrnd = 1, int i = -1);

	int GetCurrentItem() const;
	LPCTSTR GetItemName(int i) const;

	UINT GetIdGroupView(int i);
	UINT GetIdGroupText(int i);
	UINT GetIdGroupBackgrnd(int i);
	UINT GetIdGroupUser(int i);
	UINT GetIdGroupGlass(int i);

	typedef UINT (CPropertyPageGlass::*PFuncGetIdGroup)(int i);

	void ShowGroup(PFuncGetIdGroup func, int show, UINT except = 0, int flags = 0x0);
	void EnableGroup(PFuncGetIdGroup func, BOOL enable = TRUE, UINT except1 = 0, UINT except2 = 0);
	void MoveGroup(PFuncGetIdGroup func, UINT src, UINT dst, int show);
	void MoveGroup(PFuncGetIdGroup func, UINT src, CRect dst, int show);

protected:
	enum {
		F_DRAW_WINDOW_STYLE = 0x1,
		F_DRAW_TEXT_COLOR = 0x2,
		F_DRAW_GLASS_BACKGRND_COLOR = 0x4,
		F_DRAW_GLASS_COLOR = 0x8,
		F_DRAW_COLUMN_SIZER = 0x10,
		F_DRAW_ALL = F_DRAW_WINDOW_STYLE|F_DRAW_TEXT_COLOR|F_DRAW_GLASS_BACKGRND_COLOR|F_DRAW_GLASS_COLOR,
	};	
	void Invalidate2() { m_painter.Invalidate(F_DRAW_ALL, FALSE); }
	void Invalidate2(HWND hWnd, int flags = F_DRAW_ALL) { m_painter.Invalidate(hWnd, flags, FALSE); }

protected:
	my::Painter m_painter;
	CColumnSizer m_columnSizer;
	BOOL m_canModify;

protected:
	int DrawMiniPicture(CWindow wnd, int style);
	int DrawColumnSizer(CWindow wnd);

protected:
	BEGIN_MSG_MAP_EX(CPropertyPageGlass)
		MSG_WM_INITDIALOG(OnInitDialog)
		MSG_WM_PAINT(OnPaint)
		MSG_WM_MOUSEMOVE(OnMouseMove)
		MSG_WM_LBUTTONDOWN(OnLButtonDown)
		MSG_WM_LBUTTONUP(OnLButtonUp)
		COMMAND_HANDLER_EX(IDC_FONT_COLOR, STN_CLICKED, OnStnClickedFontColor)
		COMMAND_HANDLER_EX(IDC_COMBO_FONT_NAME, CBN_SELCHANGE, OnCbnSelchangeComboFontName)
		COMMAND_HANDLER_EX(IDC_COMBO_FONT_STYLE, CBN_SELCHANGE, OnCbnSelchangeComboFontStyle)
		COMMAND_HANDLER_EX(IDC_COMBO_FONT_SIZE, CBN_SELCHANGE, OnCbnSelchangeComboFontSize)
		COMMAND_HANDLER_EX(IDC_COMBO_FONT_SIZE, CBN_EDITCHANGE, OnCbnEditchangeComboFontSize)
		COMMAND_HANDLER_EX(IDC_CHECK_SHOW_USER_BIDS, BN_CLICKED, OnBnClickedCheckShowUserBids)
		COMMAND_HANDLER_EX(IDC_CHECK_SHOW_USER_STOPS, BN_CLICKED, OnBnClickedCheckShowUserStopbids)
		COMMAND_HANDLER_EX(IDC_CHECK_SHOW_USER_DEALS, BN_CLICKED, OnBnClickedCheckShowUserDeals)
		COMMAND_HANDLER_EX(IDC_CHECK_SHOW_USER_COL, BN_CLICKED, OnBnClickedCheckShowUserCol)
		COMMAND_HANDLER_EX(IDC_CHECK_SHOW_USER_ICONS, BN_CLICKED, OnBnClickedCheckShowUserIcons)
		COMMAND_HANDLER_EX(IDC_CHECKBOX_USER_GROUP_BY_PRICE, BN_CLICKED, OnBnClickedCheckUserGroupByPrice)
		COMMAND_HANDLER_EX(IDC_GLASS_BACKGRND_COLOR, STN_CLICKED, OnStnClickedGlassBackgrndColor)
		COMMAND_HANDLER_EX(IDC_EDIT_GLASS_BACKGRND, EN_CHANGE, OnEnChangeEditGlassBackgrnd)
		NOTIFY_HANDLER_EX(IDC_SPIN_GLASS_BACKGRND, UDN_DELTAPOS, OnDeltaposSpinGlassBackgrnd)
		COMMAND_HANDLER_EX(IDC_GLASS_COLOR, STN_CLICKED, OnStnClickedGlassColor)
		COMMAND_HANDLER_EX(IDC_EDIT_GLASS, EN_CHANGE, OnEnChangeEditGlass)
		NOTIFY_HANDLER_EX(IDC_SPIN_GLASS, UDN_DELTAPOS, OnDeltaposSpinGlass)
		COMMAND_HANDLER_EX(IDC_LIST1, LBN_SELCHANGE, OnLbnSelchangeList1)
		COMMAND_HANDLER_EX(IDC_RADIO_STYLE1, BN_CLICKED, OnBnClickedRadioStyle1)
		COMMAND_HANDLER_EX(IDC_RADIO_STYLE2, BN_CLICKED, OnBnClickedRadioStyle2)
		COMMAND_HANDLER_EX(IDC_RADIO_STYLE3, BN_CLICKED, OnBnClickedRadioStyle3)
		COMMAND_HANDLER_EX(IDC_COMBO_GLASS1, CBN_SELCHANGE, OnCbnSelchangeComboGlass1)
		COMMAND_HANDLER_EX(IDC_COMBO_GLASS2, CBN_SELCHANGE, OnCbnSelchangeComboGlass2)
		COMMAND_HANDLER_EX(IDC_CHECK_BUY_ABOVE, BN_CLICKED, OnBnClickedCheckBuyAbove)
		COMMAND_HANDLER_EX(IDC_CHECK_FLIP_HORIZ, BN_CLICKED, OnBnClickedCheckFlipHoriz)
		COMMAND_HANDLER_EX(IDC_CHECK_RAREFY, BN_CLICKED, OnBnClickedCheckRarefy)
		COMMAND_HANDLER_EX(IDC_CHECK_NEUTRAL_ZONE, BN_CLICKED, OnBnClickedCheckNeutralZone)
		COMMAND_HANDLER_EX(IDC_CHECK_GROUP_DIGITS, BN_CLICKED, OnBnClickedCheckGroupDigits)
		COMMAND_HANDLER_EX(IDC_CHECKBOX_TEXT_1, BN_CLICKED, OnBnClickedCheckText1)
		COMMAND_HANDLER_EX(IDC_CHECKBOX_TEXT, BN_CLICKED, OnBnClickedCheckText)
		COMMAND_HANDLER_EX(IDC_CHECKBOX_BACKGRND, BN_CLICKED, OnBnClickedCheckBackgrnd)
		COMMAND_HANDLER_EX(IDC_CHECKBOX_BACKGRND_1, BN_CLICKED, OnBnClickedCheckBackgrnd1)
		COMMAND_HANDLER_EX(IDC_COMBO_GLASS1, CBN_SELCHANGE, OnCbnSelchangeComboGlass1)
		COMMAND_HANDLER_EX(IDC_COMBO_GLASS2, CBN_SELCHANGE, OnCbnSelchangeComboGlass2)
		COMMAND_HANDLER_EX(IDC_CHECKBOX_ALIGNMENT, BN_CLICKED, OnBnClickedCheckAlignment)
		COMMAND_HANDLER_EX(IDC_CHECKBOX_INDICATION, BN_CLICKED, OnBnClickedCheckboxIndication)
		COMMAND_HANDLER_EX(IDC_CHECKBOX_GRID, BN_CLICKED, OnBnClickedCheckGrid)
		COMMAND_HANDLER_EX(IDC_CHECKBOX_MARGINS, BN_CLICKED, OnBnClickedCheckMargins)
		COMMAND_HANDLER_EX(IDC_CHECKBOX_BORDER, BN_CLICKED, OnBnClickedCheckBorder)
		COMMAND_HANDLER_EX(IDC_CHECKBOX_MARKER, BN_CLICKED, OnBnClickedCheckMarker)
		COMMAND_HANDLER_EX(IDC_CHECKBOX_GRID_LINES_V, BN_CLICKED, OnBnClickedCheckGridLinesV)
		COMMAND_HANDLER_EX(IDC_CHECKBOX_GRID_LINES_H, BN_CLICKED, OnBnClickedCheckGridLinesH)
		COMMAND_HANDLER_EX(IDC_CHECKBOX3, BN_CLICKED, OnBnClickedCheckbox3)
		CHAIN_MSG_MAP(CPropertyPageImpl<CPropertyPageGlass>)
	END_MSG_MAP()

	BEGIN_DDX_MAP(CPropertyPageGlass)
		DDX_CONTROL(IDC_COLUMN_SIZER, m_columnSizer)
	END_DDX_MAP()

	BOOL OnInitDialog(CWindow wndFocus, LPARAM lInitParam);
	
	void OnPaint(CDCHandle );
	void OnStnClickedFontColor(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnCbnSelchangeComboFontName(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnCbnSelchangeComboFontStyle(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnCbnSelchangeComboFontSize(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnCbnEditchangeComboFontSize(UINT uNotifyCode, int nID, CWindow wndCtl);

	void OnBnClickedCheckShowUserBids(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckShowUserStopbids(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckShowUserDeals(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckShowUserCol(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckShowUserIcons(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckUserGroupByPrice(UINT uNotifyCode, int nID, CWindow wndCtl);

	void OnStnClickedGlassBackgrndColor(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnEnChangeEditGlassBackgrnd(UINT uNotifyCode, int nID, CWindow wndCtl);
	LRESULT OnDeltaposSpinGlassBackgrnd(NMHDR *pNMHDR);
	void OnStnClickedGlassColor(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnEnChangeEditGlass(UINT uNotifyCode, int nID, CWindow wndCtl);
	LRESULT OnDeltaposSpinGlass(NMHDR *pNMHDR);

	void OnLbnSelchangeList1(UINT uNotifyCode, int nID, CWindow wndCtl);

	void OnMouseMove(UINT nFlags, CPoint point);
	void OnLButtonDown(UINT nFlags, CPoint point);
	void OnLButtonUp(UINT nFlags, CPoint point);

	void OnBnClickedRadioStyle1(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedRadioStyle2(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedRadioStyle3(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckBuyAbove(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckFlipHoriz(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckRarefy(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckNeutralZone(UINT uNotifyCode, int nID, CWindow wndCtl);

	void OnBnClickedCheckGroupDigits(UINT uNotifyCode, int nID, CWindow wndCtl);

	void OnBnClickedCheckText(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckBackgrnd(UINT uNotifyCode, int nID, CWindow wndCtl);

	void OnBnClickedCheckText1(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckBackgrnd1(UINT uNotifyCode, int nID, CWindow wndCtl);

	void OnCbnSelchangeComboGlass1(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnCbnSelchangeComboGlass2(UINT uNotifyCode, int nID, CWindow wndCtl);

	void OnBnClickedCheckAlignment(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckboxIndication(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckGrid(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckMargins(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckBorder(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckMarker(UINT uNotifyCode, int nID, CWindow wndCtl);

	void OnBnClickedCheckGridLinesV(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckGridLinesH(UINT uNotifyCode, int nID, CWindow wndCtl);

	void OnBnClickedCheckbox3(UINT uNotifyCode, int nID, CWindow wndCtl);

	BOOL OnApply();
	BOOL OnQueryCancel();
	BOOL OnSetActive();
};

typedef CPropertyPageImpl<CPropertyPageGlass> CPropertyPageGlassBase;
