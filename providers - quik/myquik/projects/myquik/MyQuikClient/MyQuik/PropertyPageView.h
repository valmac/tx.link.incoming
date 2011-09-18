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
 *  PropertyPageView.h
 */

#pragma once

// CPropertyPageView dialog

class CPropertyPageView : public CPropertyPageImpl<CPropertyPageView>
{
public:
	CPropertyPageView(const ::Settings::Presentation & presentation);
	virtual ~CPropertyPageView();

// Dialog Data
	enum { IDD = IDD_SETTINGS_VIEW };

protected:
	::Settings::Presentation m_settings;
	BOOL m_initialized;
	BOOL m_updating;
	CListViewCtrl m_listWindows;
	CListViewCtrl m_listMain;

protected:
	void InitListWindows();
	void InitListMain();

	int GetCurrentWindowIndex();
	int GetCurrentWindow(Settings::Presentation::Window ** ppWindow);

	void UpdateSettings(int set);
	void UpdatePresentation(int set, int index = -1);
	void UpdateOpacity(int set, Settings::Presentation::Window * pWindow, BOOL visible);
	void UpdatePresentationDeals(int set);
	void UpdateMargins(int set);
	void UpdatePlacement(int set);
	void UpdateFlashing(int set);

	void EnableCheckBoxTitleBar(UINT id, BOOL enable = TRUE);
	void EnableCheckBoxFrame(UINT id, BOOL enable = TRUE);

	void SetModified(BOOL bChanged = TRUE);

	void OnDeltaposSpinOpacity(LPNMUPDOWN pNMUpDown, UINT id);
	void OnEnChangeEditOpacity(UINT id);

protected:
	BEGIN_MSG_MAP_EX(CPropertyPageView)
		MSG_WM_INITDIALOG(OnInitDialog)
		NOTIFY_HANDLER_EX(IDC_LIST_WINDOWS, LVN_ITEMCHANGED, OnLvnItemchangedListWindows)
		COMMAND_HANDLER_EX(IDC_CHECKBOX_SHOW_TITLE, BN_CLICKED, OnBnClickedShowTitle)
		COMMAND_HANDLER_EX(IDC_CHECK_SHOW_LISTOFDEALS, BN_CLICKED, OnBnClickedCheckShowListofdeals)
		COMMAND_HANDLER_EX(IDC_CHECK_SHOW_OSD, BN_CLICKED, OnBnClickedCheckShowOsd)
		COMMAND_HANDLER_EX(IDC_CHECK_SHOW_CONTROLS, BN_CLICKED, OnBnClickedCheckShowControls)
		COMMAND_HANDLER_EX(IDC_CHECK_SHOW_FRAME_LISTOFDEALS, BN_CLICKED, OnBnClickedCheckShowFrameListofdeals)
		COMMAND_HANDLER_EX(IDC_CHECK_SHOW_FRAME_OSD, BN_CLICKED, OnBnClickedCheckShowFrameOsd)
		COMMAND_HANDLER_EX(IDC_CHECK_SHOW_FRAME_CONTROLS, BN_CLICKED, OnBnClickedCheckShowFrameControls)
		COMMAND_HANDLER_EX(IDC_EDIT_OPACITY, EN_CHANGE, OnEnChangeEditOpacity)
		COMMAND_HANDLER_EX(IDC_EDIT_OPACITY2, EN_CHANGE, OnEnChangeEditOpacity2)
		NOTIFY_HANDLER_EX(IDC_SPIN_OPACITY, UDN_DELTAPOS, OnDeltaposSpinOpacity)
		NOTIFY_HANDLER_EX(IDC_SPIN_OPACITY2, UDN_DELTAPOS, OnDeltaposSpinOpacity2)
		COMMAND_HANDLER_EX(IDC_EDIT_MARGINS, EN_CHANGE, OnEnChangeEditMargins)
		NOTIFY_HANDLER_EX(IDC_SPIN2, UDN_DELTAPOS, OnDeltaposSpin2)
		COMMAND_HANDLER_EX(IDC_CHECK_ALLWAYS_ON_TOP, BN_CLICKED, OnBnClickedCheckAllwaysOnTop)
		COMMAND_HANDLER_EX(IDC_CHECKBOX_ENABLE_FLASHING, BN_CLICKED, OnBnClickedCheckEnableFlashing)
		CHAIN_MSG_MAP(CPropertyPageImpl<CPropertyPageView>)
	END_MSG_MAP()

	BOOL OnInitDialog(CWindow wndFocus, LPARAM lInitParam);

	LRESULT OnLvnItemchangedListWindows(NMHDR *pNMHDR);
	void OnBnClickedShowTitle(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckShowListofdeals(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckShowOsd(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckShowControls(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckShowFrameListofdeals(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckShowFrameOsd(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckShowFrameControls(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnEnChangeEditOpacity(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnEnChangeEditOpacity2(UINT uNotifyCode, int nID, CWindow wndCtl);
	LRESULT OnDeltaposSpinOpacity(NMHDR *pNMHDR);
	LRESULT OnDeltaposSpinOpacity2(NMHDR *pNMHDR);
	void OnEnChangeEditMargins(UINT uNotifyCode, int nID, CWindow wndCtl);
	LRESULT OnDeltaposSpin2(NMHDR *pNMHDR);
	void OnBnClickedCheckAllwaysOnTop(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckEnableFlashing(UINT uNotifyCode, int nID, CWindow wndCtl);

	BOOL OnApply();
	BOOL OnQueryCancel();
};
