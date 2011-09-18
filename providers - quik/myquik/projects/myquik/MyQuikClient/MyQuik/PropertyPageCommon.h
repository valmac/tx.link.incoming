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
 *  PropertyPageCommon.h
 */

#pragma once

// CPropertyPageCommon dialog

class CPropertyPageCommon : public CPropertyPageImpl<CPropertyPageCommon>
{
public:
	CPropertyPageCommon(const ::Settings::Common & common, const ::Settings::Log & log, const ::Settings::Server & server);
	virtual ~CPropertyPageCommon();

// Dialog Data
	enum { IDD = IDD_SETTINGS_COMMON };

	struct Settings {
		::Settings::Common common;
		::Settings::Log log;
		::Settings::Server server;
	};

	enum PathType {
		PATH_QUIK,
		PATH_TRANS2QUIK,
		PATH_HISTORY,
		PATH_LOG,
		PATH_LAST,
	};

protected:
	CPropertyPageCommon::Settings m_settings;
	BOOL m_initialized;

protected:
	void UpdateSettings(int set);
	void UpdateCommon(int set);
	void UpdatePath(int i, int set);

	BOOL OnSelectPath(int i, UINT id, LPCTSTR filterName, LPCTSTR filter);
	void SetTextRightAligned(UINT id, LPCTSTR text);
	void SetTextRightAligned(CEdit & edit, LPCTSTR text);

	LPCTSTR GetPathName(int type);

	void SetModified(BOOL bChanged = TRUE);

protected:
	BEGIN_MSG_MAP_EX(CPropertyPageCommon)
		MSG_WM_INITDIALOG(OnInitDialog)
		COMMAND_HANDLER_EX(IDC_COMBO1, CBN_SELCHANGE, OnCbnSelchangeComboPath)
		COMMAND_HANDLER_EX(IDC_BUTTON_QUIK_PATH, BN_CLICKED, OnBnClickedButtonPath)
		COMMAND_HANDLER_EX(IDC_EDIT_QUIK_PATH, EN_CHANGE, OnEnChangeEditPath)
		COMMAND_HANDLER_EX(IDC_CHECK_PRINT_TRANSACTION, BN_CLICKED, OnBnClickedCheckPrintTransaction)
		COMMAND_HANDLER_EX(IDC_CHECK_SAVE_ON_DISK, BN_CLICKED, OnBnClickedCheckSaveOnDisk)
		COMMAND_HANDLER_EX(IDC_BUTTON_CLEAR_CACHE, BN_CLICKED, OnBnClickedButtonClearCache)
		COMMAND_HANDLER_EX(IDC_CHECK_AUTO_CHECK_CONNECTION, BN_CLICKED, OnBnClickedCheckAutoCheckConnection)
		CHAIN_MSG_MAP(CPropertyPageImpl<CPropertyPageCommon>)
	END_MSG_MAP()

	BOOL OnInitDialog(CWindow wndFocus, LPARAM lInitParam);

	void OnEnChangeEditPath(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonPath(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckPrintTransaction(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckSaveOnDisk(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonClearCache(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnCbnSelchangeComboPath(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckAutoCheckConnection(UINT uNotifyCode, int nID, CWindow wndCtl);

	BOOL OnApply();
	BOOL OnQueryCancel();
};
