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
 *  PropertyPageTrading.h
 */

#pragma once

// CPropertyPageTrading dialog

class CPropertyPageTrading : public CPropertyPageImpl<CPropertyPageTrading>
{
public:
	CPropertyPageTrading(const Settings::Trading & settings);
	virtual ~CPropertyPageTrading();

// Dialog Data
	enum { IDD = IDD_SETTINGS_TRADING };

	void SetModified(BOOL bChanged = TRUE);

protected:
	Settings::Trading m_settings;
	BOOL m_initialized;

protected:
	void UpdateSettings(int set);
	void UpdateSettingsTrading(int set);
	void UpdateSettingsHistory(int set);

	void UpdateButtonsHistory();
	void UpdateButtonsHistoryModify();

	void EnableButtonsHistory(BOOL enable = TRUE);
	void EnableButtonsHistoryModify(BOOL enable = TRUE);

	void Reset();

protected:

	BEGIN_MSG_MAP_EX(CPropertyPageTrading)
		MSG_WM_INITDIALOG(OnInitDialog)
		COMMAND_HANDLER_EX(IDC_CHECK_CONFIRM_TRANSACTION, BN_CLICKED, OnBnClickedCheckConfirmTransaction)
		COMMAND_HANDLER_EX(IDC_CHECK_CONFIRM_TRANSACTION_CANCEL, BN_CLICKED, OnBnClickedCheckConfirmTransactionCancel)
		COMMAND_HANDLER_EX(IDC_CHECKBOX_PRINT_MESSAGE_ON_TRANSACTION_ERROR, BN_CLICKED, OnBnClickedCheckPrintTransactionError)
		COMMAND_HANDLER_EX(IDC_CHECK_CANCEL_ALL_ORDERS, BN_CLICKED, OnBnClickedCheckCancelAll)
		COMMAND_HANDLER_EX(IDC_CHECK_AUTOPRICE, BN_CLICKED, OnBnClickedCheckAutoprice)
		COMMAND_HANDLER_EX(IDC_CHECK_SPREAD, BN_CLICKED, OnBnClickedCheckSpread)
		COMMAND_HANDLER_EX(IDC_CHECK_BEST_PRICE_IS_CURRENT_PRICE, BN_CLICKED, OnBnClickedCheckBestPriceIsCurrentPrice)
		COMMAND_HANDLER_EX(IDC_CHECK_KEEP_HISTORY, BN_CLICKED, OnBnClickedCheckKeepHistory)
		COMMAND_HANDLER_EX(IDC_CHECK_AUTOREGISTER_STOPORDER, BN_CLICKED, OnBnClickedCheckAutoregisterStoporder)
		COMMAND_HANDLER_EX(IDC_BUTTON_CLEAR_HISTORY, BN_CLICKED, OnBnClickedButtonClearHistory)
		COMMAND_HANDLER_EX(IDC_BUTTON_EDIT_HISTORY, BN_CLICKED, OnBnClickedButtonEditHistory)
		COMMAND_HANDLER_EX(IDC_CHECK_HISTORY_CONFIRM_REMOVE, BN_CLICKED, OnBnClickedCheckHistoryConfirmRemove)
		COMMAND_HANDLER_EX(IDC_CHECK_MODIFY_HISTORY, BN_CLICKED, OnBnClickedCheckModifyHistory)
		COMMAND_HANDLER_EX(IDC_CHECK_HISTORY_KEEP_CANCEL, BN_CLICKED, OnBnClickedCheckHistoryKeepCancel)
		CHAIN_MSG_MAP(CPropertyPageImpl<CPropertyPageTrading>)
	END_MSG_MAP()

	BOOL OnInitDialog(CWindow wndFocus, LPARAM lInitParam);

	void OnBnClickedCheckConfirmTransaction(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckConfirmTransactionCancel(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckPrintTransactionError(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckCancelAll(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckAutoprice(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckSpread(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckBestPriceIsCurrentPrice(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckKeepHistory(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckAutoregisterStoporder(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonClearHistory(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedButtonEditHistory(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckHistoryConfirmRemove(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckModifyHistory(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCheckHistoryKeepCancel(UINT uNotifyCode, int nID, CWindow wndCtl);

	BOOL OnApply();
	BOOL OnQueryCancel();
	BOOL OnKillActive();
};

typedef CPropertyPageImpl<CPropertyPageTrading> CPropertyPageTradingBase;
