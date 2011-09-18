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
 *  PropertyPageTrading.cpp : implementation file
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "PropertyPageTrading.h"
#include "DlgHistory.h"
#include "history.h"


// CPropertyPageTrading dialog

CPropertyPageTrading::CPropertyPageTrading(const Settings::Trading & settings)
{
	m_initialized = FALSE;
	m_settings = settings;
}

CPropertyPageTrading::~CPropertyPageTrading()
{
}

void CPropertyPageTrading::UpdateSettings(int set)
{
	UpdateSettingsTrading(set);
	UpdateSettingsHistory(set);
}

void CPropertyPageTrading::UpdateSettingsTrading(int set)
{
	CButton btn;
	DWORD mask;

	if (! set)
		m_settings.flags = 0;

	btn = GetDlgItem(IDC_CHECK_CONFIRM_TRANSACTION);
	if (set)
		btn.SetCheck(m_settings.confirmTransaction ? BST_CHECKED : BST_UNCHECKED);
	else
		m_settings.confirmTransaction = (btn.GetCheck() == BST_CHECKED);
	btn = GetDlgItem(IDC_CHECK_CONFIRM_TRANSACTION_CANCEL);
	if (set)
		btn.SetCheck(m_settings.confirmCancel ? BST_CHECKED : BST_UNCHECKED);
	else
		m_settings.confirmCancel = (btn.GetCheck() == BST_CHECKED);

	mask = F_TRADING_PRINT_MESSAGE_ON_TRANSACTION_ERROR;
	btn = GetDlgItem(IDC_CHECKBOX_PRINT_MESSAGE_ON_TRANSACTION_ERROR);
	if (set)
		btn.SetCheck((m_settings.flags & mask) ? BST_CHECKED : BST_UNCHECKED);
	else if (btn.GetCheck() == BST_CHECKED)
		m_settings.flags |= mask;

	mask = F_TRADING_CANCEL_ALL;
	btn = GetDlgItem(IDC_CHECK_CANCEL_ALL_ORDERS);
	if (set)
		btn.SetCheck((m_settings.flags & mask) ? BST_CHECKED : BST_UNCHECKED);
	else if (btn.GetCheck() == BST_CHECKED)
		m_settings.flags |= mask;

	btn = GetDlgItem(IDC_CHECK_SPREAD);
	if (set)
		btn.SetCheck(m_settings.useSpread ? BST_CHECKED : BST_UNCHECKED);
	else
		m_settings.useSpread = (btn.GetCheck() == BST_CHECKED);

	btn = GetDlgItem(IDC_CHECK_AUTOPRICE);
	if (set)
		btn.SetCheck(m_settings.autoPrice ? BST_CHECKED : BST_UNCHECKED);
	else
		m_settings.autoPrice = (btn.GetCheck() == BST_CHECKED);

	btn = GetDlgItem(IDC_CHECK_BEST_PRICE_IS_CURRENT_PRICE);
	if (set)
		btn.SetCheck(m_settings.bestIsCurrent ? BST_CHECKED : BST_UNCHECKED);
	else
		m_settings.bestIsCurrent = (btn.GetCheck() == BST_CHECKED);

	btn = GetDlgItem(IDC_CHECK_AUTOREGISTER_STOPORDER);
	if (set)
		btn.SetCheck(m_settings.autoStop ? BST_CHECKED : BST_UNCHECKED);
	else
		m_settings.autoStop = (btn.GetCheck() == BST_CHECKED);
}

void CPropertyPageTrading::UpdateSettingsHistory(int set)
{
	CButton btn;
	btn = GetDlgItem(IDC_CHECK_KEEP_HISTORY);
	if (set)
		btn.SetCheck(m_settings.history.keep ? BST_CHECKED : BST_UNCHECKED);
	else
		m_settings.history.keep = (btn.GetCheck() == BST_CHECKED);
	btn = GetDlgItem(IDC_CHECK_HISTORY_KEEP_CANCEL);
	if (set)
		btn.SetCheck(m_settings.history.keepCancel ? BST_CHECKED : BST_UNCHECKED);
	else
		m_settings.history.keepCancel = (btn.GetCheck() == BST_CHECKED);
	btn = GetDlgItem(IDC_CHECK_MODIFY_HISTORY);
	if (set)
		btn.SetCheck(m_settings.history.modify ? BST_CHECKED : BST_UNCHECKED);
	else
		m_settings.history.modify = (btn.GetCheck() == BST_CHECKED);
	btn = GetDlgItem(IDC_CHECK_HISTORY_CONFIRM_REMOVE);
	if (set)
		btn.SetCheck(m_settings.history.confirmRemove ? BST_CHECKED : BST_UNCHECKED);
	else
		m_settings.history.confirmRemove = (btn.GetCheck() == BST_CHECKED);
}

void CPropertyPageTrading::UpdateButtonsHistory()
{
	CButton btn = GetDlgItem(IDC_CHECK_KEEP_HISTORY);
	BOOL enable = (btn.GetCheck() == BST_CHECKED);
	EnableButtonsHistory(enable);

	UpdateButtonsHistoryModify();
}

void CPropertyPageTrading::UpdateButtonsHistoryModify()
{
	CButton btn1 = GetDlgItem(IDC_CHECK_KEEP_HISTORY);
	CButton btn2 = GetDlgItem(IDC_CHECK_MODIFY_HISTORY);
	BOOL enable = (btn1.GetCheck() == BST_CHECKED) & (btn2.GetCheck() == BST_CHECKED);
	EnableButtonsHistoryModify(enable);
}

void CPropertyPageTrading::EnableButtonsHistory(BOOL enable)
{
	CButton btn;
	btn = GetDlgItem(IDC_CHECK_HISTORY_KEEP_CANCEL);
	btn.EnableWindow(enable);
	btn = GetDlgItem(IDC_CHECK_MODIFY_HISTORY);
	btn.EnableWindow(enable);
	btn = GetDlgItem(IDC_CHECK_HISTORY_CONFIRM_REMOVE);
	btn.EnableWindow(enable);
	btn = GetDlgItem(IDC_BUTTON_EDIT_HISTORY);
	btn.EnableWindow(enable);
	btn = GetDlgItem(IDC_BUTTON_CLEAR_HISTORY);
	btn.EnableWindow(enable);
}

void CPropertyPageTrading::EnableButtonsHistoryModify(BOOL enable)
{
	CButton btn;
	btn = GetDlgItem(IDC_CHECK_HISTORY_CONFIRM_REMOVE);
	btn.EnableWindow(enable);
	CWindow wnd;
	wnd = GetDlgItem(IDC_BUTTON_EDIT_HISTORY);
	wnd.EnableWindow(enable);
}

void CPropertyPageTrading::SetModified(BOOL bChanged)
{
	if (bChanged && m_initialized)
	{
		CWindow parent = theApp.GetMainWnd();
		if (parent)
			parent.SendMessage(UM_APPLY_SETTINGS, APPLY_SETTINGS_TRADING, (LPARAM)&m_settings);
	}
	CPropertyPageTradingBase::SetModified(bChanged);
}

void CPropertyPageTrading::Reset()
{
	UpdateSettings(SET);
	UpdateButtonsHistory();

	OnBnClickedCheckKeepHistory(0, 0, NULL);

	SetModified(FALSE);
}

// CPropertyPageTrading message handlers

BOOL CPropertyPageTrading::OnQueryCancel()
{
	return CPropertyPageTradingBase::OnQueryCancel();
}

BOOL CPropertyPageTrading::OnApply()
{
	UpdateSettings(GET);

	CWindow wnd = GetParent();
	if (wnd)
		wnd.SendMessage(UM_APPLY, this->IDD, (LPARAM)&m_settings);

	SetModified(FALSE);

	return CPropertyPageTradingBase::OnApply();
}

BOOL CPropertyPageTrading::OnInitDialog(CWindow wndFocus, LPARAM lInitParam)
{
	Reset();

	m_initialized = TRUE;

	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX Property Pages should return FALSE
}

void CPropertyPageTrading::OnBnClickedCheckConfirmTransaction(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsTrading(GET);
	SetModified();
}

void CPropertyPageTrading::OnBnClickedCheckConfirmTransactionCancel(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsTrading(GET);
	SetModified();
}

void CPropertyPageTrading::OnBnClickedCheckPrintTransactionError(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsTrading(GET);
	SetModified();
}

void CPropertyPageTrading::OnBnClickedCheckCancelAll(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsTrading(GET);
	SetModified();
}

void CPropertyPageTrading::OnBnClickedCheckAutoprice(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsTrading(GET);
	SetModified();
}

void CPropertyPageTrading::OnBnClickedCheckSpread(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsTrading(GET);
	SetModified();
}

void CPropertyPageTrading::OnBnClickedCheckBestPriceIsCurrentPrice(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsTrading(GET);
	SetModified();
}

void CPropertyPageTrading::OnBnClickedCheckAutoregisterStoporder(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsTrading(GET);
	SetModified();
}

void CPropertyPageTrading::OnBnClickedButtonClearHistory(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	LPCTSTR msg = TEXT("Вы уверены, что хотите очистить историю транзакций?");
	int ret = my::MessageBox(*this, msg, MB_ICONQUESTION|MB_YESNOCANCEL);
	if (ret == IDYES)
	{	
		CWindow wnd = theApp.GetMainWnd(); 
		UINT uMsg = UM_REMOVE_TRANSACTION;
		::SendMessage(wnd, uMsg, (WPARAM)NULL, History::F_CLEAR);
		::SendMessage(wnd, uMsg, (WPARAM)NULL, History::F_UPDATE);
	}
}

void CPropertyPageTrading::OnBnClickedButtonEditHistory(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	CWindow wnd = theApp.GetMainWnd();
	if (wnd)
		wnd.PostMessage(UM_HISTORY, 0, 0);
}

void CPropertyPageTrading::OnBnClickedCheckKeepHistory(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateButtonsHistory();

	SetModified();
}

void CPropertyPageTrading::OnBnClickedCheckHistoryKeepCancel(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsHistory(GET);
	SetModified();
}

void CPropertyPageTrading::OnBnClickedCheckModifyHistory(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateButtonsHistoryModify();
	UpdateSettingsHistory(GET);

	SetModified();
}

void CPropertyPageTrading::OnBnClickedCheckHistoryConfirmRemove(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateSettingsHistory(GET);
	SetModified();
}

BOOL CPropertyPageTrading::OnKillActive()
{
#if 0
	Reset();
#endif
	return CPropertyPageTradingBase::OnKillActive();
}
