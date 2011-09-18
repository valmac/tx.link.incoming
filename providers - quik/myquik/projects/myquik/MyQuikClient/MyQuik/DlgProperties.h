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
 *  DlgProperties.h
 */

#pragma once

#include "tables.h"

// CDlgProperties dialog

class CDlgProperties : public CDialogImpl<CDlgProperties>, public CWinDataExchange<CDlgProperties>
{
public:
	CDlgProperties();
	virtual ~CDlgProperties();

// Dialog Data
	enum { IDD = IDD_PROPERTIES };

	void Init(LPCTSTR name, const QuoteTable::Properties * pProperties, const ListOfTableProperties * pListOfProperties);

	void SavePosition();

	const QuoteTable::Properties * GetProperties() const { return &m_properties; }

	static LPCTSTR GetName() { return TEXT("DlgProperties"); };

protected:
	static void InsertStringInCombo(LPCTSTR str, CComboBox combo);

protected:
	QuoteTable::Properties m_properties;
	const ListOfTableProperties * m_pListOfProperties;
	BOOL m_initialized;
	BOOL m_changing;

protected:
	void UpdateProperties(QuoteTable::Properties * pProperties, int set);
	void UpdatePropertiesTradingAutos(QuoteTable::Properties * pProperties, int set);
	void UpdateStopRelative();
	void UpdateStopAbsolut();
	void Update_xQ(QuoteTable::Properties * pProperties, int set = 1);

protected:
	BEGIN_MSG_MAP_EX(CDlgProperties)
		MSG_WM_INITDIALOG(OnInitDialog)
		MSG_WM_CLOSE(OnClose)
		COMMAND_HANDLER_EX(IDOK, BN_CLICKED, OnBnClickedOk)
		COMMAND_HANDLER_EX(IDCANCEL, BN_CLICKED, OnBnClickedCancel)
		COMMAND_HANDLER_EX(IDC_RADIO_STOP_RELATIVE, BN_CLICKED, OnBnClickedRadioStopRelative)
		COMMAND_HANDLER_EX(IDC_RADIO_STOP_ABSOLUT, BN_CLICKED, OnBnClickedRadioStopAbsolut)
		COMMAND_HANDLER_EX(IDC_EDIT_QUANTITY, EN_CHANGE, OnEnChangeEditQuantity)
		COMMAND_HANDLER_EX(IDC_EDIT_STOP_RELATIVE, EN_CHANGE, OnEnChangeEditStopRelative)
		COMMAND_HANDLER_EX(IDC_EDIT_STOP_ABSOLUT, EN_CHANGE, OnEnChangeEditStopAbsolut)
		COMMAND_HANDLER_EX(IDC_EDIT_SLIPPAGE, EN_CHANGE, OnEnChangeEditSlippage)
		NOTIFY_HANDLER_EX(IDC_SPIN_DPRICE, UDN_DELTAPOS, OnDeltaSpinPrice)
		NOTIFY_HANDLER_EX(IDC_SPIN_QUANTITY, UDN_DELTAPOS, OnDeltaSpinQuantity)
		NOTIFY_HANDLER_EX(IDC_SPIN_SPREAD, UDN_DELTAPOS, OnDeltaposSpinSpread)
		NOTIFY_HANDLER_EX(IDC_SPIN_STOP_RELATIVE, UDN_DELTAPOS, OnDeltaposSpinStopRelative)
		NOTIFY_HANDLER_EX(IDC_SPIN_STOP_ABSOLUT, UDN_DELTAPOS, OnDeltaposSpinStopAbsolut)
		NOTIFY_HANDLER_EX(IDC_SPIN_SLIPPAGE, UDN_DELTAPOS, OnDeltaposSpinSlippage)
		COMMAND_HANDLER_EX(IDC_COMBO_TRADING_AUTOS, CBN_SELCHANGE, OnCbnSelchangeComboTradingAutos)
	END_MSG_MAP()

public:
	BOOL OnInitDialog(CWindow wndFocus, LPARAM lInitParam);

	void OnClose();
	void OnBnClickedOk(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedCancel(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedRadioStopRelative(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedRadioStopAbsolut(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnEnChangeEditQuantity(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnEnChangeEditStopRelative(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnEnChangeEditStopAbsolut(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnEnChangeEditSlippage(UINT uNotifyCode, int nID, CWindow wndCtl);
	LRESULT OnDeltaSpinPrice(NMHDR *pNMHDR);
	LRESULT OnDeltaSpinQuantity(NMHDR *pNMHDR);
	LRESULT OnDeltaposSpinSpread(NMHDR *pNMHDR);
	LRESULT OnDeltaposSpinStopRelative(NMHDR *pNMHDR);
	LRESULT OnDeltaposSpinStopAbsolut(NMHDR *pNMHDR);
	LRESULT OnDeltaposSpinSlippage(NMHDR *pNMHDR);
	void OnCbnSelchangeComboTradingAutos(UINT uNotifyCode, int nID, CWindow wndCtl);
};
