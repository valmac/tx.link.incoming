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
 *  DlgStop2.h
 */

#pragma once

#include "myctrlx.h"

//
// CDlgStop2 dialog
//
class CDlgStop2 : public CDialogImpl<CDlgStop2>, public CWinDataExchange<CDlgStop2>
{
public:
	CDlgStop2();
	virtual ~CDlgStop2();

// Dialog Data
	enum { IDD = IDD_STOP };

	LPCTSTR GetName() const { return TEXT("DlgStop"); }

	void Init(QuoteTable * pTable, HWND hActiveWindow);

protected:
	//const QuoteTable::Properties * m_pProperties;
	QuoteTable * m_pTable;

	int m_operation;
	int m_iExpireDate;
	int m_bid;

	CStatic m_picture;

	HWND m_hActiveWindow;

	struct Edits {
		CMyEdit price;
		CMyEdit price2;
		CMyEdit quantity;
	} m_edits;

#if USE_STATIC_MENU_USER_VALUES
	CMenu m_menuUserValues;
#endif

protected:
	void UpdateControls(const QuoteTable::Properties * pProperties);
	int AddStringToCombo(LPCTSTR name, int id);
	void SetPicture(UINT id);

	void ModifyMenuEdit(CMenu & menu, UINT id);

	QuoteTable * GetCurrentTable();

protected:
	void KeepUserValues(QuoteTable * pTable);
	void RestoreUserValues(QuoteTable * pTable);

protected:
	void OnBnClickedRadioExpire(int id);

protected:
	BEGIN_MSG_MAP_EX(CDlgStop2)
		MSG_WM_INITDIALOG(OnInitDialog)
		COMMAND_HANDLER_EX(IDOK, BN_CLICKED, OnBnClickedOk)
		COMMAND_HANDLER_EX(IDCANCEL, BN_CLICKED, OnCancel)
		COMMAND_HANDLER_EX(IDC_RADIO_BUY, BN_CLICKED, OnBnClickedRadioBuy)
		COMMAND_HANDLER_EX(IDC_RADIO_SELL, BN_CLICKED, OnBnClickedRadioSell)
		COMMAND_HANDLER_EX(IDC_RADIO_EXPIRE_TODAY, BN_CLICKED, OnBnClickedRadioExpireToday)
		COMMAND_HANDLER_EX(IDC_RADIO_EXPIRE_DATE, BN_CLICKED, OnBnClickedRadioExpireDate)
		COMMAND_HANDLER_EX(IDC_RADIO_EXPIRE_GTC, BN_CLICKED, OnBnClickedRadioExpireGtc)
		NOTIFY_HANDLER_EX(IDC_SPIN_STOP_STOPPRICE, UDN_DELTAPOS, OnDeltaposSpinStopStopprice)
		NOTIFY_HANDLER_EX(IDC_SPIN_STOP_PRICE, UDN_DELTAPOS, OnDeltaposSpinStopPrice)
		NOTIFY_HANDLER_EX(IDC_SPIN_STOP_QUANTITY, UDN_DELTAPOS, OnDeltaposSpinStopQuantity)
		MSG_WM_CLOSE(OnClose)
		MSG_WM_DESTROY(OnDestroy)
		MSG_WM_ACTIVATE(OnActivate)
		MSG_WM_RBUTTONUP(OnRButtonUp)
		MESSAGE_HANDLER(WM_COMMAND, OnCmdMsg)
	END_MSG_MAP()

	BEGIN_DDX_MAP(CDlgStop2)
		DDX_CONTROL(IDC_EDIT_PRICE, m_edits.price)
		DDX_CONTROL(IDC_EDIT_STOPPRICE, m_edits.price2)
		DDX_CONTROL(IDC_EDIT_QUANTITY, m_edits.quantity)
	END_DDX_MAP()

	BOOL OnInitDialog(CWindow wndFocus, LPARAM lInitParam);

	void OnBnClickedOk(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnCancel(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedRadioBuy(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedRadioSell(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedRadioExpireToday(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedRadioExpireDate(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnBnClickedRadioExpireGtc(UINT uNotifyCode, int nID, CWindow wndCtl);
	LRESULT OnDeltaposSpinStopStopprice(NMHDR *pNMHDR);
	LRESULT OnDeltaposSpinStopPrice(NMHDR *pNMHDR);
	LRESULT OnDeltaposSpinStopQuantity(NMHDR *pNMHDR);
	void OnClose();
	void OnDestroy();

	void OnActivate(UINT nState, BOOL bMinimized, CWindow wndOther);

	void OnRButtonUp(UINT nFlags, CPoint point);

	LRESULT OnCmdMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);

};
