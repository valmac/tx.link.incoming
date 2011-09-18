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
 *  PropertySheetSettings.h
 */

#pragma once

#include "PropertyPageCommon.h"
#include "PropertyPageTrading.h"
#include "PropertyPageInstruments.h"
#include "PropertyPageView.h"
#include "PropertyPageGlass.h"
#include "PropertyPageOSD.h"
#include "PropertyPageListOfDeals.h"
#include "PropertyPageKeys.h"

// CPropertySheetSettings

class CPropertySheetSettings : public CPropertySheetImpl<CPropertySheetSettings>
{
public:
	CPropertySheetSettings(_U_STRINGorID title = (LPCTSTR) NULL, UINT uStartPage = 0, HWND hWndParent = NULL);
	virtual ~CPropertySheetSettings();

	static LPCTSTR GetName() { return TEXT("ProperySheetSettings"); }

	const Settings * GetSettings() const { return &m_settings; }
	int GetLastIndex() const { return m_activePage; }

	void Init(const Settings * pSettings, int activePage);

protected:
	void SavePosition();

protected:
	int m_initialized;

	::Settings m_settings;

	CPropertyPageCommon * m_pageCommon;
	CPropertyPageTrading * m_pageTrading;
	CPropertyPageInstruments * m_pageInstruments;
	CPropertyPageView * m_pageView;
	CPropertyPageGlass * m_pageGlass;
	CPropertyPageOSD * m_pageOSD;
	CPropertyPageListOfDeals * m_pageListOfDeals;
	CPropertyPageKeys * m_pageKeys;

	int m_activePage;

public:
	void OnSheetInitialized();

protected:
	BEGIN_MSG_MAP_EX(CPropertySheetSettings)
		MSG_WM_SHOWWINDOW(OnShowWindow)
		MSG_WM_DESTROY(OnDestroy)
		//COMMAND_ID_HANDLER_EX(IDOK, OnOK)
		MESSAGE_RANGE_HANDLER(UM_BASE + 1, UM_LAST - 1, OnWndMsg)
		CHAIN_MSG_MAP(CPropertySheetImpl<CPropertySheetSettings>)
	END_MSG_MAP()

	void OnShowWindow(BOOL bShow, UINT nStatus);
	void OnDestroy();

	void OnOK(UINT uNotifyCode, int nID, CWindow wndCtl);

	LRESULT OnWndMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
};

typedef CPropertySheetImpl<CPropertySheetSettings> CPropertySheetSettingsBase;

