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
 *  PropertySheetSettings.cpp : implementation file
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "PropertySheetSettings.h"


// CPropertySheetSettings

CPropertySheetSettings::CPropertySheetSettings(_U_STRINGorID title, UINT uStartPage, HWND hWndParent)
	: CPropertySheetSettingsBase(title, uStartPage, hWndParent)
{
	m_pageTrading = NULL;
	m_pageCommon = NULL;
	m_pageInstruments = NULL;
	m_pageView = NULL;
	m_pageGlass = NULL;
	m_pageOSD = NULL;
	m_pageListOfDeals = NULL;
	m_pageKeys = NULL;

	m_activePage = 0;

	m_initialized = FALSE;
}

CPropertySheetSettings::~CPropertySheetSettings()
{
	SAFE_DELETE(m_pageTrading);	
	SAFE_DELETE(m_pageCommon);
	SAFE_DELETE(m_pageInstruments);
	SAFE_DELETE(m_pageView);
	SAFE_DELETE(m_pageGlass);
	SAFE_DELETE(m_pageOSD);
	SAFE_DELETE(m_pageListOfDeals);
	SAFE_DELETE(m_pageKeys);
}

void CPropertySheetSettings::Init(const Settings * pSettings, int activePage)
{
	m_settings = *pSettings;

	m_pageCommon = new CPropertyPageCommon(m_settings.common, m_settings.log, m_settings.server);
	if (m_pageCommon != NULL)
		AddPage(*m_pageCommon);
	m_pageTrading = new CPropertyPageTrading(m_settings.trading);
	if (m_pageTrading != NULL)
		AddPage(*m_pageTrading);
	m_pageInstruments = new CPropertyPageInstruments(m_settings.trading.instrument.name);
	if (m_pageInstruments != NULL)
		AddPage(*m_pageInstruments);
	m_pageKeys = new CPropertyPageKeys(m_settings.shortcuts);
	if (m_pageKeys != NULL)
		AddPage(*m_pageKeys);
	m_pageView = new CPropertyPageView(m_settings.presentation);
	if (m_pageView != NULL)
		AddPage(*m_pageView);
	m_pageListOfDeals = new CPropertyPageListOfDeals(m_settings.presentation.list);
	if (m_pageListOfDeals != NULL)
		AddPage(*m_pageListOfDeals);
	m_pageOSD = new CPropertyPageOSD(m_settings.presentation.osd);
	if (m_pageOSD != NULL)
		AddPage(*m_pageOSD);
	m_pageGlass = new CPropertyPageGlass(m_settings.presentation.glass);
	if (m_pageGlass != NULL)
		AddPage(*m_pageGlass);

	m_activePage = activePage;
}

void CPropertySheetSettings::SavePosition()
{
	theApp.SaveWindowPosition(*this, GetName());
}

// CPropertySheetSettings message handlers

void CPropertySheetSettings::OnSheetInitialized()
{
	HICON hIcon = theApp.LoadSmallIcon(IDI_TOOLS);
	SetIcon(hIcon, FALSE);
	
	SetWindowText(TEXT("Настройки"));
#if 1
	theApp.DisablelHooks(FALSE);
#endif
}

LRESULT CPropertySheetSettings::OnWndMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = TRUE;
	switch (uMsg)
	{
	case UM_OK:
	case UM_APPLY:
		{
			DWORD id = (DWORD)wParam;
			if (id == IDD_SETTINGS_COMMON)
			{
				CPropertyPageCommon::Settings * pSettings = (CPropertyPageCommon::Settings*)lParam;
				m_settings.common = pSettings->common;
				m_settings.log = pSettings->log;
			}
			else if (id == IDD_SETTINGS_TRADING)
			{
				Settings::Trading * pSettings = (Settings::Trading*)lParam;
				m_settings.trading = *pSettings;
			}
			else if (id == IDD_SETTINGS_INSTRUMENTS)
			{
				LPCTSTR name = (LPCTSTR)lParam;
				SAFE_TSTRCPY(m_settings.trading.instrument.name, name);
			}
			else if (id == IDD_SETTINGS_SHORTCUTS)
			{
				Settings::Shortcuts * pSettings = (Settings::Shortcuts*)lParam;
				m_settings.shortcuts = *pSettings;
			}
			else if (id == IDD_SETTINGS_VIEW)
			{
				Settings::Presentation * pSettings = (Settings::Presentation*)lParam;
				m_settings.presentation.main = pSettings->main;
				m_settings.presentation.view = pSettings->view;
			}
			else if (id == IDD_SETTINGS_GLASS)
			{
				Settings::Presentation::Glass * pSettings = (Settings::Presentation::Glass*)lParam;
				m_settings.presentation.glass = *pSettings;
			}
			else if (id == IDD_SETTINGS_OSD)
			{
				Settings::Presentation::OSD * pSettings = (Settings::Presentation::OSD*)lParam;
				m_settings.presentation.osd = *pSettings;
			}
			else if (id == IDD_SETTINGS_LIST)
			{
				Settings::Presentation::ListOfDeals * pSettings = (Settings::Presentation::ListOfDeals*)lParam;
				m_settings.presentation.list = *pSettings;
			}
		}
		break;
	case UM_CANCEL:
		{
			CWindow wnd = theApp.GetMainWnd();
			DWORD id = (DWORD)wParam;
			if (id == IDD_SETTINGS_COMMON)
			{
				CPropertyPageCommon::Settings * pSettings = (CPropertyPageCommon::Settings*)lParam;
				CPropertyPageCommon::Settings settings;
				settings.common = m_settings.common;
				settings.log = m_settings.log;
				settings.server = m_settings.server;
				::SendMessage(wnd, UM_APPLY_SETTINGS, APPLY_SETTINGS_COMMON, (LPARAM)&settings);
			}
			else if (id == IDD_SETTINGS_VIEW)
			{
				Settings::Presentation * pSettings = (Settings::Presentation*)lParam;
				Settings::Presentation settings;
				settings.view = m_settings.presentation.view;
				settings.main = m_settings.presentation.main;
				::SendMessage(wnd, UM_APPLY_SETTINGS, APPLY_SETTINGS_PRESENTATION_MAIN, (LPARAM)&settings);
			}
			else if (id == IDD_SETTINGS_INSTRUMENTS)
				::SendMessage(wnd, UM_APPLY_SETTINGS, APPLY_SETTINGS_INSTRUMENT, (LPARAM)(LPCTSTR)m_settings.trading.instrument.name);
			else if (id == IDD_SETTINGS_GLASS)
				::SendMessage(wnd, UM_APPLY_SETTINGS, APPLY_SETTINGS_PRESENTATION_GLASS, (LPARAM)&m_settings.presentation.glass);
			else if (id == IDD_SETTINGS_OSD)
				::SendMessage(wnd, UM_APPLY_SETTINGS, APPLY_SETTINGS_PRESENTATION_OSD, (LPARAM)&m_settings.presentation.osd);
			else if (id == IDD_SETTINGS_LIST)
				::SendMessage(wnd, UM_APPLY_SETTINGS, APPLY_SETTINGS_PRESENTATION_LIST, (LPARAM)&m_settings.presentation.list);
		}
		break;
	default:
		bHandled = FALSE;
	}

	return 0;
}

void CPropertySheetSettings::OnShowWindow(BOOL bShow, UINT nStatus)
{
#if defined _DEBUG && 1
	TRACE(__FUNCTION__"(%d, %d)\r\n", bShow, nStatus);
#endif
	if (! bShow)
		return;

	if (! m_initialized)
	{
		CREATESTRUCT cs;
		if (S_OK == theApp.LoadWindowPosition(&cs, GetName()))
#if 0
			MoveWindow(cs.x, cs.y, cs.cx, cs.cy);
#else
			SetWindowPos (NULL, cs.x, cs.y, 0, 0, SWP_NOZORDER|SWP_NOSIZE);
#endif
		SetActivePage(m_activePage);

		m_initialized = TRUE;
	}
}

void CPropertySheetSettings::OnDestroy()
{
#if 1
	SavePosition();
#endif
	m_activePage = GetActiveIndex();
#if 1
	theApp.EnablelHooks();
#endif
}

void CPropertySheetSettings::OnOK(UINT uNotifyCode, int nID, CWindow wndCtl)
{
#if defined _DEBUG && 1
	TRACE(__FUNCTION__"\r\n");
#endif
}

