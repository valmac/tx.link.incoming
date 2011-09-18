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
 *  PropertyPageCommon.cpp : implementation file
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "PropertyPageCommon.h"

int __stdcall BrowseProc(HWND hwnd, UINT uMsg, LPARAM lParam, LPARAM lpData)
{
    switch (uMsg)
    {
		case BFFM_INITIALIZED:
		{
			LPCITEMIDLIST pIdList = reinterpret_cast<LPCITEMIDLIST>(lpData);
			if (pIdList)
			{
				::SendMessage(hwnd, BFFM_SETEXPANDED, FALSE, reinterpret_cast<LPARAM>(pIdList));
				::SendMessage(hwnd, BFFM_SETSELECTION, FALSE, reinterpret_cast<LPARAM>(pIdList));
			}
		}
		break;
	}
    return 0;
}

HRESULT SelectFolder(LPCTSTR initPath, LPTSTR path, int size)
{
	HRESULT hr = -1;

    BROWSEINFO info;
	DEFINE_PATH_EX(szDir);
	DEFINE_PATH_EX(szDisplayName);
    LPITEMIDLIST pidl;
    LPMALLOC pShellMalloc;

	if (::SHGetMalloc(&pShellMalloc) == NO_ERROR)
    {
		// Путь для инициализации:
		SFGAOF sfgaoOut;
		::SHParseDisplayName(initPath, NULL, &pidl, SFGAO_FOLDER|SFGAO_VALIDATE, &sfgaoOut); // SFGAO_BROWSABLE

        memset(&info, 0x00, sizeof(info));
        info.hwndOwner = 0;
        info.pidlRoot = NULL;
        info.pszDisplayName = szDisplayName;
        info.lpszTitle = TEXT("Выберите папку для хранения истории транзакций");
        info.ulFlags = BIF_USENEWUI | BIF_VALIDATE | BIF_RETURNONLYFSDIRS|BIF_STATUSTEXT;
        info.lpfn = BrowseProc;
		info.lParam = (LPARAM)pidl;

		pidl = ::SHBrowseForFolder(&info);

        if (pidl)
        {
			if (::SHGetPathFromIDList(pidl, szDir))
            {
				_tcscpy_s(path, size, szDir);
				hr = S_OK;
            }

            pShellMalloc->Free(pidl);
            pShellMalloc->Release();
        }
		pShellMalloc->Release();
    }
	return hr;
}

int NormalizePath(LPCTSTR src, LPTSTR dst, size_t size)
{
	if (src[1] != TEXT(':'))
	{// Указан относительный путь - преобразуем его в полный
		DEFINE_PATH_EX(buf);
		DEFINE_PATH_EX(buf2);
		SAFE_TSTRCPY(buf, theApp.GetAppPath());
		if (src[0] != TEXT('\\'))
		{
			int len = lstrlen(buf);
			buf[len] = TEXT('\\');
			buf[len+1] = TEXT('\0');
		}
		if (src == dst)
		{
			SAFE_TSTRCPY(buf2, src);
			src = buf2;
		}
		_stprintf_s(dst, size, TEXT("%s%s"), buf, src);
		return S_OK;
	}
	else
	{
		if (src != dst)
			_tcscpy_s(dst, size, src);
	}
	return E_PENDING;
}

//
// CPropertyPageCommon dialog
//
CPropertyPageCommon::CPropertyPageCommon(const ::Settings::Common & common, const ::Settings::Log & log, const ::Settings::Server & server)
{
	m_initialized = FALSE;
	m_settings.common = common;
	m_settings.log = log;
	m_settings.server = server;
}

CPropertyPageCommon::~CPropertyPageCommon()
{
}

void CPropertyPageCommon::UpdateSettings(int set)
{
	CWindow wnd;
	CButton btn;

	UpdatePath(-1, set);
	UpdateCommon(set);

	// DDE-сервер:
	wnd = GetDlgItem(IDC_EDIT_SERVER_NAME);
	if (set)
	{
		wnd.SetWindowText(m_settings.server.name);
	}
	else
	{
		wnd.GetWindowText(m_settings.server.name, SIZEOF_ARRAY(m_settings.server.name));
	}
	btn = GetDlgItem(IDC_BUTTON_CLEAR_CACHE);
	wnd = GetDlgItem(IDC_STATIC_SERVER_STATUS);
	if (set)
	{
		if (theApp.GetServerStatus() == S_OK)
		{
			btn.EnableWindow();
			wnd.SetWindowText(TEXT(""));
		}
		else
		{
			btn.EnableWindow(FALSE);
			wnd.SetWindowText(TEXT("Сервер не отвечает!"));
			wnd.EnableWindow(FALSE);
		}
	}
}

void CPropertyPageCommon::UpdateCommon(int set)
{
	CButton btn;

	// Взаимодействие с QUIK:
	btn = GetDlgItem(IDC_CHECK_AUTO_CHECK_CONNECTION);
	if (set)
		btn.SetCheck(m_settings.common.quik.autoCheckConnection ? BST_CHECKED : BST_UNCHECKED);
	else
		m_settings.common.quik.autoCheckConnection = (btn.GetCheck() == BST_CHECKED);

	// Журнал сообщений:
	btn = GetDlgItem(IDC_CHECK_PRINT_TRANSACTION);
	if (set)
		btn.SetCheck(m_settings.log.printTransaction ? BST_CHECKED : BST_UNCHECKED);
	else
		m_settings.log.printTransaction = (btn.GetCheck() == BST_CHECKED);
	btn = GetDlgItem(IDC_CHECK_SAVE_ON_DISK);
	if (set)
		btn.SetCheck(m_settings.log.saveOnDisk ? BST_CHECKED : BST_UNCHECKED);
	else
		m_settings.log.saveOnDisk = (btn.GetCheck() == BST_CHECKED);
}

void CPropertyPageCommon::UpdatePath(int i, int set)
{	
	LPTSTR paths[] = {
		m_settings.common.path.quik, m_settings.common.path.trans2quik, m_settings.common.path.history, m_settings.common.path.log
	};
	if (i == -1)
	{
		CComboBox comboBox(GetDlgItem(IDC_COMBO1));
		i = comboBox.GetCurSel(); 
	}
	if (i >= 0)
	{
		CEdit edit(GetDlgItem(IDC_EDIT_QUIK_PATH));
		if (set)
			SetTextRightAligned(edit, paths[i]);
		else
			edit.GetWindowText(paths[i], SIZEOF_ARRAY(m_settings.common.path.quik));
	}
}

LPCTSTR CPropertyPageCommon::GetPathName(int type)
{
	static LPCTSTR names[] = {
		TEXT("Путь к терминалу QUIK"),
		TEXT("Путь к библиотеке TRANS2QUIK"),
		TEXT("Папка для хранения истории транзакций"),
		TEXT("Журнал программы MyQuik"),
	};
	return names[type];
}

void CPropertyPageCommon::SetModified(BOOL bChanged)
{
	if (bChanged && m_initialized)
	{
		::SendMessage(theApp.GetMainWnd(), UM_APPLY_SETTINGS, APPLY_SETTINGS_COMMON, (LPARAM)&m_settings);
	}
	CPropertyPageImpl::SetModified(bChanged);
}


//
// CPropertyPageCommon message handlers
//

BOOL CPropertyPageCommon::OnInitDialog(CWindow wndFocus, LPARAM lInitParam)
{
	CComboBox comboBox(GetDlgItem(IDC_COMBO1));
	for (int i = 0; i < PATH_LAST; i++)
		comboBox.AddString(GetPathName(i));
	comboBox.SetCurSel(0);

	UpdateSettings(SET);

#if USE_TRANSPARENCE
#else
	CWindow wnd;
	wnd = GetDlgItem(IDC_STATIC_TRANSPARENCE);
	wnd.ShowWindow(SW_HIDE);
	wnd = GetDlgItem(IDC_EDIT_TRANSPARENCE);
	wnd.ShowWindow(SW_HIDE);
	wnd = GetDlgItem(IDC_SPIN1);
	wnd.ShowWindow(SW_HIDE);
#endif

	m_initialized = TRUE;

	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX Property Pages should return FALSE
}

BOOL CPropertyPageCommon::OnQueryCancel()
{
	UpdateSettings(GET);
	::SendMessage(GetParent(), UM_CANCEL, this->IDD, (LPARAM)&m_settings);
	return TRUE;    // ok to cancel
}

BOOL CPropertyPageCommon::OnApply()
{
	UpdateSettings(GET);

	::SendMessage(GetParent(), UM_APPLY, this->IDD, (LPARAM)&m_settings);

	SetModified(FALSE);

	return CPropertyPageImpl<CPropertyPageCommon>::OnApply();
}

void CPropertyPageCommon::OnEnChangeEditPath(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	LPTSTR paths[] = {
		m_settings.common.path.quik, m_settings.common.path.trans2quik, m_settings.common.path.history, m_settings.common.path.log,
	};
	CComboBox comboBox(GetDlgItem(IDC_COMBO1));
	int i = comboBox.GetCurSel();
	TSTRING_SMALL2(str, size);
	CWindow wnd = GetDlgItem(IDC_EDIT_QUIK_PATH);
	wnd.GetWindowText(str, size);
	if (0 != lstrcmp(str, paths[i]))
		SetModified();
}

//static LPCTSTR s_strFilter = TEXT("Текстовые файлы\0*.txt\0Все файлы\0*.*\0\0");
BOOL CPropertyPageCommon::OnSelectPath(int i, UINT id, LPCTSTR filterName, LPCTSTR filter)
{
	BOOL changed = FALSE;

	CWindow wnd;
	DEFINE_PATH_EX(path0);
	DEFINE_PATH_EX(path);
	DEFINE_PATH_EX(fullPath);
	const size_t size = SIZEOF_ARRAY(path);

	wnd = GetDlgItem(id);
	wnd.GetWindowText(path, size);

	NormalizePath(path, path0, size);

	SAFE_TSTRCPY(fullPath, path0);
	LPTSTR name = _tcsrchr(path0, TEXT('\\'));	
	if (name)
	{
		name[0] = 0;
		name += 1;
	}

	TSTRING_SMALL(str);
	int len, n = 0;
	len = lstrlen(filterName);
	memcpy(str, filterName, len*sizeof(TCHAR)); n += len;
	str[n++] = TEXT('\0'); str[n++] = TEXT('*'); str[n++] = TEXT('.');
	len = lstrlen(filter);
	memcpy(&str[n], filter, len*sizeof(TCHAR)); n += len;
	len = 16;
	memcpy(&str[n], TEXT("\0Все файлы\0*.*\0\0"), len*sizeof(TCHAR)); n += len;

	OPENFILENAME ofn;
	TCHAR szFile[MAX_PATH_EX];

	ZeroMemory(&ofn, sizeof(ofn));
	ofn.lStructSize = sizeof(ofn);
	ofn.hwndOwner = *this;
	ofn.lpstrFile = szFile;
	ofn.lpstrFile[0] = '\0';
	ofn.nMaxFile = sizeof(szFile);
	ofn.lpstrFilter = str;
	ofn.nFilterIndex = 1;
	ofn.lpstrDefExt = filter;
	ofn.lpstrInitialDir = path0;
#if 0
	ofn.lpstrFile = name;
	ofn.nMaxFile = SIZEOF_ARRAY(name);
#endif
	ofn.Flags |= OFN_FILEMUSTEXIST;
	ofn.lpstrTitle = this->GetPathName(i);

	if (TRUE == ::GetOpenFileName(&ofn)) 
	{
		LPCTSTR newPath = ofn.lpstrFile;
		if (0 != lstrcmp(fullPath, newPath))
			changed = TRUE;
		SetTextRightAligned(id, newPath);
	}

	return changed;
}

void CPropertyPageCommon::OnBnClickedButtonPath(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	CComboBox comboBox(GetDlgItem(IDC_COMBO1));
	int i = comboBox.GetCurSel();
	UINT id = IDC_EDIT_QUIK_PATH;
	BOOL changed = FALSE;
	switch (i)
	{
	case PATH_QUIK:
		changed = OnSelectPath(i, id, TEXT("Исполняемые файлы (exe)"), TEXT("exe"));
		break;
	case PATH_TRANS2QUIK:
		changed = OnSelectPath(i, id, TEXT("Загружаемые библиотеки (dll)"), TEXT("dll"));
		break;
	case PATH_HISTORY:
		{
			DEFINE_PATH_EX(text);
			DEFINE_PATH_EX(path);
			size_t size = SIZEOF_ARRAY(path);

			CWindow wnd = GetDlgItem(id);
			wnd.GetWindowText(path, size);

			NormalizePath(path, path, size);
#if 0
			CFolderDialog dlg;
			dlg.DoModal();
#else
			HRESULT hr = SelectFolder(path, text, SIZEOF_ARRAY(text));
			if (hr == S_OK)
			{
				if (0 != lstrcmp(path, text))
					changed = TRUE;
				SetTextRightAligned(id, text);
			}
#endif
		}
		break;		
	case PATH_LOG:
		changed = OnSelectPath(i, id, TEXT("Текстовые документы (log)"), TEXT("log"));
		break;
	} // switch (i)
	if (changed)
	{
		SetModified();
	}
}

void CPropertyPageCommon::OnBnClickedCheckPrintTransaction(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	SetModified();
}

void CPropertyPageCommon::OnBnClickedCheckSaveOnDisk(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	SetModified();
}

void CPropertyPageCommon::OnBnClickedButtonClearCache(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	LPCTSTR msg = TEXT("Вы уверены, что хотите очистить кэш сервера?");
	if (IDYES == my::MessageBox(*this, (LPCTSTR)msg, MB_ICONQUESTION|MB_YESNOCANCEL))
	{
		theApp.ClearCacheServer();
	}
}

void CPropertyPageCommon::SetTextRightAligned(UINT id, LPCTSTR text)
{
	CEdit edit(GetDlgItem(id));
	SetTextRightAligned(edit, text);
}

void CPropertyPageCommon::SetTextRightAligned(CEdit & edit, LPCTSTR text)
{
	edit.SetWindowText(text);
	int len = edit.LineLength();
	edit.SetSel(len, len);
}


void CPropertyPageCommon::OnCbnSelchangeComboPath(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdatePath(-1, SET);
}

void CPropertyPageCommon::OnBnClickedCheckAutoCheckConnection(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	UpdateCommon(GET);
	SetModified();
}
