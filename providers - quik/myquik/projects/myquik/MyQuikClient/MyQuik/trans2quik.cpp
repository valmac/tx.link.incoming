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
 *  trans2quik.cpp : implementation file
 */

#include "stdafx.h"
#include "trans2quik.h"
#include "MyQuik.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

//
//class TRANS2QUIK 
//

TRANS2QUIK::TRANS2QUIK()
{
	m_hWnd = NULL;
	m_status = -1;
	ClearState();
}

TRANS2QUIK::~TRANS2QUIK()
{
	this->Close();
}

DWORD TRANS2QUIK::Init(HWND hWnd)
{
	DWORD ret = 0;
	LPCTSTR name;

	m_hWnd = hWnd;
#if 0
	name = TRANS2QUIK_DLL_NAME;
#else
	name = theApp.GetSettings().common.path.trans2quik;
	if (name[0] == TEXT('\0'))
		name = TRANS2QUIK_DLL_NAME;
#endif
	HMODULE hMod = ::LoadLibrary(name);
	if (hMod == NULL)
		return m_status = GetLastError();
	m_hMod = hMod;

	// Заполняем таблицу функций:
	this->funcs.connect = (trans2quik_connect)::GetProcAddress(hMod, "_TRANS2QUIK_CONNECT@16");
	this->funcs.disconnect = (trans2quik_disconnect)::GetProcAddress(hMod, "_TRANS2QUIK_DISCONNECT@12");
	this->funcs.is_quik_connected = (trans2quik_is_quik_connected)::GetProcAddress(hMod, "_TRANS2QUIK_IS_QUIK_CONNECTED@12");
	this->funcs.is_dll_connected = (trans2quik_is_dll_connected)::GetProcAddress(hMod, "_TRANS2QUIK_IS_DLL_CONNECTED@12");
	this->funcs.send_sync_transaction = (trans2quik_send_sync_transaction)::GetProcAddress(hMod, "_TRANS2QUIK_SEND_SYNC_TRANSACTION@36");
	this->funcs.send_async_transaction = (trans2quik_send_async_transaction)::GetProcAddress(hMod, "_TRANS2QUIK_SEND_ASYNC_TRANSACTION@16");
	this->funcs.set_connection_status_calback = (trans2quik_set_connection_status_calback)::GetProcAddress(hMod, "_TRANS2QUIK_SET_CONNECTION_STATUS_CALLBACK@16");
	this->funcs.set_transaction_reply_calback = (trans2quik_set_transaction_reply_calback)::GetProcAddress(hMod, "_TRANS2QUIK_SET_TRANSACTIONS_REPLY_CALLBACK@16");

	m_status = S_OK;

	return ret;
}

int TRANS2QUIK::CheckConnection()
{
	WPARAM wParam = (WPARAM)-1;
	LPARAM lParam = 0; 
	long code, result;
	code = -1;
	result = -1;
	if (! Initialized())
	{
		code = -2; // чтобы можно было послат сообщение;
		goto end;
	}
	result = this->funcs.is_dll_connected (&code, m_error.text, sizeof (m_error.text));
	if (result != TRANS2QUIK_DLL_CONNECTED)
	{
		char szQUIKTerminalPath [MAX_PATH_EX];
		USES_CONVERSION;
		// Удаляем название файла из полного пути:
		LPCTSTR path = theApp.m_settings.common.path.quik;
		int len = lstrlen(path);
		int i;
		for (i = len - 1; i >= 0; i--)
		{
			if (path[i] == TEXT('\\'))
			{
				break;
			}
		} // for (i)
		
		SAFE_STRCPY(szQUIKTerminalPath, T2A(theApp.m_settings.common.path.quik));
		szQUIKTerminalPath[i] = 0;
		result = this->funcs.connect (szQUIKTerminalPath, &code, m_error.text, sizeof (m_error.text));
		if (result != TRANS2QUIK_SUCCESS)
		{
			wParam = (WPARAM)result;
			lParam = (LPARAM)&m_error;
			goto end;
		}
	}
	result = this->funcs.is_quik_connected (&code, m_error.text, sizeof (m_error.text));
	if (result != TRANS2QUIK_QUIK_CONNECTED)
	{
		wParam = (WPARAM)result;
		lParam = (LPARAM)&m_error;
		goto end;
	}
	result = TRANS2QUIK_SUCCESS;
	wParam = (WPARAM)S_OK;
	lParam = (LPARAM)NULL;

end:
	// Отправляем сообщение только если изменилось состояние:
	if (m_error.result != result || m_error.extCode != code)
		if (m_hWnd)
			::PostMessage(m_hWnd, UM_TRANS2QUIK, wParam, lParam);
	
	m_error.result = result;
	m_error.extCode = code;
	
	return result;
}

void TRANS2QUIK::Close()
{
	if (m_hMod)
	{
		::FreeLibrary(m_hMod);
	}
}

void TRANS2QUIK::ClearState()
{
	m_error.result = -1;
	m_error.extCode = -1;
}

LPCTSTR TRANS2QUIK::ResultToString(int result)
{
	static LPCTSTR strs[] = {
		TEXT("TRANS2QUIK_SUCCESS"),
		TEXT("TRANS2QUIK_FAILED"),
		TEXT("TRANS2QUIK_QUIK_TERMINAL_NOT_FOUND"),
		TEXT("TRANS2QUIK_DLL_VERSION_NOT_SUPPORTED"),
		TEXT("TRANS2QUIK_ALREADY_CONNECTED_TO_QUIK"),
		TEXT("TRANS2QUIK_WRONG_SYNTAX"),
		TEXT("TRANS2QUIK_QUIK_NOT_CONNECTED"),
		TEXT("TRANS2QUIK_DLL_NOT_CONNECTED"),
		TEXT("TRANS2QUIK_QUIK_CONNECTED"),
		TEXT("TRANS2QUIK_QUIK_DISCONNECTED"),
		TEXT("TRANS2QUIK_DLL_CONNECTED"),
		TEXT("TRANS2QUIK_DLL_DISCONNECTED"),
		TEXT("TRANS2QUIK_MEMORY_ALLOCATION_ERROR"),
		TEXT("TRANS2QUIK_WRONG_CONNECTION_HANDLE"),
		TEXT("TRANS2QUIK_WRONG_INPUT_PARAMS"),
		TEXT("INVALID_RESULT_CODE"),
	};
	if (result >= 0 && result <= TRANS2QUIK_WRONG_INPUT_PARAMS)
		return strs[result];
	else
		return strs[SIZEOF_ARRAY(strs) - 1];
}
