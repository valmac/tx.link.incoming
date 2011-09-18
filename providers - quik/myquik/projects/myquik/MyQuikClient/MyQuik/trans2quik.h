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
 *  trans2quik.h
 */

#pragma once

#include "trans2quik_api.h"

typedef long TRANS2QUIK_API (__stdcall *trans2quik_connect) (LPSTR lpstConnectionParamsString, long* pnExtendedErrorCode, LPSTR lpstrErrorMessage, DWORD dwErrorMessageSize);
typedef long TRANS2QUIK_API (__stdcall *trans2quik_disconnect) (long* pnExtendedErrorCode, LPSTR lpstrErrorMessage, DWORD dwErrorMessageSize);
typedef long TRANS2QUIK_API (__stdcall *trans2quik_is_quik_connected) (long* pnExtendedErrorCode, LPSTR lpstrErrorMessage, DWORD dwErrorMessageSize);
typedef long TRANS2QUIK_API (__stdcall *trans2quik_is_dll_connected) (long* pnExtendedErrorCode, LPSTR lpstrErrorMessage, DWORD dwErrorMessageSize);
typedef long TRANS2QUIK_API (__stdcall *trans2quik_send_sync_transaction) (LPSTR lpstTransactionString, long* pnReplyCode, PDWORD pdwTransId, double* pdOrderNum, LPSTR lpstrResultMessage, DWORD dwResultMessageSize, long* pnExtendedErrorCode, LPSTR lpstErrorMessage, DWORD dwErrorMessageSize);
typedef long TRANS2QUIK_API (__stdcall *trans2quik_send_async_transaction) (LPSTR lpstTransactionString, long* pnExtendedErrorCode, LPSTR lpstErrorMessage, DWORD dwErrorMessageSize);
typedef long TRANS2QUIK_API (__stdcall *trans2quik_set_connection_status_calback) (TRANS2QUIK_CONNECTION_STATUS_CALLBACK pfConnectionStatusCallback, long* pnExtendedErrorCode, LPSTR lpstrErrorMessage, DWORD dwErrorMessageSize);
typedef long TRANS2QUIK_API (__stdcall *trans2quik_set_transaction_reply_calback) (TRANS2QUIK_TRANSACTION_REPLY_CALLBACK pfTransactionReplyCallback, long* pnExtendedErrorCode, LPSTR lpstrErrorMessage, DWORD dwErrorMessageSize);

class TRANS2QUIK 
{
	friend class TransCtrl;
public:
	struct Message {
		char text[1024];
	};
	struct Error {
		long result;
		long extCode;
		char text[1024];
	};

	TRANS2QUIK();
	~TRANS2QUIK();

	static LPCTSTR ResultToString(int result);

protected:
	DWORD Init(HWND hWnd);
	int CheckConnection();
	void Close();
	void ClearState();

	BOOL Initialized() const { return (m_status == S_OK); }

public:
	struct Funcs {
		trans2quik_connect connect;
		trans2quik_disconnect disconnect;
		trans2quik_is_quik_connected is_quik_connected;
		trans2quik_is_dll_connected is_dll_connected;
		trans2quik_send_sync_transaction send_sync_transaction;
		trans2quik_send_async_transaction send_async_transaction;
		trans2quik_set_connection_status_calback set_connection_status_calback;
		trans2quik_set_transaction_reply_calback set_transaction_reply_calback;
	};

	Funcs funcs;

protected:
	HMODULE m_hMod;
	Message m_message;
	Error m_error;
	int m_status;
	HWND m_hWnd;
};

#define TRANS2QUIK_DLL_NAME TEXT("TRANS2QUIK.dll")
