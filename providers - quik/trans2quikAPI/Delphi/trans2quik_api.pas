{************************************************}
{  trans2quik_api.h                              }
{  Адаптация к Delphi - Ginger                   }
{  http://www.quik.ru/user/forum/import/24427/   }
{************************************************}

unit trans2quik_api;

interface

uses Windows;

const
  TRANS2QUIK_SUCCESS = 0;
  TRANS2QUIK_FAILED	 = 1;
  TRANS2QUIK_QUIK_TERMINAL_NOT_FOUND = 2;
  TRANS2QUIK_DLL_VERSION_NOT_SUPPORTED = 3;
  TRANS2QUIK_ALREADY_CONNECTED_TO_QUIK = 4;
  TRANS2QUIK_WRONG_SYNTAX	=	5;
  TRANS2QUIK_QUIK_NOT_CONNECTED	=	6;
  TRANS2QUIK_DLL_NOT_CONNECTED = 7;
  TRANS2QUIK_QUIK_CONNECTED	=	8;
  TRANS2QUIK_QUIK_DISCONNECTED = 9;
  TRANS2QUIK_DLL_CONNECTED = 10;
  TRANS2QUIK_DLL_DISCONNECTED	=	11;
  TRANS2QUIK_MEMORY_ALLOCATION_ERROR = 12;
  TRANS2QUIK_WRONG_CONNECTION_HANDLE = 13;
  TRANS2QUIK_WRONG_INPUT_PARAMS	=	14;

type
  TRANS2QUIK_CONNECTION_STATUS_CALLBACK = procedure (nConnectionEvent: Longint; nExtendedErrorCode: Longint; lpcstrInfoMessage: LPCSTR); stdcall;
  TRANS2QUIK_TRANSACTION_REPLY_CALLBACK = procedure (nTransactionResult: Longint; nTransactionExtendedErrorCode: Longint; nTransactionReplyCode: Longint; dwTransId: DWORD; dOrderNum: double; lpcstrTransactionReplyMessage: LPCSTR); stdcall;

  function TRANS2QUIK_CONNECT (lpstConnectionParamsString: LPSTR; var pnExtendedErrorCode: longint; lpstrErrorMessage: LPSTR; dwErrorMessageSize: DWORD): longint; stdcall;
  function TRANS2QUIK_DISCONNECT (var pnExtendedErrorCode: longint; lpstrErrorMessage: LPSTR; dwErrorMessageSize: DWORD): longint; stdcall;
  function TRANS2QUIK_IS_DLL_CONNECTED (var pnExtendedErrorCode: longint; lpstrErrorMessage: LPSTR; dwErrorMessageSize: DWORD): longint; stdcall;
  function TRANS2QUIK_IS_QUIK_CONNECTED (var pnExtendedErrorCode: longint; lpstrErrorMessage: LPSTR; dwErrorMessageSize: DWORD): longint; stdcall;
  function TRANS2QUIK_SET_CONNECTION_STATUS_CALLBACK ( pfConnectionStatusCallback: TRANS2QUIK_CONNECTION_STATUS_CALLBACK; var pnExtendedErrorCode: longint; lpstrErrorMessage: LPSTR; dwErrorMessageSize: DWORD): longint; stdcall;
  function TRANS2QUIK_SEND_SYNC_TRANSACTION (lpstTransactionString: LPSTR; var pnReplyCode: longint; var pdwTransId: DWORD; var pdOrderNum: double; lpstrResultMessage: LPSTR; dwResultMessageSize: DWORD; var pnExtendedErrorCode: longint; lpstErrorMessage: LPSTR; dwErrorMessageSize: DWORD): longint; stdcall;
  function TRANS2QUIK_SEND_ASYNC_TRANSACTION (lpstTransactionString: LPSTR; var pnExtendedErrorCode: longint; lpstErrorMessage: LPSTR; dwErrorMessageSize: DWORD): longint; stdcall;
  function TRANS2QUIK_SET_TRANSACTIONS_REPLY_CALLBACK ( pfTransactionReplyCallback: TRANS2QUIK_TRANSACTION_REPLY_CALLBACK; var pnExtendedErrorCode: longint; lpstrErrorMessage: LPSTR; dwErrorMessageSize: DWORD):  longint; stdcall;

implementation
  function TRANS2QUIK_CONNECT (lpstConnectionParamsString: LPSTR; var pnExtendedErrorCode: longint; lpstrErrorMessage: LPSTR; dwErrorMessageSize: DWORD): longint; stdcall; external 'TRANS2QUIK.DLL'  name '_TRANS2QUIK_CONNECT@16';
  function TRANS2QUIK_DISCONNECT (var pnExtendedErrorCode: longint; lpstrErrorMessage: LPSTR; dwErrorMessageSize: DWORD): longint; stdcall; external 'TRANS2QUIK.DLL' name '_TRANS2QUIK_DISCONNECT@12';
  function TRANS2QUIK_IS_DLL_CONNECTED (var pnExtendedErrorCode: longint; lpstrErrorMessage: LPSTR; dwErrorMessageSize: DWORD): longint; stdcall; external 'TRANS2QUIK.DLL' name '_TRANS2QUIK_IS_DLL_CONNECTED@12';
  function TRANS2QUIK_IS_QUIK_CONNECTED (var pnExtendedErrorCode: longint; lpstrErrorMessage: LPSTR; dwErrorMessageSize: DWORD): longint; stdcall; external 'TRANS2QUIK.DLL' name '_TRANS2QUIK_IS_QUIK_CONNECTED@12';
  function TRANS2QUIK_SET_CONNECTION_STATUS_CALLBACK ( pfConnectionStatusCallback: TRANS2QUIK_CONNECTION_STATUS_CALLBACK; var pnExtendedErrorCode: longint; lpstrErrorMessage: LPSTR; dwErrorMessageSize: DWORD): longint; stdcall; external 'TRANS2QUIK.DLL' name '_TRANS2QUIK_SET_CONNECTION_STATUS_CALLBACK@16';
  function TRANS2QUIK_SEND_SYNC_TRANSACTION (lpstTransactionString: LPSTR; var pnReplyCode: longint; var pdwTransId: DWORD; var pdOrderNum: double; lpstrResultMessage: LPSTR; dwResultMessageSize: DWORD; var pnExtendedErrorCode: longint; lpstErrorMessage: LPSTR; dwErrorMessageSize: DWORD): longint; stdcall; external 'TRANS2QUIK.DLL' name '_TRANS2QUIK_SEND_SYNC_TRANSACTION@36';
  function TRANS2QUIK_SEND_ASYNC_TRANSACTION (lpstTransactionString: LPSTR; var pnExtendedErrorCode: longint; lpstErrorMessage: LPSTR; dwErrorMessageSize: DWORD): longint; stdcall; external 'TRANS2QUIK.DLL' name '_TRANS2QUIK_SEND_ASYNC_TRANSACTION@16';
  function TRANS2QUIK_SET_TRANSACTIONS_REPLY_CALLBACK ( pfTransactionReplyCallback: TRANS2QUIK_TRANSACTION_REPLY_CALLBACK; var pnExtendedErrorCode: longint; lpstrErrorMessage: LPSTR; dwErrorMessageSize: DWORD):  longint; stdcall; external 'TRANS2QUIK.DLL' name '_TRANS2QUIK_SET_TRANSACTIONS_REPLY_CALLBACK@16';

end.
