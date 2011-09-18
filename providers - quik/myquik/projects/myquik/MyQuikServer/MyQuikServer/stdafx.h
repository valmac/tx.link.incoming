/*
 *  Copyright (C) 2010 Alexander Chernykh 
 *
 *  This file is part of MyQuikServer: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  MyQuikServer is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 *  stdafx.h : include file for standard system include files,
 *  or project specific include files that are used frequently, but
 *  are changed infrequently
 */

#pragma once

#include "targetver.h"

#define WIN32_LEAN_AND_MEAN             // Exclude rarely-used stuff from Windows headers
// Windows Header Files:
#include <windows.h>

// C RunTime Header Files
#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <tchar.h>

#define _WTL_USE_CSTRING

#if 0
#include <atlbase.h>
#include <atlapp.h>
//#include <atlwin.h>
#include <atlmisc.h>
#else
#include <atlstr.h>
#endif

typedef CString String;
//std::string

#ifdef _DEBUG
#define DEBUG_NEW new(_NORMAL_BLOCK, __FILE__, __LINE__)
#endif

#ifdef _DEBUG
#define ASSERT(a) _ASSERT(a)
#define TRACE ATLTRACE2
#else
#define ASSERT(a) 
#define TRACE 
#endif

enum {
	UM_FIRST = WM_USER,
	UM_SERVER_FAULT,
	UM_CLIENT_MESSAGE,
	UM_PRINT_MESSAGE,
	UM_MINIMIZE,
	UM_RESTORE,
};

extern HWND GetMainWindow();

#define PRINT_MESSAGE(msg) SendMessage(GetMainWindow(), UM_PRINT_MESSAGE, (WPARAM)(LPCTSTR)(msg), 0);

#define SEPARATE_THREAD_TO_SEND_DATA 1

#if defined _M_IX86
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='x86' publicKeyToken='6595b64144ccf1df' language='*'\"")
#elif defined _M_IA64
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='ia64' publicKeyToken='6595b64144ccf1df' language='*'\"")
#elif defined _M_X64
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='amd64' publicKeyToken='6595b64144ccf1df' language='*'\"")
#else
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")
#endif
