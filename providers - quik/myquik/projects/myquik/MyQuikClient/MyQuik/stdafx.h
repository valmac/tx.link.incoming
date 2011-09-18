// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

#pragma once

// Change these values to use different versions
#include "targetver.h"
#define _RICHEDIT_VER	0x0200

#include <atlbase.h>
#include <atlapp.h>

#include <atlwin.h>
#include <atlframe.h>
#include <atlctrls.h>
#include <atldlgs.h>
#include <atlcrack.h>
#include <atlddx.h>
#include <atlmisc.h>

#if defined _M_IX86
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='x86' publicKeyToken='6595b64144ccf1df' language='*'\"")
#elif defined _M_IA64
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='ia64' publicKeyToken='6595b64144ccf1df' language='*'\"")
#elif defined _M_X64
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='amd64' publicKeyToken='6595b64144ccf1df' language='*'\"")
#else
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")
#endif

#if 1
#include <uxtheme.h>
#endif

#ifdef _DEBUG
#define DEBUG_NEW new(_NORMAL_BLOCK, __FILE__, __LINE__)
#endif

#define ASSERT ATLASSERT
#define TRACE ATLTRACE


#define DISABLE_TRANS2QUIK 0
#define USE_TRANSPARENCE 1
#define AUTOSELECT_MODERN_THEME 1
#define SHOW_HISTORY_ON_START 0
#define LOAD_TRADE_IN_SEPARATE_THREAD 1
#define USE_STATIC_MENU_MAIN 1
#define USE_STATIC_MENU_USER_VALUES 1
