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
 *  shortcuts.cpp
 */

#include "stdafx.h"
#include "shortcuts.h"
#include <std.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

#define SHORTCUTS_WITH_EXTRASPACE 1

static LPCTSTR s_strNames[] = {
	TEXT("À Ã"),
	TEXT("œ Ã"),
	TEXT("Shift"),
	TEXT("Ctrl"),
	TEXT("C Ã"),
	TEXT("X Ã1"),
	TEXT("X Ã2"),
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	TEXT("Alt"),
};

int ShortcutToText(DWORD shortcut, LPTSTR text, int size)
{
	int n = 0;
	LPCTSTR name;
	int len;
	LPCTSTR strExt;
	int lenExt;
#ifdef SHORTCUTS_WITH_EXTRASPACE
	strExt = TEXT(" + "); lenExt = 3;
#else
	strExt = TEXT("+"); lenExt = 1;
#endif
	int flags = SHORTCUT2FLAGS(shortcut);
	int mask = 0x1;
	for (int i = 0; i < 16; i++)
	{
		if (flags & mask)
		{
			name = s_strNames[i];
			if (name)
			{
				len = lstrlen(name);
				memcpy(text + n, name, len*sizeof(TCHAR)); n += len;
				memcpy(text + n, strExt, lenExt*sizeof(TCHAR)); n += lenExt;
			}
		}
		mask <<= 1;
	} // for (i)
	TCHAR keys[2];
	SHORTCUT2KEYS(shortcut, keys);
	int nrKeys = 0;
	for (int i = 0 ; i < SIZEOF_ARRAY(keys); i++)
	{
		TCHAR key = keys[i];
		if (key)
		{
			++nrKeys;
			len = 1;
			TCHAR msg[32];	
			name = NULL;
			switch (key)
			{
			case VK_LEFT: name = TEXT("¬ÎÂ‚Ó"); 
				break;
			case VK_RIGHT: name = TEXT("¬Ô‡‚Ó"); 
				break;
			case VK_UP: name = TEXT("¬‚Âı"); 
				break;
			case VK_DOWN: name = TEXT("¬ÌËÁ"); 
				break;
			case VK_SPACE: name = TEXT("œÓ·ÂÎ"); 
				break;
			case VK_BACK: name = TEXT("BackSpace"); 
				break;
			case VK_HOME: name = TEXT("Home");
				break;
			case VK_END: name = TEXT("End");
				break;
			case VK_PRIOR: name = TEXT("PgUp");
				break;
			case VK_NEXT: name = TEXT("PgDn");
				break;
			case VK_INSERT: name = TEXT("Ins");
				break;
			case VK_DELETE: name = TEXT("Del");
				break;
			case VK_PAUSE: name = TEXT("Pause");
				break;
			case VK_ADD: name = TEXT("Num+"); 
				break;
			case VK_SUBTRACT: name = TEXT("Num-"); 
				break;
			case VK_MULTIPLY: name = TEXT("Num*"); 
				break;
			case VK_DIVIDE: name = TEXT("Num/"); 
				break;
			case VK_SNAPSHOT: name = TEXT("PrtScr"); 
				break;
			case VK_F1:
			case VK_F2:
			case VK_F3:
			case VK_F4:
			case VK_F5:
			case VK_F6:
			case VK_F7:
			case VK_F8:
			case VK_F9:
			case VK_F10:
			case VK_F11:
			case VK_F12:
				TCHAR str[4];
				str[0] = TEXT('F'); _itow_s(key - VK_F1 + 1, str + 1, SIZEOF_ARRAY(str) - 1, 10);
				name = str;
				break;
			case VK_OEM_PLUS: name = TEXT("=+");
				break;
			case VK_OEM_MINUS: name = TEXT("-_");
				break;
			case VK_OEM_COMMA: name = TEXT(",<");
				break;
			case VK_OEM_PERIOD: name = TEXT(".>");
				break;
			case VK_OEM_1: name = TEXT(";:");
				break;
			case VK_OEM_2: name = TEXT("/?");
				break;
			case VK_OEM_3: name = TEXT("`~"); 
				break;
			case VK_OEM_4: name = TEXT("[{"); 
				break;
			case VK_OEM_5: name = TEXT("\\|"); 
				break;
			case VK_OEM_6: name = TEXT("]}"); 
				break;
			case VK_OEM_7: name = TEXT("'\"");
				break;
			case VK_OEM_102: name = TEXT("<>");
				break;
			default:
				len = 1;
				memcpy(msg, &key, 1*sizeof(TCHAR));
			} // switch (key)
			if (name != NULL)
			{
				len = lstrlen(name);
				memcpy(msg, name, len*sizeof(TCHAR));
			}
			memcpy(text + n, msg, len*sizeof(TCHAR)); n += len;
			memcpy(text + n, strExt, lenExt*sizeof(TCHAR)); n += lenExt;
		}
	} // for (i)

	if (n >= lenExt)
		n -= lenExt;

	text[n] = 0;
	return n;
}

