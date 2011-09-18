/*
 *  Copyright (C) 2010 Alexander Chernykh 
 *
 *  This file is part of the free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This file is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 *  lib/str.cpp
 */

#include "stdafx.h"
#include "str.h"
#include "std.h"

namespace my {

	namespace str {

		int TrimRight(LPTSTR str, int len, int count, int ch)
		{
			int n = 0;
			if (count == 0)
				return 0;
			if (len <= 0)
				len = lstrlen(str);
			if (count > len)
				count = len;
			n = len - count;
			str[n] = ch;
			if (ch && (n + 1 < len))
				str[n+1] = 0;
			return n;
		}

		int TrimRight(LPTSTR str, int len, TCHAR ch, BOOL excludeChar)
		{
			int n = 0;
			LPCTSTR pStr = StrRChr(str, NULL, ch);
			if (pStr != NULL)
			{
				++pStr;
				n = (int)(pStr - str);	
				if (excludeChar)
					n--;
			}			
			str[n] = 0;
			return n;
		}

		int GroupDigits(LPCTSTR src, int len, LPTSTR dst, size_t size)
		{	
			int n = 0;
			TSTRING_STD(buf);
			LPTSTR pDst = buf;
			LPCTSTR pStr = src + len;
			int pt = 0;
			int ch;
			while (pStr != src)
			{
				ch = pStr[-1];
				*pDst++ = ch;
				pStr--;
				if (ch == TEXT('.'))
				{			
					pt = 1;
					break;
				}		
			}
			if (! pt)
			{
				pStr = src + len;
				pDst = buf;
			}
			int i = 0;	
			while (pStr != src)
			{
				ch = pStr[-1];
				if (i == 3)
				{
					*pDst++ = TEXT(' ');
					i = 1;
				}
				else
					i++;
				*pDst++ = ch;
				pStr--;
			}
	
			n = (int)(pDst - buf);
			for (i = 0; i < n; i++)
				dst[i] = buf[n - i - 1];

			dst[n] = 0;

			return n;
		}

	} // namespace str

} // namespace my
