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
 *  reg.cpp
 */

#include "stdafx.h"
#include "reg.h"
#include <std.h>

LONG QueryStringValue(CRegKey & key, CString & str, LPCTSTR pszValueName, LPCTSTR pszDefault)
{
	LONG result;
	TSTRING_STD2(buf, size);
	ULONG n = size - 1;
	result = key ? key.QueryStringValue(pszValueName, buf, &n) : -1;
	if (ERROR_SUCCESS != result || n == 0)
		str.Format(TEXT("%s"), pszDefault);
	else
		str.Format(TEXT("%s"), buf);
	return result;
}

LONG QueryStringValue(CRegKey & key, CString & str, LPCTSTR path, LPCTSTR pszValueName, LPCTSTR pszDefault)
{
	LONG result;
	CRegKey subKey(key);
	result = key ? subKey.Open(key, path) : -1;
	if (result == ERROR_SUCCESS)
		result = QueryStringValue(subKey, str, pszValueName, pszDefault);
	else
		str = pszDefault;
	return result;
}

LONG SetStringValue(CRegKey & key, LPCTSTR path, LPCTSTR pszValueName, LPCTSTR pszStrToSet)
{
	if (! key)
		return -1;
	LONG result;
	CRegKey subKey(key);
	result = key ? subKey.Create(key, path) : -1;
	if (result == ERROR_SUCCESS)
		result = subKey.SetStringValue(pszValueName, pszStrToSet);
	return result;
}
