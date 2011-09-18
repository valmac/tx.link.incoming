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
 *  reg.h
 */

#pragma once

LONG QueryStringValue(CRegKey & key, CString & str, LPCTSTR path, LPCTSTR pszValueName, LPCTSTR pszDefault);
LONG QueryStringValue(CRegKey & key, CString & str, LPCTSTR pszValueName, LPCTSTR pszDefault);
LONG SetStringValue(CRegKey & key, LPCTSTR path, LPCTSTR pszValueName, LPCTSTR pszStrToSet);

template <typename Type>
LONG QueryIntValue(CRegKey & key, Type & val, LPCTSTR pszValueName, int defaultValue)
{
	LONG result;
	DWORD dwVal;
	result = key ? key.QueryDWORDValue(pszValueName, dwVal) : -1;
	if (result == ERROR_SUCCESS)
		val = (int)dwVal;
	else
		val = defaultValue;
	return result;
}

template <typename Type>
LONG QueryIntValue(CRegKey & key, Type & val, LPCTSTR path, LPCTSTR pszValueName, int defaultValue)
{
	LONG result;
	CRegKey subKey;
	result = key ? subKey.Open(key, path) : -1;
	if (result == ERROR_SUCCESS)
		result = QueryIntValue(subKey, val, pszValueName, defaultValue);
	else
		val = defaultValue;
	return result;
}

template <typename Type>
LONG SetIntValue(CRegKey & key, Type value, LPCTSTR pszValueName)
{
	LONG result;
	result = key ? key.SetDWORDValue(pszValueName, value) : -1;
	return result;
}

template <typename Type>
LONG SetIntValue(CRegKey & key, Type value, LPCTSTR path, LPCTSTR pszValueName)
{
	LONG result;
	CRegKey subKey;
	result = key ? subKey.Create(key, path) : -1;
	if (result == ERROR_SUCCESS)
		result = subKey.SetDWORDValue(pszValueName, value);
	return result;
}
