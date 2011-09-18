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
 *  lib/str.h
 */

#pragma once

namespace my {

	namespace str {

		template <typename Type>
		Type * SkipGaps(Type * src, size_t size)
		{	
			Type * pStr = src;
			int skipped = 0;
			size_t i;
			for (i = 0; i < size; i++)
			{
				if (*pStr == '\t' || *pStr == ' ')
				{
					++skipped;
					++pStr;
					continue;
				}
				else
					break;		
			} // for (i)
			if (! skipped && i == size)
				pStr = NULL;
			return pStr;
		}

		int TrimRight(LPTSTR str, int len, int count, int ch = 0);
		int TrimRight(LPTSTR str, int len, TCHAR ch, BOOL excludeChar = TRUE);

		int GroupDigits(LPCTSTR src, int len, LPTSTR dst, size_t size);

	} // namespace str

} // namespace my
