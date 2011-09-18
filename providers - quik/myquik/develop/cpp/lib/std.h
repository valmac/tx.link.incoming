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
 *  lib/std.h
 */

#pragma once

#ifndef MIN
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif
#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif

#define SIZEOF_ARRAY(a) (sizeof((a)) / sizeof((a)[0]))

#define SAFE_DELETE(p) if ((p)) {delete (p); (p) = NULL;}
#define SAFE_DELETE_ARRAY(p) if ((p)) { delete[] (p); (p) = NULL; }

namespace my {
	namespace std {

		template <typename Type>
		BOOL SafeDeleteStdContainer(Type & objT)
		{
			Type::iterator it = objT.begin();
			for (; it != objT.end(); ++it)
			{
				SAFE_DELETE(*it);
			}
			return TRUE;
		}

	} // namespace std
} // namespace my

#define SAFE_STRCPY(dst, src) strcpy_s((dst), SIZEOF_ARRAY((dst)) - 1, (src))
#define SAFE_TSTRCPY(dst, src) _tcscpy_s((dst), SIZEOF_ARRAY((dst)) - 1, (src))

#ifndef MAX_PATH_EX
#define MAX_PATH_EX 1024
#endif

#define MAKE_DWORD_BYTES_3(b1, b2, b3) (DWORD)(((b1) & 0xff) | (((b2) & 0xff) << 8) | (((b3) & 0xff) << 16))
#define MAKE_DWORD_BYTES_4(b1, b2, b3, b4) (DWORD)(((b1) & 0xff) | (((b2) & 0xff) << 8) | (((b3) & 0xff) << 16) | (((b4) & 0xff) << 24))

#define GET_BYTES_3_DWORD(dw, b1, b2, b3) (b1) = (dw) & 0xff; (b2) = ((dw) >> 8) & 0xff; (b3) = ((dw) >> 16) & 0xff;
#define GET_BYTES_4_DWORD(dw, b1, b2, b3, b4) (b1) = (dw) & 0xff; (b2) = ((dw) >> 8) & 0xff; (b3) = ((dw) >> 16) & 0xff; (b4) = ((dw) >> 24) & 0xff;

#define SET_BIT(mask, bit) my::std::SetBit(mask, bit)
#define CLR_BIT(mask, bit) my::std::ClearBit(mask, bit)
#define CHECK_BIT(mask, bit) my::std::CheckBit(mask, bit)

#define ZERO(val) (val) = 0

#define FILL_ARRAY(ar, val) for (int i = 0; i < SIZEOF_ARRAY((ar)); i++) (ar)[i] = (val);


#ifndef MAX_SIZE_STD
#define MAX_SIZE_STD 1024
#endif
#ifndef MAX_SIZE_SMALL
#define MAX_SIZE_SMALL 256
#endif
#ifndef MAX_SIZE_BIG
#define MAX_SIZE_BIG 4096
#endif

#define TSTRING(str) TCHAR (str)[MAX_SIZE_STD];
#define TSTRING2(str, arraySize) TCHAR (str)[(arraySize)];
#define TSTRING3(str, arraySize, valSize) TCHAR (str)[(arraySize)]; size_t (valSize) = SIZEOF_ARRAY((str)); 
#define TSTRING_STD(str) TSTRING2(str, MAX_SIZE_STD)
#define TSTRING_STD2(str, valSize) TSTRING3(str, MAX_SIZE_STD, valSize)
#define TSTRING_SMALL(str) TSTRING2(str, MAX_SIZE_SMALL)
#define TSTRING_SMALL2(str, valSize) TSTRING3(str, MAX_SIZE_SMALL, valSize)
#define TSTRING_BIG(str) TSTRING2(str, MAX_SIZE_BIG)
#define TSTRING_BIG2(str, valSize) TSTRING3(str, MAX_SIZE_BIG, valSize)

#define TSTRING_PATH(str, valSize) TSTRING3(str, MAX_PATH, valSize)
#define TSTRING_PATH_EX(str, valSize) TSTRING3(str, MAX_PATH_EX, valSize)

#define DEFINE_PATH(path) TCHAR (path)[MAX_PATH]
#define DEFINE_PATH_EX(path) TCHAR (path)[MAX_PATH_EX]

namespace my {
	namespace std {

		template<typename Type, typename Type2>
		Type SetBit(Type & mask, Type2 bit)
		{
			mask |= (1 << bit);
			return mask;
		}

		template<typename Type, typename Type2>
		Type ClearBit(Type & mask, Type2 bit)
		{
			mask &= ~(1 << bit);
			return mask;
		}

		template<typename Type, typename Type2>
		BOOL CheckBit(Type mask, Type2 bit)
		{
			return ((mask &= (1 << bit)) != 0);
		}

		template<typename Type, typename Type2>
		void InverseBit(Type & mask, Type2 bit)
		{
			mask = mask ^ (1 << bit);
		}

		template<typename Type>
		int CalcNrSetBits(Type mask)
		{
			const int count = sizeof(mask) * 8;
			int n = 0;
			Type _mask = 1;
			for (int i = 0; i < count; i++)
			{
				if (mask & _mask)
					++n;
				_mask <<= 1;
			} // for (i)
			return n;
		}

	} // namespace std
} // namespace my
