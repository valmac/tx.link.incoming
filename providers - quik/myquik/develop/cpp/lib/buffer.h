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
 *  lib/buffer.h
 */

#pragma once

namespace my {
	 
	namespace lib {

		template<typename Type, typename Type2>
		class Buffer // αστεπ
		{
		public:
			Buffer()
			{
				size = 0; data = NULL; it = 0;
			}
			Buffer(Type2 size)
			{
				this->size = 0;
				Allocate(size);
			}

			virtual ~Buffer()
			{
				Release();
			}

			Type2 GetSize()const { return this->size;}
			Type2 GetLength()const { return this->it;}
			Type2 GetPos()const { return this->it;}
			Type2 GetFreeSize()const { return (size - it); }
			Type * GetData() { return data;}
			Type * GetData(Type2 iterator) { return &data[iterator];}
			Type * GetDataIt() { return &data[this->it];}
			BOOL IsValid()const { return data != NULL && size > 0;}
			//BOOL IsWritable()const { it < size;}

			HRESULT Allocate(Type2 size);
			int Release();

			Type2 Write(const Type * src, Type2 count);
			Type2 Read(Type * dst, Type2 count);
			void Empty();

			void Reset(Type2 cnt) { it = cnt;}
			void IncPos(Type2 n) { it += n;}
			void DecPos(Type2 n) { it -= n;}

		protected:
			Type * data;
			Type2 size;
			Type2 it;
		};

		template<typename Type, typename Type2>
		HRESULT Buffer<Type, Type2>::Allocate(Type2 size)
		{
			if (this->data != NULL)
				return 0;
			this->data = new Type[size];
			if (this->data != NULL)
			{
				this->size = size;
				it = 0;
				return S_OK;
			}
			else
				return E_FAIL;
		}

		template<typename Type, typename Type2>
		int Buffer<Type, Type2>::Release()
		{
			if (this->data == NULL)
				return 0;
			else
			{
				delete [] this->data;
				this->data = NULL;
				this->size = 0;
				return 0;
			}
		}

		template<typename Type, typename Type2>
		Type2 Buffer<Type, Type2>::Write(const Type * src, Type2 count)
		{
			if (!IsValid())
				return -1;
			Type2 cnt;
			if (this->it + count < size)
				cnt = count;
			else
				cnt = size - this->it;
			for (Type2 i = 0; i < cnt; i++)
			{
				data[this->it++] = *src++;
			}
			return cnt;
		}

		template<typename Type, typename Type2>
		Type2 Buffer<Type, Type2>::Read(Type * dst, Type2 count)
		{
			if (!IsValid())
				return -1;
			Type2 cnt;
			if (count <= this->it)
				cnt = count;
			else
				cnt = this->it;
			for (Type2 i = 0; i < cnt; i++)
			{
				*dst++ = data[i];
			}
			return cnt;
		}

		template<typename Type, typename Type2>
		void Buffer<Type, Type2>::Empty()
		{
			if (IsValid())
			{
				it = 0;
				data[0] = 0;
			}
		}


	}// namespace lib

	typedef lib::Buffer<BYTE, int> Buffer;

}// namespace my
