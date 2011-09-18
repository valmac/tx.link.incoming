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
 *  lib/file.h
 */

#include "types.h"

#pragma once

namespace my {
	namespace lib {

		//
		// Класс для работы с файлами
		//
		class File
		{
		public:
			enum Flags {
				O_RDONLY = 0x00000000,
				O_WRONLY = 0x00000001,
				O_RDWR = 0x00000002,

				O_NOCTTY = 0x00000040,
				O_NONBLOCK = 0x00000080,

				O_CREAT = 0x00000100,
				O_TRUNC = 0x00000200,
				O_EXCL = 0x00000400,
				O_APPEND = 0x00000800,
				O_DSYNC = 0x00001000,
				O_RSYNC = 0x00002000,
				O_SYNC = 0x00004000,
				O_MP_OBJ = 0x00008000,
				O_REFSTOP = 0x00010000,
				O_ACCMODE = (O_RDONLY | O_WRONLY | O_RDWR),
				O_FORCE = 0x80000000
			};

			enum SeekPosition { begin = 0x0, current = 0x1, end = 0x2 };

			struct Size{
				union {
					QWORD full;
					struct {
						DWORD lowPart;
						DWORD hiPart;
					};
				};
			};
			struct Info {
				Info() : eof(FALSE)
				{}
				TCHAR name[MAX_PATH];
				TCHAR shortName[MAX_PATH];
				Size size;
				BOOL eof;
			};

			File(LPCTSTR name = NULL);
			File(LPCTSTR name, DWORD dwDesiredAccess, DWORD dwCreationDisposition);
			virtual ~File();

			int Create(LPCTSTR name, DWORD dwDesiredAccess, 
								DWORD dwCreationDisposition, DWORD dwShareMode = FILE_SHARE_READ);
			int Create(LPCTSTR name, 
								DWORD dwDesiredAccess,
								DWORD dwShareMode,
								LPSECURITY_ATTRIBUTES lpSecurityAttributes,
								DWORD dwCreationDisposition,
								DWORD dwFlagsAndAttributes,
								HANDLE hTemplateFile);

			int Open(LPCTSTR name, DWORD access = GENERIC_READ, DWORD dwShareMode = FILE_SHARE_READ);
			int New(LPCTSTR name, DWORD access = GENERIC_WRITE, DWORD dwShareMode = FILE_SHARE_READ);
			int Close();

			virtual int Flush() { return 0;}

			BOOL IsValid()const {return (m_hFile != INVALID_HANDLE_VALUE && m_hFile != 0);}

			virtual int Write(LPCVOID buf, size_t count);
			virtual int WriteEx(LPCVOID buf, size_t count, size_t * pNrBytesWritten);
			int WriteLn(LPCVOID buf, DWORD count);
		//	int Write(int val);
			template<typename _Type> int Write(_Type val)		//запись в файл целого значения
			{
				UCHAR buf[8];
				const int max = sizeof(_Type);
			//	UCHAR * pVal = reinterpret_cast<UCHAR*>(&val);
			//	for(int i = 0 ; i < max; i++)
			//		buf[i] = *pVal++;
				memcpy(buf, &val, max);	//reinterpret_cast<UCHAR*>(val)
				return Write(buf, max);
			}

			int Read(LPVOID buf, size_t count);
			int ReadEx(LPVOID buf, size_t count, size_t * pNrBytesRead = NULL);
			int ReadLine(LPVOID buf, int max = 512, LPCSTR end = "\n");

			HANDLE GetHandle()const {return m_hFile;}
			LPCTSTR GetName()const {return m_info.name;}
			LPTSTR GetNameBuf() {return m_info.name;}

			QWORD GetFileSize() const {return m_info.size.full;}
			QWORD GetSize() const {return GetFileSize();}

			inline DWORD SetFilePointer(LONG lDistanceToMove, PLONG lpDistanceToMoveHigh, DWORD dwMoveMethod);
			inline DWORD Seek(LONG lDistanceToMove, DWORD dwMoveMethod);

			BOOL IsEof()const { return m_info.eof; }

			BOOL GetTime(SYSTEMTIME & time, BOOL local = TRUE) const;
			BOOL GetLocalTime(SYSTEMTIME & time) const {return this->GetTime(time, TRUE);}

		protected:
			void Reset();
			inline BOOL QueryFileSize();

		protected:
			HANDLE m_hFile;
			Info m_info;
			Size m_pointerPos;
			static LPCSTR signature;
			static int GetSignatureSize() { if(signature == NULL) return 0; else return static_cast<int>(strlen(signature)); }
		};

		inline int File::QueryFileSize()
		{
			DWORD result = ::GetFileSize(m_hFile, &m_info.size.hiPart); 
			if (result == INVALID_FILE_SIZE)
				return -1;
			else
				return m_info.size.lowPart = result;
		}

		inline DWORD File::SetFilePointer(LONG lDistanceToMove, PLONG lpDistanceToMoveHigh, DWORD dwMoveMethod)
		{
			return ::SetFilePointer(m_hFile, lDistanceToMove, lpDistanceToMoveHigh, dwMoveMethod);
		}
		inline DWORD File::Seek(LONG lDistanceToMove, DWORD dwMoveMethod)
		{
			LARGE_INTEGER lint;
			lint.QuadPart = lDistanceToMove;
			return ::SetFilePointer(m_hFile, lint.LowPart, &lint.HighPart, dwMoveMethod);
		}

		//
		// Класс для работы с файлами с поддержкой буферизации данных
		//
		class FileB : public File
		{
		public:
			FileB(int bufSize = 4096*2);
			FileB(LPCTSTR name, DWORD dwDesiredAccess, DWORD dwCreationDisposition);
			virtual ~FileB();

			virtual int Write(LPCVOID buf, DWORD count);
			virtual int WriteEx(LPCVOID buf, size_t count, size_t * pNrBytesWritten);

			virtual int Flush();

		protected:
			Buffer<BYTE, int> m_buf;
		};

	}	//namespace lib
}// namespace my

