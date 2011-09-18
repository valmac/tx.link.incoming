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
 *  lib/file.cpp
 */

#include "stdafx.h"
#include "file.h"
#include "std.h"

namespace my {
	namespace lib {

		//
		// Класс для работы с файлами
		//
		File::File(LPCTSTR name)
		{
			this->Reset();
			if (name != NULL)
			{
				SAFE_TSTRCPY(m_info.name, name);
			}
		}

		File::File(LPCTSTR name, DWORD dwDesiredAccess, DWORD dwCreationDisposition)
		{
			BOOL result = Create(name, dwDesiredAccess, dwCreationDisposition);
			ASSERT(result);
		}

		File::~File()
		{
			Close();
		}

		void File::Reset()
		{
			memset(&m_info, 0, sizeof(m_info));
			m_hFile = INVALID_HANDLE_VALUE;
			m_info.eof = FALSE; // TRUE
		}

		int File::Create(LPCTSTR name, DWORD dwDesiredAccess, DWORD dwCreationDisposition, DWORD dwShareMode)
		{
			return Create(name, dwDesiredAccess,
								dwShareMode, (LPSECURITY_ATTRIBUTES) NULL, 
								dwCreationDisposition, FILE_ATTRIBUTE_NORMAL, (HANDLE) NULL);
		}

		//создание файла для чтения и/или записи
		int File::Create(LPCTSTR name, 
								DWORD dwDesiredAccess,
								DWORD dwShareMode,
								LPSECURITY_ATTRIBUTES lpSecurityAttributes,
								DWORD dwCreationDisposition,
								DWORD dwFlagsAndAttributes,
								HANDLE hTemplateFile)
		{
			m_hFile = CreateFile(name, dwDesiredAccess, 
				dwShareMode, lpSecurityAttributes, 
				dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
			if (m_hFile != INVALID_HANDLE_VALUE && NULL != lstrcpy (m_info.name, name))
				return S_OK;
			else
			{
				::SetLastError(E_HANDLE);
				return -1;
			}
		}

		//открыть файл
		int File::Open(LPCTSTR name, DWORD access, DWORD dwShareMode)
		{
			int result = Create (name, GENERIC_READ | access, 
								dwShareMode, (LPSECURITY_ATTRIBUTES) NULL, 
								OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, (HANDLE) NULL);
			if (SUCCEEDED(result) && QueryFileSize() >= 0)
				return S_OK;
			else
				return -1;
		}

		//создать новый файл
		int File::New(LPCTSTR name, DWORD access, DWORD dwShareMode)
		{
			return Create(name, GENERIC_WRITE | access, 
				dwShareMode, (LPSECURITY_ATTRIBUTES) NULL, 
				CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, (HANDLE) NULL);
		}

		// Закрыть файл
		int File::Close()
		{
			if (this->IsValid())
			{
				this->Flush();
				::CloseHandle(m_hFile);
				this->Reset();
			}
			return S_OK;
		}

		// Запись данных в файл
		int File::Write(LPCVOID buf, size_t count)
		{
			if (m_hFile != INVALID_HANDLE_VALUE)
			{
				DWORD numberOfBytesWritten;
				if (::WriteFile(m_hFile, buf, (DWORD)count, &numberOfBytesWritten, NULL) == TRUE)
					return numberOfBytesWritten;
			}
			else
				::SetLastError(E_HANDLE);
			return -1;
		}

		// Запись данных в файл
		int File::WriteEx(LPCVOID buf, size_t count, size_t * pNrBytesWritten)
		{
			int status = -1;
			DWORD nrBytesWritten = 0;
			if (m_hFile != INVALID_HANDLE_VALUE)
			{
				if (::WriteFile(m_hFile, buf, (DWORD)count, &nrBytesWritten, NULL) == TRUE)
					status = ERROR_SUCCESS;
			}
			else
				::SetLastError(E_HANDLE);
			if (pNrBytesWritten != NULL)
				*pNrBytesWritten = nrBytesWritten;
			return status;
		}

		// Запись данных в файл
		int File::WriteLn(LPCVOID buf, DWORD count)
		{
			DWORD numberOfBytesWritten = Write(buf, count);
			if (numberOfBytesWritten != -1)
			{
				DWORD temp;
				if (::WriteFile (m_hFile, TEXT("\r\n"), 2 * sizeof(_TCHAR), &temp, NULL) == FALSE)
					return -1;
				else
					return numberOfBytesWritten;
			}
			return -1;
		}

		//чтение данных из файла
		int File::Read(LPVOID buf, size_t count)
		{
			if (m_hFile != INVALID_HANDLE_VALUE)
			{
				DWORD numberOfBytesRead;
				if (::ReadFile(m_hFile, buf, (DWORD)count, &numberOfBytesRead, NULL) == TRUE)
				{
					if (numberOfBytesRead == 0)
						m_info.eof = TRUE;
					return numberOfBytesRead;
				}
			}
			else
				::SetLastError(E_HANDLE);
			return -1;
		}

		int File::ReadEx(LPVOID buf, size_t count, size_t * pNrBytesRead)
		{
			int status = -1;
			DWORD nrBytesRead = 0;
			if (m_hFile != INVALID_HANDLE_VALUE)
			{
				if (::ReadFile(m_hFile, buf, (DWORD)count, &nrBytesRead, NULL) == TRUE)
				{
					status = ERROR_SUCCESS;
					if (nrBytesRead == 0)
						m_info.eof = TRUE;
				}
			}
			else
				::SetLastError(E_HANDLE);
			if (pNrBytesRead != NULL)
				*pNrBytesRead = nrBytesRead;
			return status;
		}

		//чтение из файла, пока не будет достигнут определенный символ (н.р., "\r\n")
		//!!! длина буфера не должна превышать максимального значения, указанного в данной функции (max)
		int File::ReadLine(LPVOID bufLine, int max, LPCSTR endStr)	//const char *	//LPCTSTR
		{
			BYTE * pBufLine = static_cast<BYTE*>(bufLine);
		//	UCHAR strLine[max] = {0};
			BYTE _buf[10];
		//	int summaryCount = 0;
			for(int i = 0; i < max; i++)
			{
				int count = Read(_buf, 1);
				if(count == -1)
					return -1;	//ошибка при чтении файла
				else
					if(count == 0)
						return i;	//ничего не прочитали - скорее всего, конец файла
					else 
						if(count == 1)
						{//прочитали следующий символ
							const int endStrLen = static_cast<int>(strlen(endStr));
							for(int j = 0; j < endStrLen; j++)
							{
								if(_buf[0] == (BYTE)(endStr[j]))
								{
		#ifdef _UNICODE
									Read(&_buf[5], 1);	//?
		#endif	// _UNICODE
									return i + 1;
								}
							}
							pBufLine[i] = _buf[0];
						}
			}
			return -1;
		}

		BOOL File::GetTime(SYSTEMTIME & time, BOOL local) const
		{
			BOOL success;
			SYSTEMTIME stUTC;
			FILETIME ftWrite;
			success = ::GetFileTime(this->GetHandle(), NULL, NULL, &ftWrite);
			if (success)
			{
				::FileTimeToSystemTime(&ftWrite, &stUTC);
				if (local)
					::SystemTimeToTzSpecificLocalTime(NULL, &stUTC, &time);
				else
					time = stUTC;
			}
			return success;
		}

		LPCSTR signature = NULL;

		//
		// Класс для работы с файлами с поддержкой буферизации данных
		//
		FileB::FileB(int bufSize) 
		{
			m_buf.Allocate(bufSize);
		}

		FileB::FileB(LPCTSTR name, DWORD dwDesiredAccess, DWORD dwCreationDisposition)
			: File(name, dwDesiredAccess, dwCreationDisposition)
		{
			m_buf.Allocate(8192);
		}

		FileB::~FileB()
		{
			Flush();
			m_buf.Release();
		}

		// Запись данных (с буферизацией)
		int FileB::Write(LPCVOID buf, DWORD count)
		{
			int n = m_buf.Write((const BYTE*)buf, count);
			if (n < static_cast<int>(count))
			{
				File::Write(m_buf.GetData(), m_buf.GetSize());
				m_buf.Empty();
				m_buf.Write((BYTE*)buf + n, count - n);
			}
			else
			{

			}

			return count;
		}

		int FileB::WriteEx(LPCVOID buf, size_t count, size_t * pNrBytesWritten)
		{
			int status = NOERROR;
			size_t nrBytesWritten = 0;
			size_t n = m_buf.Write((const BYTE*)buf, static_cast<int>(count));
			if (n < count)
			{
				status = File::WriteEx(m_buf.GetData(), m_buf.GetSize(), &nrBytesWritten);
				nrBytesWritten += n;
				m_buf.Empty();
				m_buf.Write((BYTE*)buf + n, static_cast<int>(count - n));
			}
			if (pNrBytesWritten != NULL)
				*pNrBytesWritten = nrBytesWritten;
			return status;
		}

		// Сброс данных на диск
		int FileB::Flush()
		{
			int n = 0;
			if (m_buf.GetSize() > 0)
				n = File::Write(m_buf.GetData(), m_buf.GetLength());
			else
				n = 0;
			m_buf.Empty();
			return n;
		}

	}	//namespace lib
}	// namespace my
