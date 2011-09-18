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
 *  lib/thread.cpp
 */

#include "stdafx.h"
#include "thread.h"

namespace my {
	namespace lib {

		Thread::Thread()
		{
			hThread = NULL;
			hEvent = NULL;
			id = (DWORD)-1;
		}

		int ClearThread(Thread & thread)
		{
			thread.hThread = NULL;
			thread.hEvent = NULL;
			thread.id = (DWORD)-1;
			return S_OK;
		}

		int CreateThread(Thread & thread, LPTHREAD_START_ROUTINE lpStartAddress, LPVOID lpParameter)
		{
			int status = -1;
			thread.hEvent = ::CreateEvent(NULL, FALSE, FALSE, NULL);
			thread.hThread = ::CreateThread(NULL, 0, lpStartAddress, lpParameter, 0, &thread.id);
			if (thread.hThread != NULL)
			{
				status = S_OK;
			}
			else
				status = ::GetLastError();
			return status;
		}


		int CloseThread(Thread & thread)
		{
			::SetEvent(thread.hEvent);
			::WaitForSingleObject(thread.hThread, INFINITE);
			::CloseHandle(thread.hThread);
			return S_OK;
		}

		BOOL SetEvent(Thread & thread)
		{
			return ::SetEvent(thread.hEvent);
		}

	} // namespace lib
} // namespace my
