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
 *  lib/thread.h
 */

#pragma once

namespace my {
	namespace lib {

		struct Thread {		
			HANDLE hThread;
			DWORD id;
			HANDLE hEvent;

			Thread();
		};

		int ClearThread(Thread & thread);
		int CreateThread(Thread & thread, LPTHREAD_START_ROUTINE lpStartAddress, LPVOID lpParameter);
		int CloseThread(Thread & thread);

		BOOL SetEvent(Thread & thread);

	} // namespace lib
} // namespace my
