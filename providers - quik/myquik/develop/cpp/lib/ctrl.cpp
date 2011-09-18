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
 *  lib/ctrl.cpp
 */

#include "stdafx.h"
#include "ctrl.h"
#include <windows.h>

namespace my {
	namespace ctrl { 
		
		struct MsgBoxCtrl {
			HHOOK hhk;
			HWND hParent;
			HWND hAfter;
		} msgbox;
		
		LRESULT CALLBACK CBTProc(INT nCode, WPARAM wParam, LPARAM lParam)
		{
			HWND  hParentWnd, hChildWnd;    // msgbox is "child"
			RECT  rParent, rChild, rDesktop;
			POINT pCenter, pStart;
			INT   nWidth, nHeight;

			// notification that a window is about to be activated
			// window handle is wParam
			if (nCode == HCBT_ACTIVATE)
			{
				// set window handles
#if 0
				hParentWnd = ::GetForegroundWindow();
#else
				hParentWnd = msgbox.hParent;
#endif
				hChildWnd  = (HWND)wParam;

				if ((hParentWnd != NULL) && (hChildWnd != NULL) &&
					(::GetWindowRect(::GetDesktopWindow(), &rDesktop) != 0) &&
					(::GetWindowRect(hParentWnd, &rParent) != 0) &&
					(::GetWindowRect(hChildWnd, &rChild) != 0))				
				{
					// calculate message box dimensions
					nWidth  = (rChild.right - rChild.left);
					nHeight = (rChild.bottom - rChild.top);

					// calculate parent window center point
					pCenter.x = rParent.left + ((rParent.right - rParent.left)/2);
					pCenter.y = rParent.top + ((rParent.bottom - rParent.top)/2);

					// calculate message box starting point
					pStart.x = (pCenter.x - (nWidth/2));
					pStart.y = (pCenter.y - (nHeight/2));

					// adjust if message box is off desktop

					const int cxFrame = ::GetSystemMetrics(SM_CXFRAME);
					const int cyFrame = ::GetSystemMetrics(SM_CYFRAME);

					if (pStart.x - cxFrame < 0) 
					   pStart.x = cxFrame;
					if (pStart.y - cyFrame < 0) 
					   pStart.y = cyFrame;
					if (pStart.x + nWidth + cxFrame > rDesktop.right)
					  pStart.x = rDesktop.right - nWidth - cxFrame;
					if (pStart.y + nHeight + cyFrame > rDesktop.bottom)
					  pStart.y = rDesktop.bottom - nHeight - cyFrame;

					// move message box
					::MoveWindow(hChildWnd, pStart.x, pStart.y, nWidth, nHeight, FALSE);
				}

				// Всегда поверх окна:
				::SetWindowPos(hChildWnd, ::GetForegroundWindow(), 
					0, 0, 0, 0, SWP_NOMOVE|SWP_NOSIZE|SWP_NOACTIVATE|SWP_NOREDRAW);

				// exit CBT hook
				::UnhookWindowsHookEx(msgbox.hhk);
			}
			else // otherwise, continue with any possible chained hooks
				::CallNextHookEx(msgbox.hhk, nCode, wParam, lParam);
			return 0;
		}

		int MessageBox(HWND hWnd, LPCTSTR lpText, LPCTSTR lpCaption, UINT uType)
		{			
			msgbox.hParent = hWnd;
			msgbox.hhk = ::SetWindowsHookEx(WH_CBT, &CBTProc, 0, GetCurrentThreadId());
			return ::MessageBox(hWnd, lpText, lpCaption, uType);
		}

	} // namespace ctrl
} // namespace my
