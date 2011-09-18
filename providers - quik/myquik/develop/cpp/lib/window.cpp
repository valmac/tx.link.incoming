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
 *  lib/window.h
 */

#include "stdafx.h"
#include "window.h"

namespace my {
	namespace wnd {

		void SetPos(HWND hWnd, int x, int y, int flags)
		{
			int defaultFlags = SWP_NOZORDER|SWP_NOSIZE|SWP_NOACTIVATE;
			::SetWindowPos(hWnd, NULL, x, y, 0, 0, defaultFlags|flags);
		}

		void SetPos(HWND hParent, HWND hWnd, int x, int y, int flags)
		{
			int defaultFlags = SWP_NOZORDER|SWP_NOSIZE|SWP_NOACTIVATE;
			if (flags == 0)
				flags = defaultFlags;
			WINDOWINFO wi;
			wi.cbSize = sizeof(wi);
			::GetWindowInfo(hWnd, &wi);
			CPoint point(wi.rcWindow.left, wi.rcWindow.top);
			::ScreenToClient(hParent, &point);
			if (point.x != x || point.y != y || defaultFlags != flags)
				::SetWindowPos(hWnd, NULL, x, y, 0, 0, defaultFlags|flags);
		}

		void Flash(HWND hWnd, UINT count, DWORD timeout)
		{
			FLASHWINFO flashInfo;
			flashInfo.hwnd = hWnd;
			flashInfo.cbSize = sizeof(flashInfo);
			flashInfo.uCount = count;
			flashInfo.dwTimeout = timeout;
			flashInfo.dwFlags = FLASHW_ALL;
			::FlashWindowEx(&flashInfo);
		}

		int SetOpacity(HWND hWnd, int alpha)
		{
			if (hWnd != NULL)
			{
				DWORD exStyles = ::GetWindowLong(hWnd, GWL_EXSTYLE);
				if (!(exStyles & WS_EX_LAYERED))
					::SetWindowLong(hWnd, GWL_EXSTYLE,  exStyles|WS_EX_LAYERED);		
				::SetLayeredWindowAttributes(hWnd, 0, (BYTE)alpha, LWA_ALPHA);
				return S_OK;
			} // if (hWnd != NULL)
			return -1;
		}

		int SetOpacityPerc(HWND hWnd, int percents)
		{
			int alpha = 255 * percents / 100;
			return SetOpacity(hWnd, alpha);
		}

		int SetDialogTitleBar(HWND hWnd, BOOL set)
		{
			DWORD style = ::GetWindowLong(hWnd, GWL_STYLE);
			DWORD newStyle;
			DWORD mask = (WS_CAPTION);
			if (set)
				newStyle = style | mask;
			else
				newStyle = (style & ~mask);
			newStyle |= (style & WS_THICKFRAME) | WS_BORDER;
			if (newStyle != style)
				::SetWindowLong(hWnd, GWL_STYLE, newStyle);

			// Перерисовываем с учётом новых стилей:
			::SetWindowPos(hWnd, NULL, 0,0,0,0, SWP_NOSIZE|SWP_NOMOVE|SWP_NOZORDER|SWP_NOACTIVATE|SWP_FRAMECHANGED); 

			return 0;
		}

		int SetAppWindow(HWND hWnd)
		{
			DWORD exStyles = ::GetWindowLong(hWnd, GWL_EXSTYLE);
			if (!(exStyles & WS_EX_APPWINDOW))
				::SetWindowLong(hWnd, GWL_EXSTYLE,  exStyles|WS_EX_APPWINDOW);
			return 0;
		}

		int SetNotAppWindow(HWND hWnd)
		{
			DWORD exStyles = ::GetWindowLong(hWnd, GWL_EXSTYLE);
			if (exStyles & WS_EX_APPWINDOW)
				::SetWindowLong(hWnd, GWL_EXSTYLE,  exStyles & (~WS_EX_APPWINDOW));
			return 0;
		}


	} // namespace wnd
} // namespace my
