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

#pragma once

namespace my {
	namespace lib {

		enum WindowPositionFlags {
			FWP_NONE = 0x0,
			FWP_X = 0x01,
			FWP_Y = 0x02,
			FWP_CX = 0x04,
			FWP_CY = 0x08,
			FWP_STYLE = 0x10,			
			FWP_NORMAL = 0x20,
			FWP_DIALOG = 0x40,
			FWP_GENERIC = (FWP_X|FWP_Y|FWP_CX|FWP_CY|FWP_STYLE),
			FWP_XY = (FWP_X|FWP_Y),
			FWP_CXCY = (FWP_CX|FWP_CY),
			FWP_FORCE = 0x1000,
		};

	} // namespace lib

	namespace wnd {

		inline BOOL IsValid(HWND hWnd)
		{
			return (hWnd != NULL && ::IsWindow(hWnd));
		}

		inline BOOL IsVisible(HWND hWnd)
		{
			return (IsValid(hWnd) && ::IsWindowVisible(hWnd));
		}

		inline void Show(HWND hWnd, int show, int flags = 0x0)
		{
			BOOL visible = ::IsWindowVisible(hWnd);
			if ((show && !visible) || (!show && visible) || flags)
				::ShowWindow(hWnd, show);
		}

		inline void Enable(HWND hWnd, BOOL enable, int flags = 0x0)
		{
			BOOL enabled = ::IsWindowEnabled(hWnd);
			if ((enable && !enabled) || (!enable && enabled) || flags)
				::EnableWindow(hWnd, enable);
		}

		void SetPos(HWND hWnd, int x, int y, int flags = 0x0);
		void SetPos(HWND hParent, HWND hWnd, int x, int y, int flags = 0x0);

		void Flash(HWND hWnd, UINT count, DWORD timeout);

		int SetOpacity(HWND hWnd, int alpha);
		int SetOpacityPerc(HWND hWnd, int percents);

		int SetDialogTitleBar(HWND hWnd, BOOL set = TRUE);

		int SetAppWindow(HWND hWnd);
		int SetNotAppWindow(HWND hWnd);

	} // namespace wnd
} // namespace my
