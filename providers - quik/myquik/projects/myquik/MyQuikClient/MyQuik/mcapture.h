/*
 *  Copyright (C) 2010 Alexander Chernykh 
 *
 *  This file is part of MyQuik: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  MyQuik is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 *  mcapture.h
 */

#pragma once

namespace my {
	class MouseCapturer 
	{
	public:
		static void Capture(HWND hWnd, UINT nFlags, CPoint poin);
		static void Release();

		static void OnMouseMove(UINT nFlags, CPoint point);

	protected:
		static void MoveWindow(HWND hWnd, CPoint point);

		static BOOL m_capture;
		static CRect m_rect;
		static CPoint m_pos;
		static HWND m_hWnd;
	}; // class MouseCapturer
}; // namespace my
