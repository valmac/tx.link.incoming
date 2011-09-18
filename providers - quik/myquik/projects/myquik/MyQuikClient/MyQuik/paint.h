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
 *  paint.h
 */

#pragma once

namespace my {

class Painter {
	public:
		Painter();
		Painter(CWindow wnd);

		int Create(CWindow wnd);

		void Invalidate(int flags, BOOL erase);
		void Invalidate(HWND hWnd, int flags, BOOL erase);

		void SetPaint(int flags) { m_flags = flags; }
		int GetPaint() const { return m_flags; }

		static int DrawColorPicker(CWindow wnd, COLORREF color, int blank = 0);

	protected:
		int m_flags;
		CWindow m_wnd;
	}; // Painter

} // namespace my

