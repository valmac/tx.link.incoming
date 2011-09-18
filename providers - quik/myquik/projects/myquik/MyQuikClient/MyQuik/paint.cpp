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
 *  paint.cpp
 */

#include "stdafx.h"
#include "paint.h"
#include <color.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

namespace my {

	Painter::Painter()
	{
	}

	Painter::Painter(CWindow wnd)
	{
		Create(wnd);
	}

	int Painter::Create(CWindow wnd)
	{
		m_wnd = wnd;
		return S_OK;
	}

	void Painter::Invalidate(int flags, BOOL erase)
	{
		SetPaint(flags);
		m_wnd.Invalidate(erase);
	}

	void Painter::Invalidate(HWND hWnd, int flags, BOOL erase)
	{
		SetPaint(flags);
		if (flags)
		{
			if (hWnd)
			{
				CRect rect(0, 0, 0, 0);
				::GetClientRect(hWnd, &rect);
				m_wnd.InvalidateRect(&rect, erase);
			}
			else
			{
				m_wnd.Invalidate(erase);
			}
		}
	}

	int Painter::DrawColorPicker(CWindow wnd, COLORREF color, int blank)
	{
		int result;
		CPen pen;
		CBrush brush;
		COLORREF penColor;
		CRect rect;

		BOOL enabled = wnd.IsWindowEnabled();
		
		wnd.GetClientRect(&rect);

		wnd.InvalidateRect(&rect);
		wnd.UpdateWindow();

		CDC dc = wnd.GetDC();

		if (blank)
		{
			color = ::GetSysColor(COLOR_BTNFACE);
			penColor = ::GetSysColor(COLOR_3DDKSHADOW);	
		}
		else
		{
			penColor = ::GetSysColor(enabled ? COLOR_3DDKSHADOW : COLOR_ACTIVEBORDER);
		}
		if (! enabled)
			color = my::lib::SumColors(color, ::GetSysColor(COLOR_BTNFACE));
		brush.CreateSolidBrush(color);
		dc.SelectBrush(brush);
		pen.CreatePen(PS_SOLID, 1, penColor);
		dc.SelectPen(pen);
		dc.Rectangle(&rect);
		
		CPen pen0;
		pen0.CreatePen(PS_SOLID, 1, enabled ? ::GetSysColor(COLOR_BTNHIGHLIGHT) : color);
		dc.SelectPen(pen0);
		rect.DeflateRect(1, 1);
		dc.Rectangle(&rect);

		wnd.ReleaseDC(dc);

		result = S_OK;

		return result;
	}

} // namespace my

