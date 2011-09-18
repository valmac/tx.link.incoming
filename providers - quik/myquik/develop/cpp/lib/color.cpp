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
 *  lib/color.cpp
 */

#include "stdafx.h"
#include "color.h"

namespace my {
	namespace lib {

		COLORREF SumColors(COLORREF color1, COLORREF color2)
		{
			COLORREF sum;
			int r1 = GetRValue(color1); int g1 = GetGValue(color1); int b1 = GetBValue(color1);
			int r2 = GetRValue(color2); int g2 = GetGValue(color2); int b2 = GetBValue(color2);
			int r = (r1 + r2) / 2; int g = (g1 + g2) / 2; int b = (b1 + b2) / 2;
			sum = RGB(r, g, b);
			return sum;
		}

		COLORREF SumColorsAlpha(COLORREF color1, COLORREF color2, double alpha)
		{
			int r1 = GetRValue(color1); int g1 = GetGValue(color1); int b1 = GetBValue(color1);
			int r2 = GetRValue(color2); int g2 = GetGValue(color2); int b2 = GetBValue(color2);
			int r = (int)((double)r1*(1 - alpha) + (double)r2*alpha);
			int g = (int)((double)g1*(1 - alpha) + (double)g2*alpha);
			int b = (int)((double)b1*(1 - alpha) + (double)b2*alpha);
			return RGB(r, g, b);
		}

		COLORREF AddColor(COLORREF color1, COLORREF color2)
		{
			int r1 = GetRValue(color1); int g1 = GetGValue(color1); int b1 = GetBValue(color1);
			int r2 = GetRValue(color2); int g2 = GetGValue(color2); int b2 = GetBValue(color2);
			int r = (r1 + r2); int g = (g1 + g2); int b = (b1 + b2);
			my::color::Normalize(r, g, b);
			return RGB(r, g, b);
		}

		COLORREF MulColor (COLORREF color, double k)
		{
			int r = (int)((double)GetRValue(color) * k);
			int g = (int)((double)GetGValue(color) * k);
			int b = (int)((double)GetBValue(color) * k);
			my::color::Normalize(r, g, b);
			return RGB(r, g, b);
		}

		COLORREF MakeColorLighter(COLORREF color, int d)
		{
			int r = (GetRValue(color) + d);
			int g = (GetGValue(color) + d);
			int b = (GetBValue(color) + d);
			my::color::Normalize(r, g, b);
			return RGB(r, g, b);
		}

		COLORREF MakeColorDarker(COLORREF color, int d)
		{
			int r = GetRValue(color) - d;
			int g = GetGValue(color) - d;
			int b = GetBValue(color) - d;
			my::color::Normalize(r, g, b);
			return RGB(r, g, b);
		}

		COLORREF MakeColorLighterEx(COLORREF color, int d, int lim, int proportionally)
		{
			int r = GetRValue(color);
			int g = GetGValue(color);
			int b = GetBValue(color);
			int val = proportionally ? max(max(r, g), b) : min(min(r, g), b);
			if (val >= lim)
				return color;
			if (val + d > lim)
				d = lim - val;
			r += d; g += d; b += d;
			my::color::Normalize(r, g, b);
			color = RGB(r, g, b);
			return color;
		}

	} // namespace lib

	namespace color {

		void Normalize(int & r, int & g, int & b)
		{
			if (r > 255)
				r = 255;
			else if (r < 0)
				r = 0;
			if (g > 255)
				g = 255;
			else if (g < 0)
				g = 0;
			if (b > 255) 
				b = 255;
			else if (b < 0)
				b = 0;
		}
	
	} // namespace color

} // namespace my
