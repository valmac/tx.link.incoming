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
 *  lib/color.h
 */

#pragma once

namespace my {

	namespace lib {
		COLORREF SumColors(COLORREF color1, COLORREF color2);
		COLORREF SumColorsAlpha(COLORREF color1, COLORREF color2, double alpha = 0.5);

		COLORREF AddColor(COLORREF color1, COLORREF color2);
		COLORREF MulColor (COLORREF color, double k);

		COLORREF MakeColorLighter(COLORREF color, int d);
		COLORREF MakeColorDarker(COLORREF color, int d);
		COLORREF MakeColorLighterEx(COLORREF color, int d = 255, int lim = 255, int proportionally = 1);
	} // namespace lib

	namespace color {
		void Normalize(int & r, int & g, int & b);
	} // namespace color

} // namespace my
