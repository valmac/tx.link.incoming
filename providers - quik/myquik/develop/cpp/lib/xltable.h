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
 *  lib/xltable.h
 */

#pragma once

enum {
	TDT_TABLE = 0x10,
	TDT_FLOAT = 0x01,
	TDT_STRING = 0x02,
	TDT_BOOL = 0x03,
	TDT_ERROR = 0x04,
	TDT_BLANK = 0x05,
	TDT_TYPE = 0x80,
	TDT_FORMAT = 0x81,
};
