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
 *  shortcuts.h
 */

#pragma once

#include <map>

namespace my {
	typedef ::std::map<int, DWORD> ShortcutMap; 
	typedef ShortcutMap::value_type ShortcutKey;

	union Shortcut
	{
		BYTE b[4];
		DWORD val32;
	};

} // namespace my

enum {
	SCK_LBUTTON = 0x0001,
	SCK_RBUTTON = 0x0002,
	SCK_SHIFT = 0x0004,
	SCK_CONTROL = 0x0008,
	SCK_MBUTTON = 0x0010,
	SCK_XBUTTON1 = 0x0020,
	SCK_XBUTTON2 = 0x0040,

	SCK_LBUTTONDBL = 0x0100,
	SCK_RBUTTONDBL = 0x0200,
	SCK_MBUTTONDBL = 0x0400,
	SCK_XBUTTONDBL = 0x0800,

	SCK_ALT = 0x8000,
};

#define INVALID_SHORTCUT (DWORD)(-1)

#define MAKESHORTCUT(key, key2, flags) (DWORD)(((flags) << 16) | ((key) & 0xff) | (((key2) & 0xff) << 8 ))
#define MAKESHORTCUT2(keys, flags) MAKESHORTCUT((keys)[0], (keys)[1], flags)

#define SHORTCUT2KEY(shortcut) ((shortcut) & 0xff)
#define SHORTCUT2KEY2(shortcut) (((shortcut) >> 8) & 0xff)
#define SHORTCUT2FLAGS(shortcut) (((shortcut) >> 16) & 0xffff)
#define SHORTCUT2CTRL(shortcut) (SHORTCUT2FLAGS((shortcut)) & SCK_CONTROL)
#define SHORTCUT2SHIFT(shortcut) (SHORTCUT2FLAGS((shortcut)) & SCK_SHIFT)
#define SHORTCUT2ALT(shortcut) (SHORTCUT2FLAGS((shortcut)) & SCK_ALT)

#define SHORTCUT2KEYS(shortcut, keys) (keys)[0] = SHORTCUT2KEY((shortcut)); (keys)[1] = SHORTCUT2KEY2((shortcut));

#define VK_ALT VK_MENU

int ShortcutToText(DWORD shortcut, LPTSTR text, int size);
