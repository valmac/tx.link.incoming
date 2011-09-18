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
 *  um.h
 */

#pragma once 

enum {
	UM_BASE = WM_USER,
	UM_TERMINATE,
	UM_SERVER_FAULT,
	UM_TRANS2QUIK,
	UM_TRANS2QUIK_EX,
	UM_RX_NEWDATA,
	UM_RX_TABLE,
	UM_RX_TABLE_CURRENT,
	UM_RX_TABLE_QUOTES,
	UM_RX_TABLE_BIDS,
	UM_RX_TABLE_DEALS,
	UM_RX_TABLE_STOPS,
	UM_RX_TABLE_LIMITS,
	UM_RX_TABLE_PORTFOLIO,
	UM_TRANSACTION,
	UM_SHOW_DIALOG_STOP,
	UM_SHOW_PROPERTIES,
	UM_SHOW_WINDOW,
	UM_UPDATE_INSTRUMENTS,
	UM_ENABLE_CANCEL,
	UM_LOG,
	UM_UPDATE_OSD,
	UM_USER_TRADE,
	UM_APPLY_SETTINGS,
	UM_CLOSE_WINDOW,
	UM_KEYUP,
	UM_KEYDOWN,
	UM_OK,
	UM_CANCEL,
	UM_APPLY,
	UM_OSD,
	UM_HISTORY,
	UM_REMOVE_TRANSACTION,
	UM_LOAD_TRADE,
	UM_RESTORE,
	UM_EDIT,
	UM_ACTION,
	UM_UPDATE,
	UM_MENU,
	UM_ACTIVE,
	UM_LAST,
};
