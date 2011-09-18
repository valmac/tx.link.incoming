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
 *  actions.h
 */

#pragma once

#include "shortcuts.h"

enum ActionType {
	ACTION_BUY, 
	ACTION_SELL,
	ACTION_BUY_FIXED, 
	ACTION_SELL_FIXED,
	ACTION_BUY_MARKET, 
	ACTION_SELL_MARKET, 
	ACTION_BUY_CURRENT, 
	ACTION_SELL_CURRENT, 
	ACTION_BUY_TA, 
	ACTION_SELL_TA, 

	ACTION_STOP_ORDER,

	ACTION_STOP_BID_BUY,
	ACTION_STOP_BID_SELL,
	ACTION_STOP_BID_BUY_OFFSET,
	ACTION_STOP_BID_SELL_OFFSET,
	ACTION_STOP_BID_BUY_TA,
	ACTION_STOP_BID_SELL_TA,
	ACTION_STOP_BID_BUY_OFFSET_TA,
	ACTION_STOP_BID_SELL_OFFSET_TA,
	ACTION_STOP_BID_TA,
#if USE_CHANGE_ACTIVE_BID
	ACTION_CHANGE_BID,
#endif
	ACTION_CANCEL_ORDER,
	ACTION_CANCEL_ORDERS_ALL,
	ACTION_CANCEL_ORDERS_BUY,
	ACTION_CANCEL_ORDERS_SELL,
	ACTION_CANCEL_ORDERS_ALL_STOP,

	ACTION_GLASS_BID_FIXED,
	ACTION_GLASS_BUY_FIXED,
	ACTION_GLASS_SELL_FIXED,
	ACTION_GLASS_BID_MARKET,
	ACTION_GLASS_BUY_MARKET,
	ACTION_GLASS_SELL_MARKET,
	ACTION_GLASS_BID_CURRENT,
	ACTION_GLASS_BUY_CURRENT,
	ACTION_GLASS_SELL_CURRENT,
	ACTION_GLASS_STOP_BID,
	ACTION_GLASS_STOP_BID_BUY,
	ACTION_GLASS_STOP_BID_SELL,
	ACTION_GLASS_STOP_BID_OFFSET,
	ACTION_GLASS_STOP_BID_BUY_OFFSET,
	ACTION_GLASS_STOP_BID_SELL_OFFSET,

	ACTION_GLASS_CANCEL_BID,
	ACTION_GLASS_CANCEL_ALL,

	ACTION_GLASS_SELECT_ITEM,
	ACTION_GLASS_POPUP_MENU,

	ACTION_FREEZE,

	ACTION_TRADE_ZERO,
	ACTION_TRADE_REVERSE,

	ACTION_SHOW_INSTRUMENT,
	ACTION_INSTRUMENT_NEXT,
	ACTION_INSTRUMENT_PREVIOS,
	ACTION_EDIT_INSTRUMENT,
#if USE_ACTION_CAPTURE_VALUE
	ACTION_CAPTURE_VALUE,
#endif
#if USE_ACTION_CHANGE_QUANTITY
	ACTION_INCREMENT_QUANTITY_1,
	ACTION_DECREMENT_QUANTITY_1,
	ACTION_INCREMENT_QUANTITY_2,
	ACTION_DECREMENT_QUANTITY_2,
	ACTION_INCREMENT_QUANTITY_10,
	ACTION_DECREMENT_QUANTITY_10,
#endif
	ACTION_SHOW_HIDE_WINDOW_DEALS,
	ACTION_SHOW_HIDE_WINDOW_GLASS,
	ACTION_SHOW_HIDE_WINDOW_HISTORY,
	ACTION_SHOW_HIDE_WINDOW_MESSAGES,

	ACTION_SHOW_TRANSACTION_INFO,
	ACTION_SHOW_ACTIVE_BIDS_AND_DEALS,
	ACTION_SHOW_RAREFIED_GLASS,
	ACTION_SHOW_NEUTRAL_ZONE,	

	ACTION_CHANGE_BUTTONS,
	ACTION_SETTINGS,

	ACTION_TYPE_UNKNOWN,
};

struct Action {
	typedef ActionType Type;

	enum Flags {
		F_UP = 0x01,
		F_CHECKMOUSEASYNC = 0x10,
	};

	Type type;
	int flags;

	Action() {}
	Action(Type type, int flags) {this->type = type; this->flags = flags;}

}; // struct Action

#define ACTION_FLAG_END(flags) ((flags) & Action::F_UP)


LPCTSTR GetActionName(int index);
Action::Type CheckActionByShortcutKey(DWORD shortcut, const my::ShortcutMap & map);
Action::Type CheckActionByKey(int vkey, int & flags, const my::ShortcutMap & map, UINT msg, LPARAM lParam);
void InitActionsDefault(my::ShortcutMap & map);
