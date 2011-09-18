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
 *  actions.cpp
 */

#include "stdafx.h"
#include "actions.h"
#include "MyQuik.h"
#include "strings.h"

static LPCTSTR s_actionNames[] = {
	STRING_BUY,
	STRING_SELL,
	STRING_BUY_FIXED,
	STRING_SELL_FIXED,
	STRING_BUY_MARKET,
	STRING_SELL_MARKET,
	STRING_BUY_CURRENT,
	STRING_SELL_CURRENT,
	STRING_BUY_TA,
	STRING_SELL_TA,

	STRING_STOP_ORDER,
	STRING_STOP_BID_BUY,
	STRING_STOP_BID_SELL,
	STRING_STOP_BID_BUY_OFFSET,
	STRING_STOP_BID_SELL_OFFSET,
	STRING_STOP_BID_BUY_TA,
	STRING_STOP_BID_SELL_TA,
	STRING_STOP_BID_BUY_OFFSET_TA,
	STRING_STOP_BID_SELL_OFFSET_TA,
	STRING_STOP_BID_TA,
#if USE_CHANGE_ACTIVE_BID
	STRING_CHANGE_BID,
#endif
	STRING_CANCEL_BID,
	STRING_CANCEL_ALL,
	STRING_CANCEL_ALL_BUY,
	STRING_CANCEL_ALL_SELL,
	STRING_CANCEL_ALL_STOPS,

	TEXT("Стакан: заявка по указанной цене"),
	TEXT("Стакан: купить по указанной цене"),
	TEXT("Стакан: продать по указанной цене"),
	TEXT("Стакан: заявка по рыночной цене"),
	TEXT("Стакан: купить по рыночной цене"),
	TEXT("Стакан: продать по рыночной цене"),
	TEXT("Стакан: заявка по цене последней сделки"),
	TEXT("Стакан: купить по цене последней сделки"),
	TEXT("Стакан: продать по цене последней сделки"),
	TEXT("Стакан: стоп-заявка по указанной цене"), //  по указанной цене
	TEXT("Стакан: стоп-покупка по указанной цене"),
	TEXT("Стакан: стоп-продажа по указанной цене"),
	TEXT("Стакан: стоп-заявка со смещением"),
	TEXT("Стакан: стоп-покупка со смещением"),
	TEXT("Стакан: стоп-продажа со смещением"),
	TEXT("Стакан: отменить заявку"),
	TEXT("Стакан: отменить все заявки"),

	TEXT("Стакан: выделить элемент маркером"),
	TEXT("Стакан: вызвать контекстное меню"),

	TEXT("Остановить движение цен в стакане"),

	STRING_TRADE_ZERO,
	STRING_TRADE_REVERSE,

	TEXT("Показать информацию об инструменте"),
	TEXT("Выбрать следующий инструмент"),
	TEXT("Выбрать предыдущий инструмент"),
	TEXT("Редактировать параметры инструмента"),
#if USE_ACTION_CAPTURE_VALUE
	TEXT("Использовать указанное значение"),
#endif
#if USE_ACTION_CHANGE_QUANTITY
	TEXT("Увеличить количество"),
	TEXT("Уменьшить количество"),
	TEXT("Увеличить количество в 2 раза"),
	TEXT("Уменьшить количество в 2 раза"),
	TEXT("Увеличить количество в 10 раз"),
	TEXT("Уменьшить количество в 10 раз"),
#endif
	TEXT("Показать/скрыть окно сделок"),
	TEXT("Показать/скрыть стакан заявок"),
	TEXT("Показать/скрыть историю транзакций"),
	TEXT("Показать/скрыть окно сообщений"),

	TEXT("Показывать информацию о транзакциях"),
	TEXT("Показывать активные заявки и сделки"),	
	TEXT("Показывать разреженный стакан"),
	TEXT("Показывать нейтральную зону стакана"),

	TEXT("Переключить набор кнопок окна сделок"),

	TEXT("Вызвать окно настроек программы"),

	NULL,
};

LPCTSTR GetActionName(int index)
{
	LPCTSTR name = NULL;
	name = s_actionNames[index];
	return name;
}

Action::Type CheckActionByKey(int vkey, int & flags, const my::ShortcutMap & map, UINT msg, LPARAM lParam)
{
	Action::Type action = ACTION_TYPE_UNKNOWN;
	DWORD shortcut = MakeShortcutKey(vkey, flags, msg, lParam);
	if (shortcut != INVALID_SHORTCUT && shortcut != 0)
		action = CheckActionByShortcutKey(shortcut, map);
	return action;
}

Action::Type CheckActionByShortcutKey(DWORD shortcut, const my::ShortcutMap & map)
{
	Action::Type action = ACTION_TYPE_UNKNOWN;
	my::ShortcutMap::const_iterator it = map.begin();
#if !USE_KEY_ORDER
	DWORD shortcut0, shortcut1;
	int flags = SHORTCUT2FLAGS(shortcut);
	int key0 = SHORTCUT2KEY(shortcut); int key1 = SHORTCUT2KEY2(shortcut);
	shortcut0 = shortcut; shortcut1 = MAKESHORTCUT(key1, key0, flags);
#endif
	for (it = map.begin(); it != map.end(); it++)
	{
#if USE_KEY_ORDER
		if (it->second == shortcut)
#else
		// Не важен порядок нажатия клавиш:
		if (it->second == shortcut0 || it->second == shortcut1)
#endif
		{
			action = static_cast<Action::Type>((*it).first);
			break;
		}
	}
	return action;
}

void InitActionsDefault(my::ShortcutMap & map)
{
	map.clear();
	// Сначала обнуляем всю карту:
	for (int i = 0; ; i++)
	{
		LPCTSTR name = GetActionName(i);
		if (name)
			map[i] = MAKESHORTCUT(0, 0, 0);
		else
			break;
	} // for (i)
	// Назначаем некоторые комбинации:
	map[ACTION_BUY] = MAKESHORTCUT(VK_OEM_PLUS, 0, SCK_CONTROL|SCK_ALT|SCK_SHIFT);
	map[ACTION_SELL] = MAKESHORTCUT(VK_OEM_MINUS, 0, SCK_CONTROL|SCK_ALT|SCK_SHIFT);
	map[ACTION_BUY_FIXED] = MAKESHORTCUT(VK_OEM_PLUS, 0, SCK_CONTROL);
	map[ACTION_SELL_FIXED] = MAKESHORTCUT(VK_OEM_MINUS, 0, SCK_CONTROL);
	map[ACTION_BUY_MARKET] = MAKESHORTCUT(VK_OEM_PLUS, 0, SCK_CONTROL|SCK_SHIFT);
	map[ACTION_SELL_MARKET] = MAKESHORTCUT(VK_OEM_MINUS, 0, SCK_CONTROL|SCK_SHIFT);
	map[ACTION_BUY_CURRENT] = MAKESHORTCUT(VK_OEM_PLUS, 0, SCK_ALT);
	map[ACTION_SELL_CURRENT] = MAKESHORTCUT(VK_OEM_MINUS, 0, SCK_ALT);

	map[ACTION_STOP_ORDER] = MAKESHORTCUT('P', 0, SCK_CONTROL);
	map[ACTION_STOP_BID_BUY] = MAKESHORTCUT(VK_OEM_6, 0, SCK_CONTROL);
	map[ACTION_STOP_BID_SELL] = MAKESHORTCUT(VK_OEM_4, 0, SCK_CONTROL);
	map[ACTION_STOP_BID_BUY_OFFSET] = MAKESHORTCUT(VK_OEM_6, 0, SCK_CONTROL|SCK_SHIFT);
	map[ACTION_STOP_BID_SELL_OFFSET] = MAKESHORTCUT(VK_OEM_4, 0, SCK_CONTROL|SCK_SHIFT);

	map[ACTION_TRADE_ZERO] = MAKESHORTCUT('Z', 0, SCK_CONTROL);
	map[ACTION_TRADE_REVERSE] = MAKESHORTCUT(VK_BACK, 0, SCK_CONTROL);

	map[ACTION_CANCEL_ORDER] = MAKESHORTCUT(VK_SPACE, 0, SCK_CONTROL);
	map[ACTION_CANCEL_ORDERS_ALL] = MAKESHORTCUT(VK_SPACE, 0, SCK_CONTROL|SCK_ALT);

	map[ACTION_INSTRUMENT_NEXT] = MAKESHORTCUT(VK_OEM_2, VK_OEM_PERIOD, 0);
	map[ACTION_INSTRUMENT_PREVIOS] = MAKESHORTCUT(VK_OEM_2, VK_OEM_COMMA, 0);
	map[ACTION_SHOW_INSTRUMENT] = MAKESHORTCUT(VK_OEM_2, 0, 0);
	map[ACTION_EDIT_INSTRUMENT] = MAKESHORTCUT('I', 0, SCK_CONTROL);
#if USE_ACTION_CHANGE_QUANTITY
	map[ACTION_INCREMENT_QUANTITY_1] = MAKESHORTCUT('1', VK_OEM_PLUS, 0);
	map[ACTION_DECREMENT_QUANTITY_1] = MAKESHORTCUT('1', VK_OEM_MINUS, 0);
	map[ACTION_INCREMENT_QUANTITY_2] = MAKESHORTCUT('2', VK_OEM_PLUS, 0);
	map[ACTION_DECREMENT_QUANTITY_2] = MAKESHORTCUT('2', VK_OEM_MINUS, 0);
	map[ACTION_INCREMENT_QUANTITY_10] = MAKESHORTCUT('0', VK_OEM_PLUS, 0);
	map[ACTION_DECREMENT_QUANTITY_10] = MAKESHORTCUT('0', VK_OEM_MINUS, 0);
#endif
	map[ACTION_SHOW_HIDE_WINDOW_DEALS] = MAKESHORTCUT('D', 0, SCK_CONTROL);
	map[ACTION_SHOW_HIDE_WINDOW_GLASS] = MAKESHORTCUT('G', 0, SCK_CONTROL);
#if 0
	map[ACTION_SHOW_HIDE_WINDOW_QUOTES] = MAKESHORTCUT('Q', 0, SCK_CONTROL);
#endif
	map[ACTION_SHOW_HIDE_WINDOW_MESSAGES] = MAKESHORTCUT('L', 0, SCK_CONTROL);
	map[ACTION_SHOW_HIDE_WINDOW_HISTORY] = MAKESHORTCUT('H', 0, SCK_CONTROL);
#if 0
	map[ACTION_SHOW_TRANSACTION_INFO] = MAKESHORTCUT('T', 0, SCK_CONTROL);
#endif
	map[ACTION_SHOW_ACTIVE_BIDS_AND_DEALS] = MAKESHORTCUT('U', 0, SCK_CONTROL);
	map[ACTION_SHOW_RAREFIED_GLASS] = MAKESHORTCUT('R', 0, SCK_CONTROL);
	map[ACTION_SHOW_NEUTRAL_ZONE] = MAKESHORTCUT('N', 0, SCK_CONTROL);

	map[ACTION_CHANGE_BUTTONS] = MAKESHORTCUT('K', 0, SCK_CONTROL);
#if 1
	map[ACTION_SETTINGS] = MAKESHORTCUT(VK_F12, 0, 0);
#endif
	map[ACTION_GLASS_BID_FIXED] = MAKESHORTCUT(0, 0, SCK_CONTROL|SCK_LBUTTON);
	map[ACTION_GLASS_BUY_FIXED] = MAKESHORTCUT(VK_OEM_PLUS, 0, SCK_LBUTTON);
	map[ACTION_GLASS_SELL_FIXED] = MAKESHORTCUT(VK_OEM_MINUS, 0, SCK_LBUTTON);
	map[ACTION_GLASS_BID_MARKET] = MAKESHORTCUT(0, 0, SCK_CONTROL|SCK_RBUTTON);
	map[ACTION_GLASS_BUY_MARKET] = MAKESHORTCUT(VK_OEM_PLUS, 0, SCK_RBUTTON);
	map[ACTION_GLASS_SELL_MARKET] = MAKESHORTCUT(VK_OEM_MINUS, 0, SCK_RBUTTON);
	map[ACTION_GLASS_BID_CURRENT] = MAKESHORTCUT(0, 0, SCK_ALT|SCK_RBUTTON);
#if 0
	map[ACTION_GLASS_BUY_CURRENT] = MAKESHORTCUT(VK_OEM_PLUS, 0, SCK_ALT|SCK_RBUTTON);
	map[ACTION_GLASS_SELL_CURRENT] = MAKESHORTCUT(VK_OEM_MINUS, 0, SCK_ALT|SCK_RBUTTON);
#endif
	map[ACTION_GLASS_CANCEL_BID] = MAKESHORTCUT(VK_SPACE, 0, 0);
	map[ACTION_GLASS_CANCEL_ALL] = MAKESHORTCUT(0, 0, 0);

	map[ACTION_GLASS_STOP_BID] = MAKESHORTCUT('P', 0, 0);
	map[ACTION_GLASS_STOP_BID_BUY] = MAKESHORTCUT(VK_OEM_6, 0, 0);
	map[ACTION_GLASS_STOP_BID_SELL] = MAKESHORTCUT(VK_OEM_4, 0, 0);
	map[ACTION_GLASS_STOP_BID_OFFSET] = MAKESHORTCUT('P', 0, SCK_SHIFT);
	map[ACTION_GLASS_STOP_BID_BUY_OFFSET] = MAKESHORTCUT(VK_OEM_6, 0, SCK_SHIFT);
	map[ACTION_GLASS_STOP_BID_SELL_OFFSET] = MAKESHORTCUT(VK_OEM_4, 0, SCK_SHIFT);

	map[ACTION_GLASS_SELECT_ITEM] = MAKESHORTCUT(0, 0, SCK_LBUTTON);
	map[ACTION_GLASS_POPUP_MENU] = MAKESHORTCUT(0, 0, SCK_RBUTTON);

	map[ACTION_FREEZE] = MAKESHORTCUT(VK_PAUSE, 0, 0);
}
