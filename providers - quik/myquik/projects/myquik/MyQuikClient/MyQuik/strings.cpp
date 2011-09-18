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
 *  strings.cpp
 */

#include "stdafx.h"
#include "strings.h"

LPCTSTR STRING_BUY = TEXT("Купить");
LPCTSTR STRING_SELL = TEXT("Продать");
LPCTSTR STRING_BUY_FIXED = TEXT("Купить по указанной цене");
LPCTSTR STRING_SELL_FIXED = TEXT("Продать по указанной цене");
LPCTSTR STRING_BUY_MARKET = TEXT("Купить по рыночной цене");
LPCTSTR STRING_SELL_MARKET = TEXT("Продать по рыночной цене");
LPCTSTR STRING_BUY_CURRENT = TEXT("Купить по цене последней сделки");
LPCTSTR STRING_SELL_CURRENT = TEXT("Продать по цене последней сделки");
LPCTSTR STRING_BUY_TA = TEXT("Купить по цене транзакции");
LPCTSTR STRING_SELL_TA = TEXT("Продать по цене транзакции");
LPCTSTR STRING_STOP_ORDER = TEXT("Поставить стоп");
LPCTSTR STRING_STOP_ORDER_ = TEXT("Поставить стоп...");
LPCTSTR STRING_STOP_BID_BUY = TEXT("Стоп-покупка по указанной цене");
LPCTSTR STRING_STOP_BID_SELL = TEXT("Стоп-продажа по указанной цене");
LPCTSTR STRING_STOP_BID_BUY_OFFSET = TEXT("Стоп-покупка от указанной цены");
LPCTSTR STRING_STOP_BID_SELL_OFFSET = TEXT("Стоп-продажа от указанной цены");
LPCTSTR STRING_STOP_BID_BUY_TA = TEXT("Стоп-покупка по цене транзакции");
LPCTSTR STRING_STOP_BID_SELL_TA = TEXT("Стоп-продажа по цене транзакции");
LPCTSTR STRING_STOP_BID_BUY_OFFSET_TA = TEXT("Стоп-покупка от цены транзакции");
LPCTSTR STRING_STOP_BID_SELL_OFFSET_TA = TEXT("Стоп-продажа от цены транзакции");
LPCTSTR STRING_STOP_BID_TA = TEXT("Защитный стоп от цены транзакции");
LPCTSTR STRING_CANCEL_BID = TEXT("Отменить заявку");
LPCTSTR STRING_CANCEL_ALL = TEXT("Отменить все заявки");
LPCTSTR STRING_CANCEL_ALL_BUY = TEXT("Отменить все заявки на покупку");
LPCTSTR STRING_CANCEL_ALL_SELL = TEXT("Отменить все заявки на продажу");
LPCTSTR STRING_CANCEL_ALL_STOPS = TEXT("Отменить все стоп-заявки");
LPCTSTR STRING_CHANGE_BID = TEXT("Заменить заявку");
LPCTSTR STRING_CHANGE_STOP_BID = TEXT("Заменить стоп-заявку");
LPCTSTR STRING_TRADE_ZERO = TEXT("Обнулить текущий трейд");
LPCTSTR STRING_TRADE_REVERSE = TEXT("Развернуть текущий трейд");

LPCTSTR STRING_TRANSACTION_INFO = TEXT("Информация о транзакциях");
LPCTSTR STRING_ACTIVE_BIDSNDEALS = TEXT("Активные заявки и сделки");

LPCTSTR STRING_INSTRUMENT = TEXT("Инструмент");
LPCTSTR STRING_PROPERTIES = TEXT("Свойства инструмента...");

LPCTSTR STRING_WND_DEALS = TEXT("Окно сделок");
LPCTSTR STRING_WND_GLASS = TEXT("Стакан заявок");
LPCTSTR STRING_WND_QUOTES = TEXT("Окно котировок");
LPCTSTR STRING_WND_HISTORY = TEXT("История транзакций");
LPCTSTR STRING_WND_MSGLOG = TEXT("Журнал сообщений");

LPCTSTR STRING_TOPMOST = TEXT("Поверх остальных окон");

LPCTSTR STRING_LISTOFDEALS = TEXT("Список сделок");
LPCTSTR STRING_OSD = TEXT("Область информации");
LPCTSTR STRING_CONTROLS = TEXT("Элементы управления");

LPCTSTR STRING_KEYS_SET_BASIC = TEXT("Основной набор кнопок");
LPCTSTR STRING_KEYS_SET_EXT = TEXT("Дополнительный набор кнопок");

LPCTSTR STRING_ABOUT = TEXT("Сведения о программе...");

LPCTSTR STRING_GLASS_VIEW = TEXT("Вид стакана");
#if 0
LPCTSTR STRING_GLASS_STYLE_1 = TEXT("Вертикально");
LPCTSTR STRING_GLASS_STYLE_2 = TEXT("Вертикально (валетом)");
LPCTSTR STRING_GLASS_STYLE_3 = TEXT("Горизонтально");
LPCTSTR STRING_GLASS_BUY_ABOVE = TEXT("Покупки показывать сверху");
LPCTSTR STRING_GLASS_FLIP_HORIZONTAL = TEXT("Отразить по горизонтали");
LPCTSTR STRING_GLASS_RAREFY = TEXT("Разреженный стакан");
LPCTSTR STRING_GLASS_USER_SEPARATE_COLUMN = TEXT("Отдельный столбец для своих заявок");
#endif

LPCTSTR STRING_DEALS_VIEW = TEXT("Вид окна сделок");
