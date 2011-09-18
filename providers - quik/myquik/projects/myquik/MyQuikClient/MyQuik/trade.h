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
 *  trade.h
 */

#pragma once

#include <list>
#include "transactions.h"


struct TradeItem {
	double N;
	SYSTEMTIME time;
	Transaction::Operation operation;
	Transaction::Action action;
	Transaction::Status status;
	double price;
	double quantity;
	double volume;
	double volume2;
#if 1
	Transaction * pTa;
#endif
	TradeItem() {}
	TradeItem(Transaction * pTa)
		: N(pTa->orderKey), time(pTa->time), operation(pTa->operation), 
		action(pTa->action), status(pTa->status), 
		price(pTa->price), quantity(pTa->quantity), volume(pTa->volume), volume2(pTa->price*pTa->quantity)		
	{this->pTa = pTa;}
};

enum {
	F_SEARCH_ANY = 0x00,
	F_SEARCH_ACTION = 0x01,
	F_SEARCH_STATUS = 0x02,
	F_SEARCH_OPERATION = 0x04,
	F_SEARCH_PRICE = 0x08,
	F_SEARCH_NUMBER = 0x10,
	F_SEARCH_TIME = 0x20,
	F_SEARCH_TA = 0x80,
	F_SEARCH_BID = 0x100,
	F_SEARCH_STOP = 0x200,
};

struct Trade {
	TCHAR name[MAX_PATH];

	int operation; // направление торговли;
	double price; // средн€€ цена;
	double price2; // средн€€ цена 2 (актуально дл€ RTS Fut и т.п.);
	double volume; // общий объем;
	double volume2; // общий объем;
	double quantity;

	double profit;
	int valid;

	struct Last {
		double profit; // последний доход;
		int valid;
	} last;

	inline void Reset() {memset(this, 0, sizeof(Trade));}
};

//
// ListOfTradeItems
// 
class ListOfTradeItems : public std::list<TradeItem>
{
	friend class Trader;
public:
	ListOfTradeItems(LPCTSTR name);

	LPCTSTR GetName() const {return trade.name;}

	TradeItem * FindItem(ListOfTradeItems::iterator & it, int flags, double N, int action, int state, int operation, double price, double quantity);
	const TradeItem * FindItem(ListOfTradeItems::const_iterator & it, int flags, double N, int action, int state, int operation, double price, double quantity) const;
	TradeItem * AddItem(const TradeItem & item);
	void RemoveItem(ListOfTradeItems::iterator & it, TradeItem * pItem);

	void Add(Transaction * pTransaction);
	void Remove(Transaction * pTransaction);
	void Modify(Transaction * pTransaction);

protected:
	Trade trade;
	ListOfTradeItems::const_iterator it;
}; // class ListOfTradeItems

typedef std::list<ListOfTradeItems*> ListOfListOfTradeItems;

class TradeItems : public ListOfListOfTradeItems
{
	friend class Trader;
public:
	TradeItems();
	~TradeItems();

	ListOfTradeItems * Add(LPCTSTR name);
	void Remove(TradeItems::iterator it);
	void RemoveAll();
	ListOfTradeItems * Find(LPCTSTR name, TradeItems::iterator * pIt = NULL);

	int GetCount() const {return size();}

	const ListOfTradeItems * GetCurrent() const {return pCurrent;}
	ListOfTradeItems * GetCurrent() {return pCurrent;}

	void SetCurrent(ListOfTradeItems * pCurrent) {this->pCurrent = pCurrent;}

protected:
	ListOfTradeItems * pCurrent;
};

class Trader 
{
	friend class TradeItems;
public:
	Trader();
	~Trader();

	int SetCurrent(LPCTSTR name);
	int GetTrade(LPCTSTR name, Trade & trade);
	int GetCurrentTrade(Trade & trade);

	BOOL IsActiveTrade(LPCTSTR name);

	int OnAction(LPCTSTR name, Transaction * pTransaction);

	static void ResetTrade(Trade & trade);
	void ResetTrade(LPCTSTR name);
	void ResetAllTrades();

	void BeginFindItem();
	void EndFindItem();
	const TradeItem * FindItem(int flags, double N, int action, int state, int operation, double price, double quantity);

	int FindItems(::std::list<TradeItem> & list, double price);
	int FindItems(::std::list<TradeItem> & list, int flags, int operation, double price);

protected:
	TradeItems m_tradeItems;
}; // class Trader

