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
 *  trade.cpp
 */

#include "stdafx.h"
#include "trade.h"
#include <std.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

//
// ListOfTradeItems
//

ListOfTradeItems::ListOfTradeItems(LPCTSTR name)
{	
	memset(&trade, 0, sizeof(trade));
	SAFE_TSTRCPY(trade.name, name);	
	trade.operation = -1;

	this->it = end();
}

TradeItem * ListOfTradeItems::AddItem(const TradeItem & item)
{
	this->push_back(item);
	return &this->back();
}

TradeItem * ListOfTradeItems::FindItem(ListOfTradeItems::iterator & it, int flags, double N, int action, int state, int operation, double price, double quantity)
{
	TradeItem * pItem;
	for ( ; it != end(); it++)
	{
		pItem = &(*it);
		if (flags != F_SEARCH_ANY)
		{
			if (flags & F_SEARCH_NUMBER)
				if (pItem->N != N)
					continue;
			if (flags & F_SEARCH_ACTION)
				if (pItem->action != action)
					continue;
			if (flags & F_SEARCH_STATUS)
				if (pItem->status != state)
					continue;
			if (flags & F_SEARCH_OPERATION)
				if (pItem->operation != operation)
					continue;
			if (flags & F_SEARCH_PRICE)
				if (pItem->price != price)
					continue;
		}
		it++;
		return pItem;
	} // for (it)
	return NULL;
}

const TradeItem * ListOfTradeItems::FindItem(ListOfTradeItems::const_iterator & it, int flags, double N, int action, int state, int operation, double price, double quantity) const
{
	const TradeItem * pItem;
	for ( ; it != end(); it++)
	{
		pItem = &(*it);
		if (flags != F_SEARCH_ANY)
		{
			if (flags & F_SEARCH_NUMBER)
				if (pItem->N != N)
					continue;
			if (flags & F_SEARCH_ACTION)
				if (pItem->action != action)
					continue;
			if (flags & F_SEARCH_STATUS)
				if (pItem->status != state)
					continue;
			if (flags & F_SEARCH_OPERATION)
				if (pItem->operation != operation)
					continue;
			if (flags & F_SEARCH_PRICE)
				if (pItem->price != price)
					continue;
		}
		it++;
		return pItem;
	} // for (it)
	return NULL;
}

void ListOfTradeItems::RemoveItem(ListOfTradeItems::iterator & it, TradeItem * pItem)
{
	//if (pItem != NULL)
	{
#if 0
		ListOfTradeItems::iterator prev = it;
		--prev;
		int count = size();
		this->erase(prev);
		count = size();
#else
		this->erase(it);
#endif
	}
}

void ListOfTradeItems::Add(Transaction * pTransaction)
{
	TradeItem item(pTransaction);

	AddItem(item);
}

void ListOfTradeItems::Remove(Transaction * pTransaction)
{
	TradeItem item(pTransaction);

	ListOfTradeItems::iterator it = begin();
	TradeItem * pItem = FindItem(it, F_SEARCH_NUMBER, item.N, 0, 0, 0, 0, 0);
	if (pItem != NULL)
		RemoveItem(--it, pItem);
}

void ListOfTradeItems::Modify(Transaction * pTransaction)
{
	TradeItem item(pTransaction);
#if defined _DEBUG && 0
	AddItem(item);
	return;
#endif
	ListOfTradeItems::iterator it, cit;
	TradeItem * pItem;

	double Q = trade.quantity;
	if (trade.quantity == 0)
		trade.operation = item.operation;

	if (trade.operation == item.operation)
	{// Продолжаем торговать в том же направлении
		AddItem(item);

		trade.quantity += item.quantity;
#if 0
		if (trade.quantity > 0)
		{
			trade.volume += item.volume;
			trade.price = trade.volume / trade.quantity;
		}
#else
		// Определяем среднюю цену и суммарный объем ниже.
#endif
	}
	else
	{
		double minPrice, maxPrice;
		for (;;)
		{		
			pItem = NULL;
			if (item.operation == Transaction::OPERATION_SELL)
			{// Ищем минимальную цену
				minPrice = (double)_I64_MAX;
				for (it = begin(); it != end(); it++)
				{
					if (Transaction::IsExecuted((*it).status) && !Transaction::IsActionStop((*it).action))
					if ((*it).price < minPrice) 
					{ 
						pItem = &(*it); 
						cit = it; 
						minPrice = pItem->price;
					}
				} // for (it)
			}
			else
			{// Ищем максимальную цену
				maxPrice = 0;
				for (it = begin(); it != end(); it++)
				{
					if (Transaction::IsExecuted((*it).status) && !Transaction::IsActionStop((*it).action))
					if ((*it).price > maxPrice)
					{ 
						pItem = &(*it); 
						cit = it; 
						maxPrice = pItem->price;
					}
				} // for (it)
			}
			if (pItem != NULL)
			{
				if (pItem->quantity == item.quantity)
				{					
					trade.quantity -= item.quantity;
					RemoveItem(cit, pItem);
					break;
				}
				else if (pItem->quantity > item.quantity)
				{
					pItem->volume = pItem->volume / pItem->quantity;
					pItem->volume2 = pItem->volume2 / pItem->quantity;
					pItem->quantity -= item.quantity;
					pItem->volume = pItem->volume * pItem->quantity;
					pItem->volume2 = pItem->volume2 * pItem->quantity;
					trade.quantity -= item.quantity;
					break;
				}
				else// if (pItem->quantity < item.quantity)
				{					
					trade.quantity -= pItem->quantity;
					// Модифицируем элемент:
					item.volume = item.volume / item.quantity;
					item.volume2 = item.volume2 / item.quantity;
					item.quantity -= pItem->quantity;
					item.volume = item.volume * item.quantity;
					item.volume2 = item.volume2 * item.quantity;
					RemoveItem(cit, pItem);
					//
					// Продолжаем цикл.
				}
			} // if (pItem != NULL)
			else
			{// Переворот трэйда
				AddItem(item);
				trade.quantity += item.quantity;
				trade.operation = item.operation;
				break;
			}
		} // for (;;)		
	}
	if (trade.quantity == 0)
	{// Изменение направления торговли
#if 0
		if (Q)
		{
			trade.last.profit = trade.profit;
			trade.last.valid = TRUE;
		}
#endif
		trade.volume = trade.volume2 = 0;
		trade.price = trade.price2 = 0;		
	}
	else
	{// Определяем среднюю цену
		if (trade.quantity < 0)
			trade.quantity = -trade.quantity;
		trade.volume = 0;
		trade.volume2 = 0;
		for (it = begin(); it != end(); it++)
		{					
			if (Transaction::IsExecuted((*it).status) && !Transaction::IsActionStop((*it).action))
			{
				pItem = &(*it); 
				trade.volume += pItem->volume;
				trade.volume2 += pItem->volume2;
			}
		} // for (it)
		trade.price = trade.volume / trade.quantity;
		trade.price2 = trade.volume2 / trade.quantity;
	}
}

//
// class TradeItems
//

TradeItems::TradeItems()
{
	this->pCurrent = NULL;
}

TradeItems::~TradeItems()
{
	RemoveAll();
}

ListOfTradeItems * TradeItems::Find(LPCTSTR name, TradeItems::iterator * pIt)
{
	ListOfTradeItems * pList;
	ListOfListOfTradeItems::iterator it;
	for (it = begin(); it != end(); it++)
	{
		pList = (*(it));
		if (0 == lstrcmp(name, pList->GetName()))
		{
			if (pIt)
				*pIt = it;
			return pList;
		}
	}
	return NULL;
}

ListOfTradeItems * TradeItems::Add(LPCTSTR name)
{
	ListOfTradeItems * pList = NULL;
	pList = new ListOfTradeItems(name);
	if (pList != NULL)
	{
		this->push_back(pList);
	}
	return pList;
}

void TradeItems::Remove(TradeItems::iterator it)
{
	ListOfTradeItems * pList = *(it);
	delete pList;
	erase(it);
}

void TradeItems::RemoveAll()
{
	ListOfTradeItems * pList;
	ListOfListOfTradeItems::iterator it;
	for (it = begin(); it != end(); it++)
	{
		pList = *it;
		delete pList;
	}
	this->clear();
}

//
// class Trader 
//

Trader::Trader()
{
	//LoadState();
}

Trader::~Trader()
{
	//SaveState();
}

int Trader::SetCurrent(LPCTSTR name)
{
	int status = E_INVALIDARG;
	if (name == NULL)
	{
		m_tradeItems.SetCurrent(NULL);
		status = S_OK;
	}
	else
	{
		ListOfTradeItems * pList = m_tradeItems.Find(name);
		if (pList == NULL)
		{
			pList = m_tradeItems.Add(name);
		}
		if (pList != NULL)
		{
			m_tradeItems.SetCurrent(pList);
			status = S_OK;
		}
	}
	return status;
}

int Trader::GetTrade(LPCTSTR name, Trade & trade)
{
	ListOfTradeItems * pList = m_tradeItems.Find(name);
	if (pList != NULL)
	{
		trade = pList->trade;
		return S_OK;
	}
	else
	{		
		return E_FAIL;
	}
}

int Trader::GetCurrentTrade(Trade & trade)
{
	if (m_tradeItems.pCurrent != NULL)
	{
		trade = m_tradeItems.pCurrent->trade;
		return S_OK;
	}
	else
	{
		memset(&trade, 0, sizeof(trade));
		return E_FAIL;
	}
}

BOOL Trader::IsActiveTrade(LPCTSTR name)
{
	BOOL active = FALSE;
	ListOfTradeItems * pList = m_tradeItems.Find(name);
	if (pList != NULL)
	{
		if (pList->size())
			active = TRUE;
	}
	return active;
}

int Trader::OnAction(LPCTSTR name, Transaction * pTransaction)
{
	int status = EAGAIN;
	ListOfTradeItems * pList;
#if 0
	pList = m_tradeItems.GetCurrent();
#else
	pList = m_tradeItems.Find(name);
	if (pList == NULL)
		pList = m_tradeItems.Add(name);
#endif
	if (pList != NULL)
	{
		int add = 0;
		int remove = 0;
		int modify = 0;

		Transaction::Action taAction = pTransaction->action;
		Transaction::Status taStatus = pTransaction->status;

		if (taStatus == Transaction::STATUS_SENT)
		{
			return status;
		}

		if (Transaction::IsActionStop(taAction))
		{// Стоп-заявка
			if (Transaction::IsActive(taStatus))
				add = 1;
			else if (Transaction::IsCanceled(taStatus) || Transaction::IsExecuted(taStatus))
				remove = 1;
		}
		else
		{
			if (Transaction::IsActive(taStatus))
				add = 1;
			else if (Transaction::IsCanceled(taStatus))
				remove = 1;
			else if (Transaction::IsExecuted(taStatus))
			{// Заявка на покупку/продажу исполнена
				modify = 1;
			}
		}

		if (add)
		{
			TRACE("Add N %.0f\r\n", pTransaction->orderKey);
			pList->Add(pTransaction);
			status = S_OK;
		}
		else if (remove)
		{
			TRACE("Remove N %.0f\r\n", pTransaction->orderKey);
			pList->Remove(pTransaction);
			status = S_OK;
		}
		else if (modify)
		{
			TRACE("Modify N %.0f\r\n", pTransaction->orderKey);
			pList->Modify(pTransaction);
			status = S_OK;
		}
	} // if (pList != NULL)
	return status;
}

void Trader::ResetTrade(LPCTSTR name)
{
	TradeItems::iterator it;
	ListOfTradeItems * pList = m_tradeItems.Find(name, &it);
	if (pList)
	{
		m_tradeItems.Remove(it);
		if (pList == m_tradeItems.pCurrent)
			m_tradeItems.pCurrent = m_tradeItems.size() ? m_tradeItems.front() : NULL;
	}
}

void Trader::ResetTrade(Trade & trade) 
{ 
	memset(&trade, 0, sizeof(trade)); 
}

void Trader::ResetAllTrades()
{
	m_tradeItems.RemoveAll();
	m_tradeItems.pCurrent = NULL;
}

const TradeItem * Trader::FindItem(int flags, double N, int action, int state, int operation, double price, double quantity)
{
	const TradeItem * pItem = NULL;
	ListOfTradeItems * pList = m_tradeItems.GetCurrent();
	if (pList != NULL)
	{
		ListOfTradeItems::const_iterator & it = pList->it;//(pList->it == pList->end()) ? pList->begin() : pList->it;
		pItem = pList->FindItem(it, flags, N, action, state, operation, price, quantity);
	}
	return pItem;
}

void Trader::BeginFindItem()
{
	ListOfTradeItems * pList = m_tradeItems.GetCurrent();
	if (pList != NULL)
		pList->it = pList->begin();
}

void Trader::EndFindItem()
{
	ListOfTradeItems * pList = m_tradeItems.GetCurrent();
	if (pList != NULL)
		pList->it = pList->begin();
}

int Trader::FindItems(::std::list<TradeItem> & list, double price)
{
	int n = 0;
	ListOfTradeItems * pList = m_tradeItems.GetCurrent();
	if (pList != NULL)
	{
		const TradeItem * pItem = NULL;
		ListOfTradeItems::const_iterator it;
		for (it = pList->begin(); it != pList->end(); it++)
		{
			pItem = &(*it);
			if ((float)pItem->price != (float)price)
				continue;
			list.push_back(*pItem);
		} // for (it)
	}
	n = static_cast<int>(list.size());
	return n;
}

int Trader::FindItems(::std::list<TradeItem> & list, int flags, int operation, double price)
{
	int n = 0;
	ListOfTradeItems * pList = m_tradeItems.GetCurrent();
	if (pList != NULL)
	{
		const TradeItem * pItem = NULL;
		ListOfTradeItems::const_iterator it;
		for (it = pList->begin(); it != pList->end(); it++)
		{
			pItem = &(*it);
			if (flags != F_SEARCH_ANY)
			{
				if (flags & F_SEARCH_PRICE)
					if (pItem->price != price)
						continue;
				if (flags & F_SEARCH_OPERATION)
					if (pItem->operation != operation)
						continue;
			}
			list.push_back(*pItem);
		} // for (it)
	}
	n = static_cast<int>(list.size());
	return n;
}
