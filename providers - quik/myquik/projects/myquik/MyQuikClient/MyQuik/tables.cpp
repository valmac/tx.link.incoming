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
 *  tables.cpp
 */

#include "stdafx.h"
#include "tables.h"
#include <std.h>
#include "common.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


//
// class ListOfUserValues
//

ListOfUserValues::ListOfUserValues()
{
	this->current = 0;
}

int ListOfUserValues::Add(double value)
{
	if (value > 0)
	{
		int found = 0;
		ListOfDoubles::iterator it;
		for (it = begin(); it != end(); it++)
		{
			if (*it == value)
			{
				erase(it);
				++found;
				break;
			}
		} // for (it)
		if (size() + 1 >= MAX_SIZE)
			pop_back(); // размер списка фиксирован;
		push_front(value);
	}
	this->current = value;

	return 0;
}

double ListOfUserValues::GetCurrentValue() const 
{
	double current = 0;
	if (this->NotEmpty())
	{
#if 0
		current = front();
#else
		current = this->current;
#endif
	}
	return current;
}

//
// class QuoteTable 
//

QuoteTable::QuoteTable()
{
	this->Init(NULL, F_FORCE);
	
}

QuoteTable::QuoteTable(LPCTSTR name)
{	
	this->Init(name, F_FORCE);
}

QuoteTable::~QuoteTable()
{
}

void QuoteTable::Properties::Initialize(LPCTSTR name, LPCTSTR seccode, LPCTSTR classcode, LPCTSTR clientcode, LPCTSTR account, int nd)
{
	if (name != NULL)
		SAFE_TSTRCPY(this->name, name);
	else
		memset(this->name, 0, sizeof(this->name));
	if (classcode != NULL)
		SAFE_TSTRCPY(this->classcode, classcode);
	else
		memset(this->classcode, 0, sizeof(this->classcode));
	if (seccode != NULL)
		SAFE_TSTRCPY(this->seccode, seccode);
	else
		memset(this->seccode, 0, sizeof(this->seccode));
	if (clientcode != NULL)
		SAFE_TSTRCPY(this->clientcode, clientcode);
	else
		memset(this->clientcode, 0, sizeof(this->clientcode));
	if (account != NULL)
		SAFE_TSTRCPY(this->account, account);
	else
		memset(this->account, 0, sizeof(this->account));

	this->price.nd = nd;
	this->price.step = Mul_0_1(1, this->price.nd);

	this->trading.quantity = 1;
	this->trading.spread = 0.00;

	this->trading.autos.stop.absolute = 0;
	this->trading.autos.stop.relative = 1.00;
	this->trading.autos.stop.isRelative = TRUE;
	this->trading.autos.stop.slippage = FALSE;
	this->trading.autos.profit.absolute = 0;
	this->trading.autos.profit.relative = 1.00;
	this->trading.autos.profit.isRelative = TRUE;
	this->trading.autos.profit.slippage = FALSE;
}

void QuoteTable::Init(LPCTSTR name, int flags)
{
	theBestBuyPrice = theBestSellPrice = 0;
	prevChangePrice = prevPrice = price = 0;
	demand = supply = 0;
	priceMin = priceMax = 0;
	volumeMin = volumeMax = 0;
	percent = 0;
	priceUser = 0;

	this->pAccount = NULL;

	for (int i = 0; i < SIZEOF_ARRAY(data.arrays); i++)
	{
		Array * pArray = GetArray(i);
		pArray->id = i; // ?
		if (flags & F_FORCE)
			pArray->Clear();
	} // for (i)
}

int QuoteTable::Create(LPCTSTR name, LPCTSTR seccode, LPCTSTR classcode, LPCTSTR clientcode, LPCTSTR account, int nd, int flags)
{
	int status = S_OK;
	this->Init(name, flags);
#if 0
	if (seccode)
#endif
		this->properties.Initialize(name, seccode, classcode, clientcode, account, nd);
	return status;
}

void QuoteTable::SetProperties(const Properties * pProperties) 
{ 
	if (pProperties != NULL)
		properties = *pProperties; 
	else
		properties.Initialize();
}

void QuoteTable::Clear(int full)
{
	//theBestBuyPrice = theBestSellPrice = 0;
	for (int i = 0; i < SIZEOF_ARRAY(data.arrays); i++)
	{
		Array * pArray = GetArray(i);
		pArray->Clear(full);
	}
}

void QuoteTable::SetCurrentPrice(double price)
{
	if (this->prevPrice != price)
		this->prevChangePrice = this->prevPrice;
	this->prevPrice = this->price; 
	this->price = price; 
}

QuoteTable::Array * QuoteTable::GetArray(int i)
{
	return &data.arrays[i];
}

const QuoteTable::Array * QuoteTable::GetArray(int i) const
{
	return &data.arrays[i];
}

QuoteTable & QuoteTable::operator = (const QuoteTable & src)
{
	Copy(*this, src);
	return *this;
}

//
// class Tables
//

Tables::Tables()
{
	pCurrent = NULL;
}

Tables::~Tables()
{
	ListOfTables::iterator it;
	for (it = begin(); it != end(); it++)
	{
		delete (*it);
	}

}

QuoteTable * Tables::FindTable(LPCTSTR name, int flags)
{
	QuoteTable * pTable;
	ListOfTables::iterator it;
	for (it = begin(); it != end(); it++)
	{
		LPCTSTR pStr;
		pTable = *(it);			
		if (flags & PROPERTY_SECCODE)
			pStr = pTable->properties.seccode;
		else if (flags & PROPERTY_ACCOUNT)
			pStr = pTable->properties.account;
		else
			pStr = pTable->properties.name;
		if (0 == lstrcmp(name, pStr))
		{
			return pTable;
		}
	}
	return NULL;
}

QuoteTable * Tables::FindTable(LPCTSTR name, LPCTSTR seccode, LPCTSTR classcode, LPCTSTR clientcode, LPCTSTR account)
{
	QuoteTable * pTable;
	ListOfTables::iterator it;
	for (it = begin(); it != end(); it++)
	{
		pTable = *(it);
		if (name)
		{
			if (0 != lstrcmp(name, pTable->properties.name))
				continue;
			else 
				return pTable;
		}
		if (seccode && 0 != lstrcmp(seccode, pTable->properties.seccode))
			continue;
		if (classcode && 0 != lstrcmp(classcode, pTable->properties.classcode))
			continue;
		if (clientcode && 0 != lstrcmp(clientcode, pTable->properties.clientcode))
			continue;
		if (account && 0 != lstrcmp(account, pTable->properties.account))
			continue;
		return pTable;
	} // for (it)
	return NULL;
}

QuoteTable * Tables::CreateTable(LPCTSTR name, const QuoteTable::Properties * pProperties)
{
	QuoteTable * pTable = new QuoteTable(name);
	if (pTable != NULL)
	{
		pTable->properties = *pProperties;
		this->push_back(pTable);
	}
	return pTable;
}

void Tables::UpdateTables(const Account * pAccount)
{
	QuoteTable * pTable;
	ListOfTables::iterator it;
	for (it = begin(); it != end(); it++)
	{
		pTable = *(it);			
		if (0 == lstrcmp(pTable->GetProperties()->account, pAccount->GetName()))
		{
			pTable->SetAccount(pAccount);
		}
	}
}

void Tables::UpdateTables(const ListOfTableProperties * pListOfProperties)
{
	QuoteTable * pTable;
	ListOfTables::iterator it;
	for (it = begin(); it != end(); it++)
	{
		pTable = *(it);	
		LPCTSTR name = pTable->GetProperties()->name;
		ListOfTableProperties::const_iterator propIt;
		for (propIt = pListOfProperties->begin(); propIt != pListOfProperties->end(); propIt++)
		{
			const QuoteTable::Properties & properties = *(propIt);
			if (0 == lstrcmp(name, properties.name))
			{
				pTable->SetProperties(&properties);
				break;
			}
		} // for (propIt)
	} // for (it)
}

QuoteTable * Tables::GetFirstTable(Tables::Iterator & it)
{
	QuoteTable * pTable = NULL;
	if (size() != 0)
	{
		it = begin();
		pTable = *it;
	}
	return pTable;
}

QuoteTable * Tables::GetNextTable(Tables::Iterator & it, int cycle)
{
	QuoteTable * pTable = NULL;
	if (size() != 0)
	{
		++it;
		if (it == end())
		{
			if (cycle)
				it = begin();
			else
				return NULL;
		}
		pTable = *it;
	}
	return pTable;
}

QuoteTable * Tables::GetPreviosTable(Tables::Iterator & it, int cycle)
{
	QuoteTable * pTable = NULL;
	if (size() != 0)
	{
		if (it == begin())
		{
			if (cycle)
				it = end();
			else
				return NULL;
		}
		--it;
		pTable = *it;
	}
	return pTable;
}

QuoteTable * Tables::GetFirstTable()
{
	Tables::Iterator it;
	return GetFirstTable(it);
}

QuoteTable * Tables::GetNextTable(int cycle)
{
	QuoteTable * pTable = NULL;
	ListOfTables::iterator it;
	for (it = begin(); it != end(); it++)
	{
		if ((*it) == this->pCurrent)
		{
			pTable = GetNextTable(it, cycle);
			break;
		}
	}
	return pTable;
}

QuoteTable * Tables::GetPreviosTable(int cycle)
{
	QuoteTable * pTable = NULL;
	ListOfTables::iterator it;
	for (it = begin(); it != end(); it++)
	{
		if ((*it) == this->pCurrent)
		{
			pTable = GetPreviosTable(it, cycle);
			break;
		}
	}
	return pTable;
}


//
// QuoteTable::Array
//

void QuoteTable::Array::Create(int size) 
{
	items.resize(size); 
	this->count = 0;
}

void QuoteTable::Array::Clear(int full) 
{ 
	if (full)
	{
		for (int i = 0; i < this->count; i++)
			items[i].Clear();
	}
	this->count = 0; 
}

void QuoteTable::Array::ClearItem(int line)
{
#if 1
	memset(&items[line], 0, sizeof(items[0]));
#else
	items[line].Clear();
#endif
}

QuoteTable::Array & QuoteTable::Array::operator = (const QuoteTable::Array & arr)
{
	int count = arr.GetCapacity();
	if (this->GetCapacity() < count)
		if (! this->IsEmpty())
			this->items.clear();
	if (this->IsEmpty())
		this->Create(count);
	this->Clear();

	Items::const_iterator src = arr.items.begin();
	Items::iterator dst = this->items.begin();
	for (int i = 0; i < count; i++)
	{
		*dst++ = *src++;
		//this->items
	} // for (i)

	this->count = arr.count;
	this->id = arr.id;

	return *this;
}

//
// ListOfTableProperties
//

int ListOfTableProperties::Add(const QuoteTable::Properties * pProperties)
{
	int ret = S_OK;
	this->push_back(*pProperties);
	return ret;
}

int ListOfTableProperties::Update(LPCTSTR name, const QuoteTable::Properties * pProperties)
{
	int ret = E_INVALIDARG;
	ListOfTableProperties::iterator it;
	for (it = this->begin(); it != this->end(); it++)
	{
		if (0 == lstrcmp(name, (*it).name))
		{
			*it = *pProperties;
			ret = S_OK;
			break;
		}
	} // for (it)

	if (ret != S_OK)
	{// Такого элемента нет, поэтому добавляем его в список:
		this->Add(pProperties);
	}

	return ret;
}

int ListOfTableProperties::Remove(LPCTSTR name)
{
	int ret = E_INVALIDARG;
	ListOfTableProperties::iterator it;
	for (it = this->begin(); it != this->end(); )
	{
		if (0 == lstrcmp(name, (*it).name))
		{
			this->erase(it);
			ret = S_OK;
			// Удаляем все элементы с таким названием!
			it = this->begin();
			continue;
		}
		it++;
	} // for (it)
	return ret;
}

int ListOfTableProperties::Clear()
{
	this->clear();
	return S_OK;
}

int ListOfTableProperties::UpdateProperties(LPCTSTR name, const QuoteTable::Properties * pProperties, int flags)
{
	int n = 0;
	if (flags & F_ADD)
		Add(pProperties);
	else if (flags & F_UPDATE)
		Update(name, pProperties);
	else if (flags & F_REMOVE)
		Remove(name);
	else if (flags & F_CLEAR)
		Clear();
	return n;
}

const QuoteTable::Properties * ListOfTableProperties::GetItem(LPCTSTR name, int flags)
{
	const QuoteTable::Properties * pProperties = NULL;
	ListOfTableProperties::const_iterator it;
	for (it = this->begin(); it != this->end(); it++)
	{
		LPCTSTR pStr = NULL;
		if (flags & PROPERTY_NAME)
			pStr = (*it).name;
		else if (flags & PROPERTY_SECCODE)
			pStr = (*it).seccode;
		else if (flags & PROPERTY_CLIENTCODE)
			pStr = (*it).clientcode;
		if (0 == lstrcmp(name, pStr))
		{
			pProperties = &(*it);
			break;
		}
	} // for (it)
#if 0
	if (pProperties == NULL && !(flags & PROPERTY_EXISTING))
	{
		LPCTSTR pName = name;
		LPCTSTR pSeccode = NULL;
		LPCTSTR pClientcode = NULL;
		QuoteTable::Properties properties;
		if (flags & PROPERTY_NAME)
			pName = name;
		else if (flags & PROPERTY_SECCODE)
			pSeccode = name;
		else if (flags & PROPERTY_CLIENTCODE)
			pClientcode = name;
		properties.Initialize(pName, pSeccode, NULL, pClientcode, NULL);
		this->properties = properties;
		pProperties = &this->properties;
	} // if (pProperties == NULL)
#endif
	return pProperties;
}

const QuoteTable::Properties * ListOfTableProperties::GetItem(
	LPCTSTR name, LPCTSTR seccode, LPCTSTR classcode, LPCTSTR clientcode, LPCTSTR account)
{	
	ListOfTableProperties::const_iterator it;
	for (it = this->begin(); it != this->end(); it++)
	{
		const QuoteTable::Properties * pProperties = &(*it);
		if (name)
		{
			if (0 != lstrcmp(name, pProperties->name))
				continue;
			else 
				return pProperties;
		}
		if (seccode && 0 != lstrcmp(seccode, pProperties->seccode))
			continue;
		if (classcode && 0 != lstrcmp(classcode, pProperties->classcode))
			continue;
		if (clientcode && 0 != lstrcmp(clientcode, pProperties->clientcode))
			continue;
		if (account && 0 != lstrcmp(account, pProperties->account))
			continue;
		return pProperties;
	} // for (it)

	return NULL;
}

const QuoteTable::Properties * ListOfTableProperties::GetCurrentItem()
{
	return this->pCurrent;
}


void Copy(QuoteTable & dst, const QuoteTable & src)
{
	for (int i = 0; i < SIZEOF_ARRAY(dst.data.arrays); i++)
		dst.data.arrays[i] = src.data.arrays[i];
	dst.properties = src.properties;
	dst.pAccount = src.pAccount;
	CopyPrices(dst, src);
	CopyVolumes(dst, src);
}

void CopyPrices(QuoteTable & dst, const QuoteTable & src)
{
	dst.theBestBuyPrice = src.theBestBuyPrice;
	dst.theBestSellPrice = src.theBestSellPrice;
	dst.price = src.price;
	dst.prevPrice = src.prevPrice;
	dst.prevChangePrice = src.prevChangePrice;
	dst.demand = src.demand;
	dst.supply = src.supply;
	dst.percent = src.percent;
	dst.priceMin = src.priceMin;
	dst.priceMax = src.priceMax;
	dst.priceBid = src.priceBid;
	dst.priceOffer = src.priceOffer;
	dst.priceUser = src.priceUser;
}

void CopyVolumes(QuoteTable & dst, const QuoteTable & src)
{
	dst.volumeMin = src.volumeMin;
	dst.volumeMax = src.volumeMax;
}
