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
 *  accounts.cpp
 */

#include "stdafx.h"
#include "accounts.h"
#include <std.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

//
// class Account 
//

Account::Account()
{
	limit = limitPrev = 0;
	profit = 0;
}

Account::Account(LPCTSTR name)
{
	SAFE_TSTRCPY(this->name, name);

	limit = limitPrev = 0;
	profit = 0;
}

Account::~Account()
{
}

void Account::Set(double limit, double limitPrev, double profit)
{
	//if (limit != 0)
		this->limit = limit;
	//if (limitPrev != 0)
		this->limitPrev = limitPrev;

	this->profit = profit;
}

void Account::Get(double & limit, double & limitPrev, double & profit) const
{
	limit = this->limit;
	limitPrev = this->limitPrev;
	profit = this->profit;
}

//
// class Accounts
//

Accounts::Accounts()
{
	pCurrent = NULL;
}

Accounts::~Accounts()
{
	ListOfAccounts::iterator it;
	for (it = begin(); it != end(); it++)
	{
		delete (*it);
	}

}

Account * Accounts::Find(LPCTSTR name, int flags)
{
	Account * pAccount;
	ListOfAccounts::iterator it;
	for (it = begin(); it != end(); it++)
	{
		LPCTSTR pStr;
		pAccount = *(it);			
		//if (flags & PROPERTY_SECCODE)
		//	pStr = pAccount->properties.seccode;
		//else if (flags & PROPERTY_ACCOUNT)
		//	pStr = pAccount->properties.account;
		//else
			pStr = pAccount->name;
		if (0 == lstrcmp(name, pStr))
		{
			return pAccount;
		}
	}
	return NULL;
}

Account * Accounts::Create(LPCTSTR name)
{
	Account * pAccount = new Account(name);
	if (pAccount != NULL)
	{
		this->push_back(pAccount);
	}
	return pAccount;
}


