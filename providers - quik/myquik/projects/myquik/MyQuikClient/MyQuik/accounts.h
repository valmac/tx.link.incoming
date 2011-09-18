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
 *  accounts.h
 */

#pragma once

#include <list>
#include <std.h>

class Account 
{
	friend class Accounts;
public:
	Account();
	Account(LPCTSTR name);
	~Account();

	LPCTSTR GetName() const { return this->name; }

	void Set(double limit, double limitPrev, double profit);
	void Get(double & limit, double & limitPrev, double & profit) const;

protected:
	TSTRING_SMALL(name);
	double limit;
	double limitPrev;
	double profit;
}; // class Account

typedef std::list<Account*> ListOfAccounts;
class Accounts : public ListOfAccounts
{
public:
	Accounts();
	~Accounts();

	Account * Find(LPCTSTR name, int flags);
	Account * Create(LPCTSTR name);

	int GetCount() const { return size(); }

	const Account * GetCurrent() const { return pCurrent; }
	Account * GetCurrent() { return pCurrent; }
	void SetCurrent(Account * pCurrent) { this->pCurrent = pCurrent; }

protected:
	Account * pCurrent;
};


