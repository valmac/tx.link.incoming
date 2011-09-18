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
 *  history.h
 */

#pragma once

#include <list>
#include "transactions.h"

class History
{	
public:
	typedef ::std::list<DWORD> ListOfDates;

	enum {
		F_ADD = 0x1,
		F_REMOVE = 0x2,
		F_UPDATE = 0x4,
		F_CLEAR = 0x8,
	};

	enum {
		F_REMOVE_DEFAULT = 0x0,
		F_REMOVE_BY_FILTER = 0x1,
		F_REMOVE_ALL = 0x2,
	};

public:
	History(Transactions & transactions);
	~History();

	int Add(const Transaction * pTa);
	int Update(const Transaction * pTa);
	int Remove(const Transaction * pTa);

	BOOL IsEmpty() const;
	BOOL NotEmpty() const;

	int GetNrDays() const;
	int GetNrTransactions() const;

	Transaction * FindTransaction(DWORD id, double num, DWORD line, DWORD dwDate = 0);
	Transaction * FindTransactionById(DWORD id);
	Transaction * FindTransactionByOrderNumber(double num, DWORD line = 0, DWORD dwDate = 0);
	Transaction * FindTransactionByOrderNumber2(double num, DWORD line, DWORD dwDate = 0);
	Transaction * FindTransactionByOrderNumber2(double num, DWORD line, Transactions::Iterator & its, DWORD dwDate = 0);
	Transaction * FindTransactionByDate(SYSTEMTIME & stime, Transactions::Iterator & its);
	Transaction * FindTransactionByDate(SYSTEMTIME & stime1, SYSTEMTIME & stime2, Transactions::Iterator & its);
	Transaction * FindTransactionByDate(DWORD date, Transactions::Iterator & its);

	Transaction * FindFirstTransaction(Transactions::Iterator & its);
	Transaction * FindNextTransaction(Transactions::Iterator & its, BOOL autoIncList = TRUE);

	Transaction * FindFirstDate(Transactions::Iterator & its);
	Transaction * FindNextDate(Transactions::Iterator & its);

public:
	void SetModified(DWORD date);
	void SetModified(const Transaction * pTa);

	BOOL IsModified() const;
	BOOL IsNotModified() const;

	int FindModifiedDate(DWORD date, int remove = 0);

	int FindFirstModifiedDate(History::ListOfDates::iterator & it, DWORD * pDate);
	int FindNextModifiedDate(History::ListOfDates::iterator & it, DWORD * pDate);

	void RemoveAt(History::ListOfDates::iterator & it);

protected:
	Transactions & m_transactions;
	ListOfDates m_listOfDates;

}; // class History
