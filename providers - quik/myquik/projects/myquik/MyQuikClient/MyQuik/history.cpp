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
 *  history.cpp
 */

#include "stdafx.h"
#include "history.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


History::History(Transactions & transactions) : m_transactions(transactions)
{
}

History::~History()
{
}

int History::Add(const Transaction * pTa)
{
	return m_transactions.AddCopy((Transaction*)pTa);
}

int History::Update(const Transaction * pTransaction)
{
	int status = 0;
	int found = 0;
	Transaction * pTa = NULL;
	Transactions::Iterator it;

	pTa = this->FindTransactionByOrderNumber2(pTransaction->orderKey, pTransaction->line);
	if (pTa != NULL)
		found = 1;

	if (found)
	{
		*pTa = *pTransaction;
	}
	else
		status = -1;
	return status;
}

int History::Remove(const Transaction * pTransaction)
{
	int status = 0;
	Transaction * pTa = NULL;
	Transactions::Iterator it;
	pTa = this->FindTransactionByOrderNumber2(pTransaction->orderKey, pTransaction->line, it);
	if (pTa != NULL)
	{
		//delete pTa;
		//m_transactions.erase(it);

		m_transactions.Remove(it);
	}
	else
		status = -1;
	return status;
}

BOOL History::IsEmpty() const 
{ 
	return (m_transactions.size() == 0); 
}

BOOL History::NotEmpty() const 
{ 
	return (! IsEmpty());
}

int History::GetNrDays() const
{
	return (int)m_transactions.size();
}

int History::GetNrTransactions() const
{
	return m_transactions.GetNrItems();
}

void History::SetModified(DWORD date)
{
	int found = 0;
	ListOfDates::iterator it;
	for (it = m_listOfDates.begin(); it != m_listOfDates.end(); it++)
	{
		DWORD d = *it;
		if (d == date)
		{
			found = 1;
			break;
		}
	} // for (it)
	if (! found)
	{
		m_listOfDates.push_back(date);
	}
}

void History::SetModified(const Transaction * pTa)
{
	DWORD date = GetTimeStampDate(pTa);
	SetModified(date);
}

BOOL History::IsModified() const
{
	return (m_listOfDates.size() > 0);
}

BOOL History::IsNotModified() const
{
	return (! IsModified());
}

int History::FindModifiedDate(DWORD date, int remove)
{
	int status = -1;
	int found = 0;
	ListOfDates::iterator it;
	for (it = m_listOfDates.begin(); it != m_listOfDates.end(); it++)
	{
		DWORD d = *it;
		if (d == date)
		{
			found = 1;
			break;
		}
	} // for (it)
	if (found)
	{
		status = S_OK;
		if (remove)
			m_listOfDates.erase(it);
	}
	return status;
}

Transaction * History::FindTransactionById(DWORD id)
{
	return m_transactions.FindTransactionById(id);
}

Transaction * History::FindTransactionByOrderNumber(double num, DWORD line, DWORD dwDate)
{
	return m_transactions.FindTransactionByOrderNumber(num, line, dwDate);
}

Transaction * History::FindTransactionByOrderNumber2(double num, DWORD line, DWORD dwDate)
{
	return m_transactions.FindTransactionByOrderNumber2(num, line, dwDate);
}

Transaction * History::FindTransactionByOrderNumber2(double num, DWORD line, Transactions::Iterator & its, DWORD dwDate)
{
	return m_transactions.FindTransactionByOrderNumber2(num, line, its, dwDate);
}

Transaction * History::FindTransaction(DWORD id, double num, DWORD line, DWORD dwDate)
{
	return m_transactions.FindTransaction(id, num, line, dwDate);
}

Transaction * History::FindTransactionByDate(SYSTEMTIME & stime, Transactions::Iterator & its)
{
	return m_transactions.FindTransactionByDate(stime, its);
}

Transaction * History::FindTransactionByDate(SYSTEMTIME & stime1, SYSTEMTIME & stime2, Transactions::Iterator & its)
{
	return m_transactions.FindTransactionByDate(stime1, stime2, its);
}

Transaction * History::FindTransactionByDate(DWORD date, Transactions::Iterator & its)
{
	return m_transactions.FindTransactionByDate(date, its);
}

Transaction * History::FindFirstTransaction(Transactions::Iterator & its)
{
	return m_transactions.FindFirstTransaction(its);
}

Transaction * History::FindNextTransaction(Transactions::Iterator & its, BOOL autoIncList)
{
	return m_transactions.FindNextTransaction(its, autoIncList);
}

Transaction * History::FindFirstDate(Transactions::Iterator & its)
{
	return m_transactions.FindFirstDate(its);
}

Transaction * History::FindNextDate(Transactions::Iterator & its)
{
	return m_transactions.FindNextDate(its);
}

int History::FindFirstModifiedDate(History::ListOfDates::iterator & it, DWORD * pDate)
{
	if (m_listOfDates.size())
	{
		it = m_listOfDates.begin();
		if (pDate != NULL)
			*pDate = *it;
		return S_OK;
	}
	else
		return -1;
}

int History::FindNextModifiedDate(History::ListOfDates::iterator & it, DWORD * pDate)
{
	if (it != m_listOfDates.end())
	{		
		if (pDate != NULL)
			*pDate = *it;
		++it;
		return S_OK;
	}
	else
		return -1;
}

void History::RemoveAt(History::ListOfDates::iterator & it)
{
	m_listOfDates.erase(it);
}
