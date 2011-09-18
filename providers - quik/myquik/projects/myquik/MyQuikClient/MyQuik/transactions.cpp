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
 *  transactions.cpp
 */

#include "stdafx.h"
#include "transactions.h"
#include <std.h>
#include <str.h>
#include "MyQuik.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

//
// class Transaction 
//
Transaction::Transaction()
{
}

Transaction::Answer::Answer(long nTransactionResult, long nTransactionExtendedErrorCode, long nTransactionReplyCode,
							DWORD dwTransId, double dOrderNum, LPCSTR lpcstrTransactionReplyMessage, int line, int callback)
{
	this->result = nTransactionResult;
	this->extendedErrorCode = nTransactionExtendedErrorCode;
	this->replyCode = nTransactionReplyCode;
	this->id = dwTransId;
	this->orderNum = dOrderNum;
	if (lpcstrTransactionReplyMessage != NULL)
		SAFE_STRCPY(this->replyMessage, lpcstrTransactionReplyMessage);
	else
		this->replyMessage[0] = '\0';
	this->ext.callback = callback;
	this->ext.line = line;
}

LPCTSTR Transaction::ActionToString(Transaction::Action action)
{
	static LPCTSTR strs[] = {
		TEXT("NEW_ORDER"), // новая заявка, 
		TEXT("NEW_NEG_DEAL"), // новая заявка на внебиржевую сделку, 
		TEXT("NEW_REPO_NEG_DEAL"), // новая заявка на сделку РЕПО, 
		TEXT("NEW_EXT_REPO_NEG_DEAL"), // новая заявка на сделку модифицированного РЕПО (РЕПО-М), 
		TEXT("NEW_STOP_ORDER"), // новая стоп-заявка, 
		TEXT("KILL_ORDER"), // снять заявку, 
		TEXT("KILL_NEG_DEAL"), // снять заявку на внебиржевую сделку или заявку на сделку РЕПО, 
		TEXT("KILL_STOP_ORDER"), // снять стоп-заявку, 
		TEXT("KILL_ALL_ORDERS"), // снять все заявки из торговой системы, 
		TEXT("KILL_ALL_STOP_ORDERS"), // снять все стоп-заявки, 
		TEXT("KILL_ALL_NEG_DEALS"), // снять все заявки на внебиржевые сделки и заявки на сделки РЕПО, 
		TEXT("KILL_ALL_FUTURES_ORDERS"), // снять все заявки на рынке FORTS, 
		TEXT("MOVE_ORDERS"), // переставить заявки на рынке FORTS, 
		TEXT("NEW_QUOTE"), // новая безадресная заявка, 
		TEXT("KILL_QUOTE"), // снять безадресную заявку, 
		TEXT("NEW_REPORT"), // новый отчет о подтверждении транзакций, 
		TEXT("SET_FUT_LIMIT"), // новое ограничение по фьючерсному счету. 
	};
	return strs[action];
}

LPCTSTR Transaction::ActionToStringRus(Transaction::Action action)
{
	static LPCTSTR strs[] = {
		TEXT("Новая заявка"),
		TEXT("Заявка на внебиржевую сделку"),
		TEXT("Заявка на сделку РЕПО"),
		TEXT("Заявка на сделку модифицированного РЕПО (РЕПО-М)"), 
		TEXT("Новая стоп-заявка"),
		TEXT("Отмена заявки"),
		TEXT("Отмена заявки на внебиржевую сделку или заявки на сделку РЕПО"), 
		TEXT("Отмена стоп-заявки"), 
		TEXT("Снять все заявки из торговой системы"),
		TEXT("Отмена всех стоп-заявок"), 
		TEXT("Отмена всех заявок на внебиржевые сделки и заявки на сделки РЕПО"), 
		TEXT("Отмена всех заявок на рынке FORTS"), 
		TEXT("Переставить заявки на рынке FORTS"), 
		TEXT("Безадресная заявка"), 
		TEXT("Отмена безадресной заявки"), 
		TEXT("Отчет о подтверждении транзакций"), 
		TEXT("Ограничение по фьючерсному счету"), 
	};
	return strs[action];
}

LPCTSTR Transaction::StatusToString(Transaction::Status status)
{
	static LPCTSTR strs[] = {
		TEXT("Отправлена"),
		TEXT("Получена сервером"),
		TEXT("Ошибка при передаче"),
		TEXT("Исполнена"),
		TEXT("Отвергнута торговой системой"),
		TEXT("Не прошла проверку сервера"),
		TEXT("Не прошла проверку лимитов"),
		TEXT("Подтверждена"),
		TEXT("Не подтверждена"),
		TEXT("Снята менеджером"),
		TEXT("Не поддерживается торговой системой"),
		TEXT("Неверная электронная подпись"),

		TEXT("Unknown"),
		TEXT("Активна"),
		TEXT("Исполнена"),
		TEXT("Снята"),
	};
	return strs[status];
}

BOOL Transaction::IsNewOrder(Transaction::Action action)
{
	BOOL ret;
	if (action == Transaction::NEW_ORDER ||
		action == Transaction::NEW_NEG_DEAL ||
		action == Transaction::NEW_REPO_NEG_DEAL ||
		action == Transaction::NEW_EXT_REPO_NEG_DEAL ||
		action == Transaction::NEW_STOP_ORDER ||
		action == Transaction::NEW_QUOTE ||
		action == Transaction::NEW_REPORT ||
		action == Transaction::SET_FUT_LIMIT )
	{
		ret = TRUE;
	}
	else
		ret = FALSE;
	return ret;
}

BOOL Transaction::IsActionCancel(Transaction::Action action)
{
	BOOL ret;
	if (action == Transaction::KILL_ORDER ||
		action == Transaction::KILL_NEG_DEAL ||
		action == Transaction::KILL_STOP_ORDER )
	{
		ret = TRUE;
	}
	else
		ret = FALSE;
	return ret;
}

BOOL Transaction::IsActionStop(Transaction::Action action)
{
	BOOL ret;
	if (action == Transaction::NEW_STOP_ORDER)
	{
		ret = TRUE;
	}
	else
		ret = FALSE;
	return ret;
}

BOOL Transaction::IsActive(Transaction::Status status)
{
	return (status == Transaction::STATUS_EX_BID_ACTIVE);
}

BOOL Transaction::IsExecuted(Transaction::Status status)
{
	return (status == Transaction::STATUS_EX_BID_EXECUTED || status == Transaction::STATUS_EXECUTED);
}

BOOL Transaction::IsCanceled(Transaction::Status status)
{
	return (status == Transaction::STATUS_EX_BID_CANCELED);
}

BOOL Transaction::IsFailed(Transaction::Status status)
{
	return (status == Transaction::STATUS_FAILED_CONNECTION ||
		status == Transaction::STATUS_REJECTED ||
		status == Transaction::STATUS_WRONG_PARAMETERS ||
		status == Transaction::STATUS_WRONG_LIMITS ||
		status == Transaction::STATUS_NOT_CONFIRMED ||
		status == Transaction::STATUS_NOT_SUPPORTED ||
		status == Transaction::STATUS_WRONG_KEYS
		);
}

void Transaction::SetPending(BOOL set) 
{ 
	const DWORD pending = Transaction::F_PENDING;
	if (set)
		this->flags |= pending; 
	else
		this->flags &= ~pending; 
}

//
// class Transactions 
//
Transactions::Transactions()
{
}

Transactions::~Transactions()
{
	Clear();
}

Transaction * Transactions::Create(Transaction * pTransaction, LPCTSTR name, WORD id, const SYSTEMTIME * pTime)
{
	Transaction * pNewTransaction = NULL;
	pNewTransaction = new Transaction;
	if (pNewTransaction != NULL)
	{
		if (pTransaction != NULL)
			*pNewTransaction = *pTransaction;
		else
		{
			memset(pNewTransaction, 0, sizeof(*pNewTransaction));
			pNewTransaction->status = Transaction::STATUS_SENT;
		}
		if (name != NULL)
			SAFE_TSTRCPY(pNewTransaction->strName, name);
		else
			memset(pNewTransaction->strName, 0, sizeof(pNewTransaction->strName));
		if (pTime != NULL)
			pNewTransaction->time = *pTime;
		pNewTransaction->id = id;
		pNewTransaction->flags = Transaction::F_ZERO;
#if 0
		// Добавляем в список:
		this->push_back(pNewTransaction);
#else
		// Добавляем в функции TransCtrl::CreateNewTransaction!
#endif
	}
	return pNewTransaction;
}

void Transactions::Clear()
{
	size_t count = size();
	for (size_t i = 0; i < count; i++)
	{
		iterator listIt = begin();
		ListOfTransactions * pList = *listIt;
		size_t count = pList->size();
		for (size_t j = 0; j < count; j++)
		{
			ListOfTransactions::iterator it = pList->begin();
			delete (*it);
			pList->erase(it);
		} // for (it)
		delete (*listIt);
		erase(listIt);
	} // for (listIt)
}

int Transactions::GetDates(::std::list<DWORD> * pDates)
{
	int n = 0;
	if (pDates)
	{	
		for (iterator it = begin(); it != end(); it++)
		{
			ListOfTransactions * pList = *it;
			DWORD date = GetTimeStampDate(pList->front());
			pDates->push_back(date);
		} // for (listIt)
	}
	return n;
}

int Transactions::Add(Transaction * pTa, Transactions::Iterator * pIts)
{
	int status = -1;
	if (pTa != NULL)
	{
		int add = 0;
		DWORD date = GetTimeStampDate(pTa);
		for (iterator it = begin(); it != end(); it++)
		{
			ListOfTransactions * pList = *it;
			if (pList->size())
			{
				DWORD d = GetTimeStampDate(pList->front());
				if (d == date)
				{
					pList->push_back(pTa);
					++add;
					if (pIts)
						pIts->list = it;
					break;
				}
			}
		} // for (it)
		if (! add)
		{
			ListOfTransactions * pList = new ListOfTransactions();
			if (pList != NULL)
			{
#if 1
				this->push_back(pList);
#else
				this->push_front(pList);
#endif
				pList->push_back(pTa);
				++add;
				//if (pIts)
				//	pIts->list = back();
			}
		}
		if (add)
		{
			//if (pIts)
			//	pIts->ta = it;
			status = S_OK;
		}
	}
	return status;
}

int Transactions::AddCopy(Transaction * pTransaction)
{
	Transaction * pTa = new Transaction(*pTransaction);
	return Add(pTa);
}

int Transactions::Remove(Transactions::Iterator its)
{
	ListOfTransactions * pList = *its.list;
	Transaction * pTa = *its.ta;

	delete (pTa);
	pList->erase(its.ta);

	if (pList->size() == 0)
	{
		delete pList;
		this->erase(its.list);
	}

	return 0;
}

int Transactions::GetNrItems() const
{
	size_t n = 0;
	for (ListOfDayTransactions::const_iterator listIt = begin(); listIt != end(); listIt++)
	{
		const ListOfTransactions * pList = *listIt;
		n += pList->size();
	} // for (listIt)	
	return static_cast<int>(n);
}

Transaction * Transactions::FindTransactionById(DWORD id)
{
	for (ListOfDayTransactions::iterator listIt = begin(); listIt != end(); listIt++)
	{
		ListOfTransactions * pList = *listIt;
		for (ListOfTransactions::iterator it = pList->begin(); it != pList->end(); it++)
		{
			Transaction * pTa = (*it);
			if (pTa->id == id)
			{
				return pTa;
			}
		} // for (it)
	} // for (listIt)
	return NULL;
}

Transaction * Transactions::FindTransactionByOrderNumberEx(double num, DWORD line, DWORD dwDate, DWORD flags)
{
	Transaction * pTaSent = NULL;
	Transaction * pTaActive = NULL;
	for (ListOfDayTransactions::iterator listIt = begin(); listIt != end(); listIt++)
	{
		ListOfTransactions * pList = *listIt;
		COMPARE_DWDATES_AND_CONTINUE(dwDate);
		for (ListOfTransactions::iterator it = pList->begin(); it != pList->end(); it++)
		{
			Transaction * pTa = (*it);
			if (pTa->orderKey == num)
			{
				if (flags & Transaction::F_SENT)
				{
					if (pTaSent == NULL)
					{
						if (pTa->status == Transaction::STATUS_SENT)
							pTaSent = pTa;
					}
				}
				if (flags & Transaction::F_ACTIVE)
				{
					if (pTaActive == NULL)
					{
						if (Transaction::IsActive(pTa->status))
							pTaActive = pTa;
					}
				}
				if (line > 0)
				{
					if (pTa->line == line)
					{
						return pTa;
					}
				}
				else
				{
					return pTa;
				}
			}
		} // for (it)
	} // for (listIt)
	if (pTaSent)
		return pTaSent;
	else if (pTaActive)
		return pTaActive;
	else
		return NULL;
}

Transaction * Transactions::FindTransactionByOrderNumber(double num, DWORD line, DWORD dwDate)
{
	for (ListOfDayTransactions::iterator listIt = begin(); listIt != end(); listIt++)
	{
		ListOfTransactions * pList = *listIt;
		COMPARE_DWDATES_AND_CONTINUE(dwDate);
		for (ListOfTransactions::iterator it = pList->begin(); it != pList->end(); it++)
		{
			Transaction * pTa = (*it);
			if (pTa->orderKey == num)
			{
				if (line > 0)
				{
					if (pTa->line == line)
					{
						return pTa;
					}
				}
				else
				{
					return pTa;
				}
			}
		} // for (it)
	} // for (listIt)
	return NULL;
}

Transaction * Transactions::FindTransactionByOrderNumber2(double num, DWORD line, DWORD dwDate)
{
	Transaction * pTa;
	Transactions::Iterator it;
	pTa = FindTransactionByOrderNumber2(num, line, it);
	return pTa;
}

Transaction * Transactions::FindTransactionByOrderNumber2(double num, DWORD line, Transactions::Iterator & its, DWORD dwDate)
{
	for (its.list = begin(); its.list != end(); its.list++)
	{
		ListOfTransactions * pList = *its.list;
		COMPARE_DWDATES_AND_CONTINUE(dwDate);
		for (its.ta = pList->begin(); its.ta != pList->end(); its.ta++)
		{
			Transaction * pTa = (*its.ta);
			if (pTa->orderKey == num)
			{
				//if (line > 0)
				{
					if (pTa->line == line)
					{
						return pTa;
					}
				}
				//else
				//{
				//	pTransaction = pTa;
				//	return pTransaction;
				//}
			}
		} // for (it)
	} // for (listIt)
	return NULL;
}

Transaction * Transactions::FindTransaction(DWORD id, double num, DWORD line, DWORD dwDate)
{
	for (ListOfDayTransactions::iterator listIt = begin(); listIt != end(); listIt++)
	{
		ListOfTransactions * pList = *listIt;
		COMPARE_DWDATES_AND_CONTINUE(dwDate);
		for (ListOfTransactions::iterator it = pList->begin(); it != pList->end(); it++)
		{
			Transaction * pTa = (*it);
			if (id != pTa->id)
				continue;
			if (num != pTa->orderKey)
				continue;
			if (line != pTa->line)
				continue;
			return pTa;
		} // for (it)
	} // for (listIt)
	return NULL;
}

#if 0
Transaction * Transactions::FindTransactionByDate(SYSTEMTIME & stime, Transactions::Iterator & its)
{
	DWORD date = (stime.wYear << 16) | (stime.wMonth << 8) | (stime.wDay);
	for (; its.list != end(); )
	{
		ListOfTransactions * pList = *its.list;
		for (; its.ta != pList->end(); its.ta++)
		{
			Transaction * pTa = (*its.ta);
			DWORD _date = (pTa->time.wYear << 16) | (pTa->time.wMonth << 8) | (pTa->time.wDay);
			if (_date == date)
			{
				its.ta++;
				return pTa;
			}
		} // for (it)
		its.list++;
		if (its.list != end())
		{
			pList = *its.list;
			its.ta = pList->begin();
		}
		else
			break;
	} // for (listIt)
	return NULL;
}

Transaction * Transactions::FindTransactionByDate(SYSTEMTIME & stime1, SYSTEMTIME & stime2, Transactions::Iterator & its)
{
	DWORD date1 = (stime1.wYear << 16) | (stime1.wMonth << 8) | (stime1.wDay);
	DWORD date2 = (stime2.wYear << 16) | (stime2.wMonth << 8) | (stime2.wDay);
	for (; its.list != end(); )
	{
		ListOfTransactions * pList = *its.list;
		for (; its.ta != pList->end(); its.ta++)
		{
			Transaction * pTa = (*its.ta);
			DWORD _date = (pTa->time.wYear << 16) | (pTa->time.wMonth << 8) | (pTa->time.wDay);
			if (_date >= date1 && _date <= date2)
			{
				its.ta++;
				return pTa;
			}
		} // for (it)
		its.list++;
		if (its.list != end())
		{
			pList = *its.list;
			its.ta = pList->begin();
		}
		else
			break;
	} // for (listIt)
	return NULL;
}
#else
Transaction * Transactions::FindTransactionByDate(SYSTEMTIME & stime, Transactions::Iterator & its)
{
	DWORD date = GetTimeStampDate(stime);
	for (; its.list != end(); )
	{
		ListOfTransactions * pList = *its.list;
		DWORD d = GetTimeStampDate(pList->front()->time);
		if (d == date)
		{
			for (; its.ta != pList->end(); its.ta++)
			{
				Transaction * pTa = (*its.ta);
				its.ta++;
				return pTa;
			} // for (it)
		}
		its.list++;
		if (its.list != end())
		{
			pList = *its.list;
			its.ta = pList->begin();
		}
		else
			break;
	} // for (listIt)
	return NULL;
}

Transaction * Transactions::FindTransactionByDate(SYSTEMTIME & stime1, SYSTEMTIME & stime2, Transactions::Iterator & its)
{
	DWORD date1 = GetTimeStampDate(stime1);
	DWORD date2 = GetTimeStampDate(stime2);
	for (; its.list != end(); )
	{
		ListOfTransactions * pList = *its.list;
		DWORD d = GetTimeStampDate(pList->front()->time);
		if (d >= date1 && d <= date2)
		{
			for (; its.ta != pList->end(); its.ta++)
			{
				Transaction * pTa = (*its.ta);
				its.ta++;
				return pTa;
			} // for (it)
		}
		its.list++;
		if (its.list != end())
		{
			pList = *its.list;
			its.ta = pList->begin();
		}
		else
			break;
	} // for (listIt)
	return NULL;
}
#endif
Transaction * Transactions::FindTransactionByDate(DWORD date, Transactions::Iterator & its)
{
	Transaction * pTa = NULL;
	for (ListOfDayTransactions::iterator listIt = begin(); listIt != end(); listIt++)
	{
		ListOfTransactions * pList = *listIt;
		its.list = listIt;
		DWORD d = GetTimeStampDate(pList->front()->time);
		if (d == date)
		{
			its.ta = pList->begin();
			pTa = *(its.ta);
			break;
		}
	}
	return pTa;
}
Transaction * Transactions::FindFirstTransaction(Transactions::Iterator & its)
{
	for (its.list = begin(); its.list != end(); its.list++)
	{
		ListOfTransactions * pList = *its.list;
		for (its.ta = pList->begin(); its.ta != pList->end(); its.ta++)
		{
			Transaction * pTa = (*its.ta);
			return pTa;
		} // for (it)
	} // for (listIt)
	return NULL;
}

Transaction * Transactions::FindNextTransaction(Transactions::Iterator & its, BOOL autoIncList)
{
	Transaction * pTa = NULL;
	if (its.list != end())
	{
		ListOfTransactions * pList = *its.list;
		if (its.ta == pList->end())
		{
			if (autoIncList)
				its.list++;	
			else
				return pTa;
			if (its.list != end())
			{
				pList = *its.list;
				its.ta = pList->begin();
				pTa = (*its.ta);
				its.ta++;
			}
		}
		else
		{
			pTa = (*its.ta);
			its.ta++;
		}
	}
	return pTa;
}

Transaction * Transactions::FindFirstDate(Transactions::Iterator & its)
{
	return FindFirstTransaction(its);
}

Transaction * Transactions::FindNextDate(Transactions::Iterator & its)
{
	Transaction * pTa = NULL;
	if (its.list != end())
		its.list++;
	if (its.list != end())
	{
		ListOfTransactions * pList = *its.list;
		its.ta = pList->begin();
		pTa = *(its.ta);
	}
	return pTa;
}

Transaction * Transactions::FindSent(double num, DWORD dwDate)
{
	Transaction * pTransaction = NULL;
	for (ListOfDayTransactions::iterator listIt = begin(); listIt != end(); listIt++)
	{
		ListOfTransactions * pList = *listIt;
		COMPARE_DWDATES_AND_CONTINUE(dwDate);
		for (ListOfTransactions::iterator it = pList->begin(); it != pList->end(); it++)
		{
			Transaction * pTa = (*it);
			if (pTa->orderKey == num)
			{
				if (pTa->status == Transaction::STATUS_SENT)
				{
					pTransaction = pTa;
					return pTransaction;
				}
			}
		} // for (it)
	} // for (listIt)
	return NULL;
}


//
// class TransCtrl 
//
TransCtrl::TransCtrl()
{
	m_transId = 0;
	m_tx.n = 0;
	m_initialized = FALSE;

	m_hWnd = NULL;
}

TransCtrl::~TransCtrl()
{
}

int TransCtrl::Init(HWND hWnd, int autoCheck)
{
	int result = S_OK;

	m_hWnd = hWnd;

	result = m_trans2quik.Init(hWnd);
	if (result == NOERROR)
	{
		m_initialized = TRUE;
		if (! autoCheck)
			m_trans2quik.CheckConnection();
	}
	else
	{		
		if (! autoCheck)
			::PostMessage(m_hWnd, UM_TRANS2QUIK, (WPARAM)-1, 0);		
	}
	SetAutocheck(autoCheck);
	return result;
}

void TransCtrl::Close()
{
	SetAutocheck(FALSE);
}

void TransCtrl::DeleteAllTransactions(int flags) // ::std::list<DWORD> * pDates
{

	m_transactions.Clear();
}

int TransCtrl::GetDates(::std::list<DWORD> * pDates)
{
	return m_transactions.GetDates(pDates);
}

int TransCtrl::SetAutocheck(int autoCheck)
{
	int status;
	HWND hWnd = m_hWnd;
	if (hWnd != NULL)
	{
		m_trans2quik.ClearState();

		const UINT id = TIMER_CONNECTION;		
		if (autoCheck)
			::SetTimer(hWnd, id, 1000, NULL);
		else
			::KillTimer(hWnd, id);
		status = S_OK;
	}
	else
		status = -1;
	return status;
}

int TransCtrl::CheckConnection()
{
#if 0
	if (m_initialized)
#endif
		return m_trans2quik.CheckConnection();
}

Transaction * TransCtrl::CreateNewTransaction(Transaction * pTransaction, LPCTSTR name, int updateParams, Transactions::Iterator * pIts)
{
	SYSTEMTIME * pTime = NULL;
	if (updateParams)
	{
		SYSTEMTIME time;
		::GetLocalTime(&time);
		pTime = &time;
	}
#if 0
	m_transId = m_tx.n + 1;
#else
	m_transId++;
#endif
	Transaction * pNewTransaction = m_transactions.Create(pTransaction, name, m_transId, pTime);
	m_transactions.Add(pNewTransaction, pIts);
	return pNewTransaction;
}

Transaction * TransCtrl::UpdateTransaction(Transaction * pTransaction, int updateParams)
{
	SYSTEMTIME * pTime = NULL;
	if (updateParams)
	{
		SYSTEMTIME time;
		::GetLocalTime(&time);
		pTime = &time;
	}
#if 0
	m_transId++;
#endif
	return pTransaction;
}

int TransCtrl::RemoveTransaction(const Transaction * pTa, Transactions::Iterator & its)
{
	if (pTa != NULL)
		return m_transactions.Remove(its);
	else 
		return 0;
}

Transaction * TransCtrl::FindTransactionById(DWORD id)
{
	return m_transactions.FindTransactionById(id);
}

Transaction * TransCtrl::FindTransactionByOrderNumber(double num, DWORD line, DWORD dwDate)
{
	return m_transactions.FindTransactionByOrderNumber(num, line, dwDate);
}

Transaction * TransCtrl::FindTransactionByOrderNumberEx(double num, DWORD line, DWORD dwDate, DWORD flags)
{
	return m_transactions.FindTransactionByOrderNumberEx(num, line, dwDate, flags);
}

Transaction * TransCtrl::FindTransactionByOrderNumber2(double num, DWORD line, Transactions::Iterator & its, DWORD dwDate)
{
	return m_transactions.FindTransactionByOrderNumber2(num, line, its, dwDate);
}

Transaction * TransCtrl::FindTransaction(DWORD id, double num, DWORD line, DWORD dwDate)
{
	return m_transactions.FindTransaction(id, num, line, dwDate);
}

Transaction * TransCtrl::FindSent(double num, DWORD dwDate)
{
	return m_transactions.FindSent(num, dwDate);
}

static CWindow * s_pWnd = NULL;

void __stdcall TransactionReplyCallback (long nTransactionResult, long nTransactionExtendedErrorCode, long nTransactionReplyCode, 
										   DWORD dwTransId, double dOrderNum, LPCSTR lpcstrTransactionReplyMessage)
{
	theApp.EnterCriticalSection();
#if _DEBUG
	TRACE("TransactionReplyCallback:\r\n");
	TRACE("\tnTransactionResult=%d\r\n", nTransactionResult);
	TRACE("\tnTransactionExtendedErrorCode=%d\r\n", nTransactionExtendedErrorCode);
	TRACE("\tnTransactionReplyCode=%d\r\n", nTransactionReplyCode);
	TRACE("\tdwTransId=%d\r\n", dwTransId);
	TRACE("\tdOrderNum=%d\r\n", (DWORD)dOrderNum);
	TRACE("\tlpcstrTransactionReplyMessage=%s\r\n", lpcstrTransactionReplyMessage);
#endif
#if 1
	Transaction::Answer * pAnswer = new Transaction::Answer(nTransactionResult, nTransactionExtendedErrorCode, nTransactionReplyCode, 
		dwTransId, dOrderNum, lpcstrTransactionReplyMessage, 0, 1);

	CWindow * pWnd = s_pWnd;
	if (pWnd)
		pWnd->PostMessage(UM_TRANS2QUIK_EX, (WPARAM)pAnswer, (LPARAM)NULL);
#endif
	theApp.LeaveCriticalSection();
}


// Выполнение транзакции
int TransCtrl::DoTransaction(Transaction * pTransaction, int nd, int flags, CWindow * pWnd, int * pResult)
{	
	TSTRING_BIG2(str, size);
	TCHAR * pDst = str;
	int n = 0;
	DWORD id;
#if 1
	id = pTransaction->id;
#else
	id = m_tx.n;
#endif
	n += _stprintf_s(pDst + n, size - n, TEXT("TRANS_ID=%d; "), id);
	n += _stprintf_s(pDst + n, size - n, TEXT("CLASSCODE=%s; "), pTransaction->strClassCode);
	n += _stprintf_s(pDst + n, size - n, TEXT("SECCODE=%s; "), pTransaction->strSecCode);
	n += _stprintf_s(pDst + n, size - n, TEXT("ACTION=%s; "), Transaction::ActionToString(pTransaction->action));

	TSTRING_SMALL(strPrice);
	if (Transaction::IsActionCancel(pTransaction->action))
	{// Снятие заявки
		if (pTransaction->baseAction == Transaction::NEW_STOP_ORDER)
			n += _stprintf_s(pDst + n, size - n, TEXT("STOP_ORDER_KEY=%.0f; "), pTransaction->orderKey); // stopOrderKey
		else
			n += _stprintf_s(pDst + n, size - n, TEXT("ORDER_KEY=%.0f; "), pTransaction->orderKey);
	}
	else
	{// Выставление заявки:
		n += _stprintf_s(pDst + n, size - n, TEXT("ACCOUNT=%s; "), pTransaction->strAccount);
		if (0 != lstrlen(pTransaction->strClientCode))
			n += _stprintf_s(pDst + n, size - n, TEXT("CLIENT_CODE=%s; "), pTransaction->strClientCode);

		n += _stprintf_s(pDst + n, size - n, TEXT("OPERATION=%s; "), 
			(pTransaction->operation == Transaction::OPERATION_BUY) ? TEXT("B") : TEXT("S"));

		FormatString(strPrice, SIZEOF_ARRAY(strPrice), pTransaction->price, nd);
		n += _stprintf_s(pDst + n, size - n, TEXT("PRICE=%s; "), strPrice);
		TSTRING_SMALL(strQuantity);
		_stprintf_s(strQuantity, SIZEOF_ARRAY(strQuantity), TEXT("%.0f"), pTransaction->quantity);
		n += _stprintf_s(pDst + n, size - n, TEXT("QUANTITY=%s; "), strQuantity);

		if (Transaction::IsActionStop(pTransaction->action))
		{// Стоп-заявка:
			FormatString(strPrice, SIZEOF_ARRAY(strPrice), pTransaction->stop.price, nd);
			n += _stprintf_s(pDst + n, size - n, TEXT("STOPPRICE=%s; "), strPrice);
			if (pTransaction->expire.type == Transaction::EXPIRE_DATE)
			{
				n += _stprintf_s(pDst + n, size - n, TEXT("EXPIRY_DATE=%04d%02d%02d; "), 
					pTransaction->expire.date.wYear, pTransaction->expire.date.wMonth, pTransaction->expire.date.wDay);
			}
			else if (pTransaction->expire.type == Transaction::EXPIRE_GTC)
				n += _stprintf_s(pDst + n, size - n, TEXT("EXPIRY_DATE=GTC")); 
		}
	}

	return SendString(str, size, n, flags, pWnd, pResult);
}

int TransCtrl::CancelTransactions(Transaction::Action action, Transaction::Operation operation, 
	LPCTSTR seccode, LPCTSTR classcode, LPCTSTR clientcode, LPCTSTR account, CWindow * pWnd, int * pResult)
{
	TSTRING_BIG2(str, size);
	TCHAR * pDst = str;
	int n = 0;

	DWORD id = ++m_transId;

	n += _stprintf_s(pDst + n, size - n, TEXT("TRANS_ID=%d; "), id);
	if (classcode)
		n += _stprintf_s(pDst + n, size - n, TEXT("CLASSCODE=%s; "), classcode);
	n += _stprintf_s(pDst + n, size - n, TEXT("ACTION=%s; "), Transaction::ActionToString(action));
	
	if (operation != Transaction::OPERATION_UNKNOWN)
		n += _stprintf_s(pDst + n, size - n, TEXT("OPERATION=%s; "), (operation == Transaction::OPERATION_BUY) ? TEXT("B") : TEXT("S"));

	if (account)
		n += _stprintf_s(pDst + n, size - n, TEXT("ACCOUNT=%s; "), account);
	if (clientcode)
		n += _stprintf_s(pDst + n, size - n, TEXT("CLIENT_CODE=%s; "), clientcode);

	if (seccode)
		n += _stprintf_s(pDst + n, size - n, 
		(action == Transaction::KILL_ALL_FUTURES_ORDERS) ? TEXT("BASE_CONTRACT=%s; ") : TEXT("SECCODE=%s; "), seccode);

	return SendString(str, size, n, 0x0, pWnd, pResult);
}

int TransCtrl::SendString(LPTSTR str, size_t size, int n, int flags, CWindow * pWnd, int * pResult)
{
	int status = S_OK;
	str[n] = '\0';
#ifdef _DEBUG
	USES_CONVERSION;
	TRACE("%s\r\n", T2A(str));
#endif
	if (pWnd)
		s_pWnd = pWnd;
#if !DISABLE_TRANS2QUIK
	// Сначала проверяем наличие подключения:
	int result;
#if 1
	result = m_trans2quik.CheckConnection();
#else
	result = TRANS2QUIK_SUCCESS;
#endif
	if (result == TRANS2QUIK_SUCCESS)
	{
		USES_CONVERSION;		

		if (theApp.m_settings.log.printTransaction)
			theApp.LogMessage(str);
#if 1
		result = m_trans2quik.funcs.set_transaction_reply_calback(TransactionReplyCallback, 
			&m_trans2quik.m_error.extCode, m_trans2quik.m_error.text, sizeof(m_trans2quik.m_error.text));
		result = m_trans2quik.funcs.send_async_transaction(T2A(str), 
			&m_trans2quik.m_error.extCode, m_trans2quik.m_error.text, sizeof(m_trans2quik.m_error.text));
#else
		long nReplyCode;
		DWORD dwTransId;
		double dOrderNum;
		result = m_trans2quik.funcs.send_sync_transaction(T2A(str), 
			&nReplyCode, &dwTransId, &dOrderNum, m_trans2quik.m_message.text, sizeof(m_trans2quik.m_message.text), 
			&m_trans2quik.m_error.extCode, m_trans2quik.m_error.text, sizeof(m_trans2quik.m_error.text));
		TransactionReplyCallback(result, m_trans2quik.m_error.extCode, nReplyCode, dwTransId, dOrderNum, m_trans2quik.m_message.text);
#endif
		m_tx.n++;
	}
	else
	{
		status = E_FAIL;
	}
	if (result != TRANS2QUIK_SUCCESS)
	{
		_stprintf_s(str, size, TEXT("%s"), TRANS2QUIK::ResultToString(result));
		theApp.LogMessage(str, result);
	}
#endif // !DISABLE_TRANS2QUIK
	if (pResult != NULL)
		*pResult = result;

	return status;
}

Transaction * TransCtrl::OnAnswer(Transaction::Answer * pAnswer, Transaction * pTransaction)
{
	if (pTransaction == NULL)
	{
		if (pAnswer->orderNum != 0)
			pTransaction = FindTransactionByOrderNumber((DWORD)pAnswer->orderNum, pAnswer->ext.line);
		if (pTransaction == NULL && pAnswer->id != 0)
		{
			pTransaction = FindTransactionById(pAnswer->id);
#if 0
			if (pAnswer->orderNum != 0)
			{
				if (pTransaction != NULL)
					pTransaction->SetPending();
			}
#endif
		}
	}
	if (pTransaction != NULL)
	{
		if (pAnswer->result != TRANS2QUIK_SUCCESS)
		{
			if (pAnswer->replyCode == Transaction::STATUS_SENT)
			{
				pAnswer->replyCode = Transaction::STATUS_WRONG_PARAMETERS;
			}
			pTransaction->status = (Transaction::Status)pAnswer->replyCode;
		}
		if (pAnswer->ext.callback)
		if (pAnswer->replyCode != Transaction::STATUS_EXECUTED)
			pTransaction->status = (Transaction::Status)pAnswer->replyCode;

		// Обновляем номер заявки:
		if (pAnswer->orderNum != 0 && (pTransaction->orderKey == 0))
			pTransaction->orderKey = pAnswer->orderNum;
	}
	return pTransaction;
}


int COMPARE_TRANSACTIONS(const Transaction * pTa1, const Transaction * pTa2)
{
	int result = -1;
	if (pTa1->orderKey == pTa2->orderKey && 
		//pTa1->n == pTa2->n &&
		pTa1->operation == pTa2->operation && 
		pTa1->price == pTa2->price && 
		pTa1->quantity == pTa2->quantity && 
		pTa1->volume == pTa2->volume && 
		0 == memcmp(&pTa1->time, &pTa2->time, sizeof(pTa1->time)) && 
		pTa1->status == pTa2->status)
	{
		result = S_OK;
	}
	return result;
}

QWORD GetTimeStamp64(const SYSTEMTIME & stime, int flags)
{
	QWORD stamp;
	DWORD time, date;
	stamp = 0;
	if (flags & TIMESTAMP_TIME)
	{
		time = (stime.wSecond & 0x3f) | ((stime.wMinute & 0x3f) << 6) | ((stime.wHour & 0x1f) << 12);
		stamp |= time;
	}
	if (flags & TIMESTAMP_DATE)
	{
		date = (stime.wDay & 0x1f) | ((stime.wMonth & 0x0f) << 5) | ((stime.wYear & 0x3fff) << 9);
		stamp |= date << 17;
	}
	return stamp;
}

DWORD GetTimeStampTime(const SYSTEMTIME & stime)
{
	DWORD time = MAKE_TIME_STAMP_TIME(stime.wHour, stime.wMinute, stime.wSecond);
	return time;
}

DWORD GetTimeStampDate(const SYSTEMTIME & stime)
{
	DWORD date = MAKE_TIME_STAMP_DATE(stime.wYear, stime.wMonth, stime.wDay);
	return date;
}

QWORD GetTimeStamp64(const Transaction * pTa, int flags)
{
	return GetTimeStamp64(pTa->time, flags);
}

DWORD GetTimeStampTime(const Transaction * pTa)
{
	return GetTimeStampTime(pTa->time);
}

DWORD GetTimeStampDate(const Transaction * pTa)
{
	return GetTimeStampDate(pTa->time);
}

int GetYearFromTimeStamp(DWORD date)
{
	return ((date >> 9) & 0x3fff);
}

int GetMonthFromTimeStamp(DWORD date)
{
	return ((date >> 5) & 0x0f);
}

int GetDayFromTimeStamp(DWORD date)
{
	return ((date) & 0x1f);
}

int Write(const Transaction * pTa, my::lib::File & file)
{
	CHAR str[MAX_PATH_EX];
	const size_t size = SIZEOF_ARRAY(str);
	int len;
	USES_CONVERSION;

	// Название:
#if 0
	len = sprintf_s(str, size, "[%s]\t", T2A(pTa->strSecCode));
#else
	len = sprintf_s(str, size, "{%s}\t", T2A(pTa->strName));
#endif
	file.Write((LPCSTR)str, len);
	// Номер:
#if 0
	len = sprintf_s(str, size, "%.0f%s\t", pTa->orderKey, Transaction::IsActionStop(pTa->baseAction) ? "\t" : "");
#else
	len = sprintf_s(str, size, "%.0f\t", pTa->orderKey);
#endif
	file.Write((LPCSTR)str, len);
	// Номер строки:
	len = sprintf_s(str, size, "%d\t", Transaction::LineToLineIndex(pTa->line));
	file.Write((LPCSTR)str, len);
	// Дата и время:
	len = sprintf_s(str, size, "%02d.%02d.%04d %02d:%02d:%02d\t", 
		pTa->time.wDay, pTa->time.wMonth, pTa->time.wYear, pTa->time.wHour, pTa->time.wMinute, pTa->time.wSecond);
	file.Write((LPCSTR)str, len);
	len = sprintf_s(str, size, "%s\t", pTa->operation == Transaction::OPERATION_BUY ? "B" : "S");
	file.Write((LPCSTR)str, len);
	// Цена:
	len = sprintf_s(str, size, "%f\t", pTa->price);
	file.Write((LPCSTR)str, len);
	// Количество:
	len = sprintf_s(str, size, "%.0f\t", pTa->quantity);
	file.Write((LPCSTR)str, len);
	// Объем:
	len = sprintf_s(str, size, "%f\t", pTa->volume);
	file.Write((LPCSTR)str, len);
	// Цена 2 (стоп-цена):
	len = sprintf_s(str, size, "%f\t", pTa->stop.price);
	file.Write((LPCSTR)str, len);
	// Атрибуты (формат -cas-):
	len = sprintf_s(str, size, "-%c%c%c-", 
		Transaction::IsCanceled(pTa->status) ? 'c' : '-', 
		Transaction::IsActive(pTa->status) ? 'a' : '-',
		Transaction::IsActionStop(pTa->baseAction) ? 's' : '-'); 
	file.Write((LPCSTR)str, len);

	file.Write("\r\n", 2);
	return S_OK;
}

int Read(Transaction & ta, my::lib::File & file)
{
	int status = -1;

	char data[1000];
	char buf[100];
	int it = 0;

	for (;;)
	{
		int ready = 0;
		UINT nrBytes = file.Read(buf, sizeof(buf));
		if (nrBytes > 0)
		{
			for (UINT i = 0; i < nrBytes; i++)
			{
				if (buf[i] == '\n')
				{
					if (i + 1 < nrBytes)
						file.Seek(-(LONGLONG)(nrBytes - (i + 1)), my::lib::File::current);
					if (i > 0 && buf[i-1] == '\r')
					{
						it--;
					}
					ready = 1;
					break;
				}
				else
				{
					if (it < sizeof(data) - 1)
						data[it++] = buf[i];
					else
						break;
				}
			} // for (i)
			if (ready)
				break;
		}
		else
			break;
	} // for (;;)
	if (it > 0)
	{// Обработка строк:
		data[it] = 0;
		int i, n = 0;
		// Код:
		char * pBegin = data;
		char * pStr;
		char * pEnd;
		LPTSTR pTaStr = NULL;
		int bracketEnd;
		if (pStr = strchr(pBegin, '['))
		{
			pTaStr = ta.strSecCode;
			bracketEnd = ']';
		}
#if 1
		else if (pStr = strchr(pBegin, '{'))
		{
			pTaStr = ta.strName;
			bracketEnd = '}';
		}
#endif
		if (pStr != NULL)
		{
			pEnd = strchr(++pStr, bracketEnd);
			if (pEnd != NULL && pEnd > pStr)
			{
				memset(&ta, 0, sizeof(ta));
#if 0
				for (i = 0; i < (pEnd - pStr); i++)
					pTaStr[i] = static_cast<TCHAR>(pStr[i]);				
#else				
				CComBSTR bstr((pEnd - pStr), pStr);
				int count = bstr.Length();
				for (i = 0; i < count; i++)
					pTaStr[i] = bstr[i];				
#endif
				pTaStr[i] = TEXT('\0');
				pStr = ++pEnd;
				// Пропускаем '\t' и ' ':
				pEnd = my::str::SkipGaps(pStr, it - (pStr - pBegin));
				if (pEnd != NULL)
				{
					char * end;
					// Номер:
					pStr = pEnd;
					ta.orderKey = strtod(pStr, &end); pStr = end;
#if 0
					if (ta.orderKey < 0)
					{
						DWORD val32 = static_cast<DWORD>(ta.orderKey);
						ta.orderKey = val32;
					}
#endif
					pEnd = my::str::SkipGaps(pStr, it - (pStr - pBegin));
					if (pEnd != NULL)
					{
						pStr = pEnd;
						// Номер строки:
						ta.line = strtol(pStr, &end, 10); pStr = end;							
						pEnd = my::str::SkipGaps(pStr, it - (pStr - pBegin));
						if (pEnd != NULL)
						{
							pStr = pEnd;								
							// Дата:
							ta.time.wDay = (WORD)strtol(pStr, &end, 10); pStr = end; pStr++;
							ta.time.wMonth = (WORD)strtol(pStr, &end, 10); pStr = end; pStr++;
							ta.time.wYear = (WORD)strtol(pStr, &end, 10); pStr = end; pStr++;
							pEnd = my::str::SkipGaps(pStr, it - (pStr - pBegin));
							if (pEnd != NULL)
							{
								pStr = pEnd;
								// Время:
								ta.time.wHour = (WORD)strtol(pStr, &end, 10); pStr = end; pStr++;
								ta.time.wMinute = (WORD)strtol(pStr, &end, 10); pStr = end; pStr++;
								ta.time.wSecond = (WORD)strtol(pStr, &end, 10); pStr = end; pStr++;
								pEnd = my::str::SkipGaps(pStr, it - (pStr - pBegin));
								if (pEnd != NULL)
								{
									pStr = pEnd;
									// Операция:
									if (*pStr == 'B' || *pStr == 'b')
										ta.operation = Transaction::OPERATION_BUY;
									else
										ta.operation = Transaction::OPERATION_SELL;
									++pStr;
									pEnd = my::str::SkipGaps(pStr, it - (pStr - pBegin));
									if (pEnd != NULL)
									{
										pStr = pEnd;
										// Цена:		
										ta.price = strtod(pStr, &end);
										pStr = end;
										pEnd = my::str::SkipGaps(pStr, it - (pStr - pBegin));
										if (pEnd != NULL)
										{
											pStr = pEnd;
											// Количество:
											ta.quantity = strtod(pStr, &end);
											pStr = end;
											pEnd = my::str::SkipGaps(pStr, it - (pStr - pBegin));
											if (pEnd != NULL)
											{
												pStr = pEnd;
												// Объем:
												ta.volume = strtod(pStr, &end);
												if (ta.volume == 0)
													ta.volume = ta.quantity * ta.price;
												pStr = end;
												pEnd = my::str::SkipGaps(pStr, it - (pStr - pBegin));
												if (pEnd != NULL)
												{
													pStr = pEnd;
													// Цена 2 (стоп-цена):		
													ta.stop.price = strtod(pStr, &end);
													pStr = end;
													pEnd = my::str::SkipGaps(pStr, it - (pStr - pBegin));
													if (pEnd != NULL)
													{
														pStr = pEnd;
														// Атрибуты:
														ta.baseAction = ta.action = Transaction::NEW_ORDER;
														ta.status = Transaction::STATUS_EXECUTED;
														if (pStr[2] == 'a')
															ta.status = Transaction::STATUS_EX_BID_ACTIVE;
														if (pStr[1] == 'c')
															ta.status = Transaction::STATUS_EX_BID_CANCELED;
														if (pStr[3] == 's')
															ta.baseAction = ta.action = Transaction::NEW_STOP_ORDER;

														status = S_OK;
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	} // if (it > 0)
	return status;
}

int MakeDescription(const Transaction * pTa, LPTSTR text, size_t size, int nd)
{
	LPCTSTR name = Transaction::ActionToStringRus(pTa->action);
	TSTRING_SMALL(strStopPrice);
	if (Transaction::IsActionStop(pTa->baseAction))
	{
		TCHAR strFormat[MAX_PATH] = TEXT(" (стоп-цена %0.xf)");
		strFormat[15] = (nd % 10) + 0x30;
		_stprintf_s(strStopPrice, SIZEOF_ARRAY(strStopPrice), strFormat, pTa->stop.price);
	}
	int n = _stprintf_s(text, size, 
		TEXT("№ %.0f от %02d.%02d.%04d %02d:%02d:%02d. ")
		TEXT("%s на %s%s. ")						
		TEXT("%s."),
		pTa->orderKey,
		pTa->time.wDay, pTa->time.wMonth, pTa->time.wYear,
		pTa->time.wHour, pTa->time.wMinute, pTa->time.wSecond,
		name, (pTa->operation == Transaction::OPERATION_BUY) ? TEXT("покупку") : TEXT("продажу"),
		Transaction::IsActionStop(pTa->baseAction) ? strStopPrice : TEXT(""),
		Transaction::StatusToString(pTa->status));
	return n;
}

int StrPrintTransactionParameters(const Transaction * pTa, LPTSTR text, size_t size, int nd, int flags)
{
	int n = 0;
	if (flags & F_PRINT_DATE)
		n += ::_stprintf_s(text + n, size - n, TEXT("%02d.%02d.%04d"), pTa->time.wDay, pTa->time.wMonth, pTa->time.wYear);
	if (flags & F_PRINT_DATE)
		n += ::_stprintf_s(text + n, size - n, TEXT("%s%02d:%02d:%02d"), (flags & F_PRINT_DATE) ? TEXT(" ") : TEXT(""),
			pTa->time.wHour, pTa->time.wMinute, pTa->time.wSecond);
	if (flags)
		n += ::_stprintf_s(text + n, size - n, TEXT(", "));

	TSTRING_SMALL(strPrice);
	TSTRING_SMALL(strVolume);
	FormatString(strPrice, SIZEOF_ARRAY(strPrice), pTa->price, nd);
	FormatString(strVolume, SIZEOF_ARRAY(strVolume), pTa->volume, nd);

	n += ::_stprintf_s(text + n, size - n, TEXT("%s, %s, %.0fx%s=%s"), 
		(pTa->operation == Transaction::OPERATION_BUY) ? TEXT("купля") : TEXT("продажа"),
		pTa->strSecCode,
		pTa->quantity, strPrice, strVolume);

	return n;
}
