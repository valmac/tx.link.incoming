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
 *  myclient.cpp
 */

#include "stdafx.h"
#include "myclient.h"
#include "um.h"
#include "MyQuik.h"
#include <std.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

CMyClient::CMyClient()
{
	m_initialized = FALSE;
	m_server.status = -1;
}

CMyClient::~CMyClient()
{
	Shutdown();
}

int CMyClient::Create(HWND hWnd)
{
	int status = S_OK;

	m_hWnd = hWnd;

	//m_server.hEvent = ::OpenEvent(EVENT_ALL_ACCESS, FALSE, DDE_SERVER_EVENT_NAME);
	if (S_OK == my::lib::CreateThread(m_thread, &CMyClient::ThreadFunc, this))
	{
		LPCTSTR name = theApp.GetAppName();
		SendMessageToServer(CLIENT_MESSAGE_CONNECT, (void*)name, lstrlen(name)*sizeof(TCHAR));
	}

	m_initialized = TRUE;
	
	return status;
}

int CMyClient::Close()
{
	LPCTSTR name = theApp.GetAppName();
	SendMessageToServer(CLIENT_MESSAGE_DISCONNECT, (void*)name, lstrlen(name)*sizeof(TCHAR));

	Shutdown();

	return S_OK;
}

void CMyClient::Shutdown()
{
	if (m_initialized)
	{
		my::lib::CloseThread(m_thread);

		ListOfMyTopics::iterator it;
		for (it = m_listOfMyTopics.begin(); it != m_listOfMyTopics.end(); it++)
		{
			CMyTopic * pMyTopic = *it;
			delete pMyTopic;
		}

		m_initialized = FALSE;
	}
}

int CMyClient::ClearCacheServer()
{
	int status = S_OK;
	if (m_initialized)
	{
		LPCTSTR name = theApp.GetAppName();
		SendMessageToServer(CLIENT_MESSAGE_CLEAR_CACHE, (void*)name, lstrlen(name)*sizeof(TCHAR));
	}
	else
		status = E_FAIL;
	return status;
}


int CMyClient::SendMessageToServer(int code, void * pData, int size)
{
	int status = S_OK;
	m_server.status = -1;

	HANDLE hEvent = ::OpenEvent(EVENT_ALL_ACCESS, FALSE, DDE_CLIENT_SERVER_EVENT_NAME);
	HANDLE hMutex = ::OpenMutex(MUTEX_ALL_ACCESS, FALSE, DDE_CLIENT_SERVER_MUTEX_NAME);

	HANDLE hFileMap = ::OpenFileMapping(FILE_MAP_READ|FILE_MAP_WRITE, FALSE, DDE_CLIENT_SERVER_FILE_NAME);
	if (hFileMap != NULL)
	{
		LPVOID lpFileMap = ::MapViewOfFile(hFileMap, FILE_MAP_READ|FILE_MAP_WRITE, 0, 0, 0);
		// Дожидаемся освобождения мьютекса:
		if (::WaitForSingleObject(hMutex, INFINITE) == WAIT_OBJECT_0)
		{
			// Теперь можно записывать данные
			BYTE * pDst = (BYTE*)lpFileMap;
			int n = sizeof(DWORD);
			DWORD sizeofTchar = sizeof(TCHAR);
			int dataSize = size;
			size += n;

			::CopyMemory(pDst, &code, n); pDst += n;
			::CopyMemory(pDst, &size, n); pDst += n;
			::CopyMemory(pDst, &sizeofTchar, n); pDst += n;
			::CopyMemory(pDst, pData, dataSize); 

			::ReleaseMutex(hMutex);

			m_server.status = S_OK;
		}
		// Сигнализируем клиенту, что данные записаны:
		::SetEvent(hEvent);
	}

	::CloseHandle(hFileMap);
	::CloseHandle(hMutex);
	::CloseHandle(hEvent);

	return status;
}

BOOL CMyClient::Poke(LPCTSTR pszTopic, LPCTSTR pszItem, void* pData, DWORD dwSize)
{
    //
    // See if we have a topic that matches
    //

    CMyTopic* pTopic = FindTopic(pszTopic);
    if (! pTopic)
	{
	    //m_DataTopic.Create(pszTopic);
		CMyTopic * pMyTopic = CreateTopic(pszTopic);
		if (pMyTopic == NULL)
			return FALSE;
		AddTopic(pMyTopic);

		pTopic = pMyTopic;
	}
#if 0
	if (pTopic != NULL)
		return pTopic->Poke(wFmt, pszItem, pData, dwSize);
	else
		return FALSE;
#else
	return TRUE;
#endif
}

BOOL CMyClient::Poke(void* pData)
{
	return FALSE;
}

CMyTopic * CMyClient::CreateTopic(LPCTSTR pszTopic)
{
	CMyTopic * pMyTopic = NULL;
	if (pszTopic[0] == TEXT(':'))
	{
		if (0 == memcmp(pszTopic, TEXT(":current"), 8*sizeof(TCHAR)))
			pMyTopic = new CMyTopicCurrent();
		else if (0 == memcmp(pszTopic, TEXT(":bid"), 4*sizeof(TCHAR)))
			pMyTopic = new CMyTopicBids();
		else if (0 == memcmp(pszTopic, TEXT(":deal"), 5*sizeof(TCHAR)))
			pMyTopic = new CMyTopicDeals();
		else if (0 == memcmp(pszTopic, TEXT(":stop"), 5*sizeof(TCHAR)))
			pMyTopic = new CMyTopicStops();
	}
	else
	{
		pMyTopic = new CMyTopicQuotes();
	}
	if (pMyTopic != NULL)
	{
		m_listOfMyTopics.push_back(pMyTopic);
		pMyTopic->Create(pszTopic);
	}
	return pMyTopic;
}

void CMyClient::AddTopic(CMyTopic * pTopic)
{
	m_listOfMyTopics.push_back(pTopic);
}

CMyTopic* CMyClient::FindTopic(LPCTSTR pszTopic)
{
	ListOfMyTopics::iterator it;
	for (it = m_listOfMyTopics.begin(); it != m_listOfMyTopics.end(); it++)
	{
		CMyTopic* pTopic = *(it);
		if (0 == lstrcmp(pTopic->m_name, pszTopic))
			return pTopic;
	}
	return NULL;
}


DWORD WINAPI CMyClient::ThreadFunc(LPVOID lpThreadParameter)
{
	DWORD result = 0;
	TRACE("CMyClient::ThreadFunc: begin\r\n");
	CMyClient * pClient = (CMyClient*)lpThreadParameter;

	HANDLE hServerEvent = ::OpenEvent(EVENT_ALL_ACCESS, FALSE, DDE_SERVER_EVENT_NAME);
	HANDLE hServerMutex = ::OpenMutex(MUTEX_ALL_ACCESS, FALSE, DDE_SERVER_MUTEX_NAME);
	HANDLE hServerEvent2 = ::OpenEvent(EVENT_ALL_ACCESS, FALSE, DDE_CLIENT_SERVER_EVENT_NAME_2);

	HANDLE hFileMap = NULL;
	LPVOID lpFileMap = NULL;

	hFileMap = ::OpenFileMapping(FILE_MAP_READ|FILE_MAP_WRITE, FALSE, DDE_SERVER_FILE_NAME);
	if (hFileMap != NULL)
	{
		lpFileMap = ::MapViewOfFile(hFileMap, FILE_MAP_READ|FILE_MAP_WRITE, 0, 0, 0);
	}

	if (hServerEvent == NULL || hServerMutex == NULL || hFileMap == NULL || lpFileMap == NULL)
	{
		HWND hWnd = pClient->m_hWnd;
		if (::IsWindow(hWnd))
			::PostMessage(hWnd, UM_SERVER_FAULT, 0, 0);
		goto end;
	}

	HANDLE hEvents[2] = {
		hServerEvent, pClient->m_thread.hEvent
	};

	while (TRUE)
	{
		DWORD dwRetCode = ::WaitForMultipleObjects(SIZEOF_ARRAY(hEvents), hEvents, FALSE, INFINITE);
		if (dwRetCode == WAIT_OBJECT_0)
		{// Сервер обновил данные:
			if (WAIT_OBJECT_0 == ::WaitForSingleObject(hServerMutex, INFINITE))
			{// Мьютекс захвачен
				BYTE * pSrc = (BYTE*)lpFileMap;
				DWORD dwSize, val32;
				int size = sizeof(DWORD);
				::CopyMemory(&val32, pSrc, size); pSrc += size;
				int sizeofTchar = (int)val32;
				::CopyMemory(&val32, pSrc, size); pSrc += size;
				int len = (int)val32;
				// Данные:
				::CopyMemory(&dwSize, pSrc + len, sizeof(DWORD));
				BYTE * newData = new BYTE [sizeof(dwSize) + sizeof(BYTE) + (len + 1)*sizeof(TCHAR) + dwSize];
				if (newData != NULL)
				{
					BYTE * pDst = newData;
					LPCTSTR pszTopic;
					memcpy(pDst, &dwSize, sizeof(dwSize)); pDst += sizeof(dwSize);
					*pDst++ = (BYTE)(len / sizeofTchar);
					{
						pszTopic = (LPCTSTR)pDst;
						TCHAR zero = 0;
						memcpy(pDst, pSrc, len); pDst += len;
						memcpy(pDst, &zero, sizeof(zero)); pDst += sizeof(zero);						
					}
					pSrc += sizeof(DWORD) + len;
					len = len / sizeofTchar;
					::CopyMemory(pDst, pSrc, dwSize); pDst += dwSize;
				
					BOOL sent = FALSE;
					HWND hWnd = pClient->m_hWnd;
					if (::IsWindow(hWnd))
					{
						int msg;
						if (0 == memcmp(pszTopic, TEXT("[Quotes]"), 8*sizeof(TCHAR)))
							msg = UM_RX_TABLE_QUOTES;
						else if (0 == memcmp(pszTopic, TEXT("[Current]"), 9*sizeof(TCHAR)) || 0 == memcmp(pszTopic, TEXT("Current"), 7*sizeof(TCHAR)))
							msg = UM_RX_TABLE_CURRENT;
						else if (0 == memcmp(pszTopic, TEXT("[Bids]"), 6*sizeof(TCHAR)) || 0 == memcmp(pszTopic, TEXT("Bids"), 4*sizeof(TCHAR)))
							msg = UM_RX_TABLE_BIDS;
						else if (0 == memcmp(pszTopic, TEXT("[Deals]"), 7*sizeof(TCHAR)) || 0 == memcmp(pszTopic, TEXT("Deals"), 5*sizeof(TCHAR)))
							msg = UM_RX_TABLE_DEALS;
						else if (0 == memcmp(pszTopic, TEXT("[Stops]"), 7*sizeof(TCHAR)) || 0 == memcmp(pszTopic, TEXT("Stops"), 5*sizeof(TCHAR)))
							msg = UM_RX_TABLE_STOPS;
						else if (0 == memcmp(pszTopic, TEXT("[Limits]"), 8*sizeof(TCHAR)) || 0 == memcmp(pszTopic, TEXT("Limits"), 6*sizeof(TCHAR)))
							msg = UM_RX_TABLE_LIMITS;
						else if (0 == memcmp(pszTopic, TEXT("[Portfolio]"), 11*sizeof(TCHAR)) || 0 == memcmp(pszTopic, TEXT("Portfolio"), 9*sizeof(TCHAR)))
							msg = UM_RX_TABLE_PORTFOLIO;
						else
							msg = 0;
#if defined _DEBUG && 0
						TRACE("msg %d\r\n", msg);
#endif
						if (msg)
#if 0
							sent = ::PostMessage(hWnd, msg, (WPARAM)newData, (LPARAM)dwSize);
#else
							sent = ::PostMessage(hWnd, UM_RX_TABLE, (WPARAM)newData, (LPARAM)msg);
#endif
					}
					if (! sent)
						delete [] newData;
				}
				// Освобождаем мьютекс:
				::ReleaseMutex(hServerMutex);
				// Сигнализируем серверу, что данные получены:
				::SetEvent(hServerEvent2);
			}
		}
		else if (dwRetCode == WAIT_OBJECT_0 + 1)
		{// Закрываемся:
			TRACE("CMyClient::ThreadFunc: terminate\r\n");
			break;
		}
		else
		{
			DWORD err = GetLastError();
			TRACE("CMyClient::ThreadFunc: error (%d)\r\n", err);
			break;
		}
	} // while (TRUE)

end:
	::CloseHandle(hServerEvent);
	::CloseHandle(hServerMutex);
	::CloseHandle(hFileMap);
	::CloseHandle(hServerEvent2);

	TRACE("CMyClient::ThreadFunc: end\r\n");
	return result;
}

//
// class CMyTopic
//
CMyTopic::CMyTopic()
{
}

int CMyTopic::Create(LPCTSTR pszTopic)
{
	m_name = pszTopic;
	return S_OK;
}

//
// class CMyTopicBids
//

CMyTopicCurrent::CMyTopicCurrent()
{
	m_msg = UM_RX_TABLE_CURRENT;
}

//
// class CMyTopicQuotes
//

CMyTopicQuotes::CMyTopicQuotes()
{
	m_msg = UM_RX_TABLE_QUOTES;
}

//
// class CMyTopicBids
//

CMyTopicBids::CMyTopicBids()
{
	m_msg = UM_RX_TABLE_BIDS;
}

//
// class CMyTopicDeals
//

CMyTopicDeals::CMyTopicDeals()
{
	m_msg = UM_RX_TABLE_DEALS;
}

//
// class CMyTopicStops
//

CMyTopicStops::CMyTopicStops()
{
	m_msg = UM_RX_TABLE_STOPS;
}


