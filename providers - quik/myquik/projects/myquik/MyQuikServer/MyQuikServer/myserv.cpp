/*
 *  Copyright (C) 2010 Alexander Chernykh 
 *
 *  This file is part of MyQuikServer: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  MyQuikServer is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 *  myserv.cpp
 */

#include "stdafx.h"
#include "myserv.h"
#include <std.h>
#include <xltable.h>

//#ifdef _DEBUG
//#define new _DEBUG_NEW
//#endif

CMyServer::CMyServer()
{
	m_hWnd = NULL;
	EnableCaching();
	SetClientConnected(FALSE);
}

CMyServer::~CMyServer()
{
	Shutdown();
}

void CMyServer::Status(const char * pszFormat, ...)
{
#ifdef _DEBUG
	char buf[1024 + 2];
	va_list arglist;
	va_start(arglist, pszFormat);
	int n = vsprintf_s(buf, sizeof(buf) - 2, pszFormat, arglist);
	va_end(arglist);
#if 1
	// Добавляем символ конца строки:
	buf[n] = '\r'; buf[n+1] = '\n'; buf[n+2] = '\0'; 
#endif
	TRACE(buf);
#endif // _DEBUG
}

void CMyServer::Shutdown(int flags)
{
	if (m_bInitialized)
	{
		CDDEServer::Shutdown(flags);

		ListOfMyTopics::iterator it;
		for (it = m_listOfMyTopics.begin(); it != m_listOfMyTopics.end(); it++)
		{
			CMyTopic * pMyTopic = *it;
			delete pMyTopic;
			//m_listOfMyTopics.erase(it);
		}

		if (m_ipc.s2c.hFileMap != NULL)
		{
			//CloseHandle(hEventTermination);

			// Отменяем отображение файла
			::UnmapViewOfFile(m_ipc.s2c.lpFileMap);

			if (m_ipc.s2c.hMutex != NULL)
			{
				::CloseHandle(m_ipc.s2c.hMutex);
			}
			::CloseHandle(m_ipc.s2c.hFileMap);
		}

#if SEPARATE_THREAD_TO_SEND_DATA	
		CloseThreadSendDataToClient();
		m_slistCtrl.Destroy();
#endif
		my::lib::CloseThread(m_thread);

		::CloseHandle(m_ipc.c2s.hEvent);
		//::CloseHandle(m_ipc.c2s.hMutex);
		//::CloseHandle(m_ipc.c2s.hFileMap);

	}
}

BOOL CMyServer::OnCreate()
{
#if 1
	m_ipc.s2c.hMutex = ::CreateMutex(NULL, FALSE, DDE_SERVER_MUTEX_NAME);
#else
	m_ipc.s2c.hMutex = ::CreateSemaphore(NULL, 1, 1, DDE_SERVER_MUTEX_NAME);
#endif
	m_ipc.s2c.hEvent = ::CreateEvent(NULL, FALSE, FALSE, DDE_SERVER_EVENT_NAME);
#if 1
	m_ipc.c2s.hEvent = ::CreateEvent(NULL, FALSE, TRUE, DDE_CLIENT_SERVER_EVENT_NAME_2); // в сигнальном состоянии;
#endif
	if (m_ipc.s2c.hFileMap == NULL)
	{
		HANDLE hFile = ::CreateFileMapping((HANDLE)0xFFFFFFFF, NULL, PAGE_READWRITE, 0, 0x10000, DDE_SERVER_FILE_NAME);
		if (hFile != NULL)
		{
			m_ipc.s2c.hFileMap = hFile;
			m_ipc.s2c.lpFileMap = ::MapViewOfFile(hFile, FILE_MAP_READ|FILE_MAP_WRITE, 0, 0, 0);
		}
	}

	if (S_OK == my::lib::CreateThread(m_thread, ThreadFunc, this))
	{
	}
	
    return TRUE;
}

int CMyServer::OnXTypConnect(LPCTSTR name)
{
	 CString msg;
	 msg.Format(TEXT("[DDE] [%s] [+] %s"), this->GetClientName(), name);
	 PRINT_MESSAGE(msg);

	return 0;
}

int CMyServer::OnXTypDisconnect(HCONV hConv)
{
	int found = 0;
	CDDEConv * pConv;
	CDDEConvList::iterator it;
	for (it = m_ConvList.begin(); it != m_ConvList.end(); it++)
	{
		pConv = *(it);
		if (pConv->m_hConv == hConv) 
		{
			found = 1;
			break;
		}
	}
	if (found)
	{
		CString msg;
		msg.Format(TEXT("[DDE] [%s] [-] %s"), this->GetClientName(), (LPCTSTR)this->StringFromHsz(pConv->m_hszTopic));
		PRINT_MESSAGE(msg);
	}

	return 0;
}

#if defined _DEBUG && 0
int g_nrEvents = 0;
#endif

void CMyServer::EnableCaching(BOOL enable)
{
	m_caching.enable = enable;
#if 1
	if (! enable)
		ClearCache();
#endif
}

void CMyServer::SetClientConnected(int connected)
{
	m_client.connected = connected;
}

BOOL CMyServer::IsClientConnected() const
{
	return m_client.connected;
}

void CMyServer::OnClientMessage(void * pData)
{
	DWORD code, dwSize;
	BYTE * pSrc = (BYTE*)pData;
	int n = sizeof(DWORD);

	memcpy(&code, pSrc, n); pSrc += n;
	memcpy(&dwSize, pSrc, n); pSrc += n;

	DWORD sizeofTchar;
	memcpy(&sizeofTchar, pSrc, n); pSrc += n;
	int len = dwSize - n;
	TCHAR str[MAX_PATH];
	memcpy(str, pSrc, len);
	len = len / sizeofTchar;
	str[len] = 0;

	CString msg;
	msg.Format(TEXT("[%s] "), str);

	ListOfMyTopics::const_iterator it;
	
	if (code == CLIENT_MESSAGE_CONNECT)
	{// Клиент только что подключился
		SetClientConnected(TRUE);

		msg += CString(TEXT("Connect"));

#if 1
		// Создаём поток для передачи данных клиенту:
		CreateThreadSendDataToClient();
#endif

		if (m_caching.enable)
		{
			// Выдаем клиенту кэшированные данные:		
			for (it = m_listOfMyTopics.begin(); it != m_listOfMyTopics.end(); it++)
			{
				CMyTopic * pMyTopic = *it;
				if (pMyTopic->IsCounted())
				{// Подготавливаем буфер для передачи
					CMyTopic::ListOfLines & lines = pMyTopic->m_listOfLines;
					if (lines.size() > 0)
					{
						CMyTopic::Line * pLine;
						CMyTopic::ListOfLines::iterator itLine;
						// Заголовок таблицы:		
						WORD tdt = TDT_TABLE;
						WORD cb = 4;
						WORD rows = 0;
						WORD cols = pMyTopic->m_table.cols;
						int size = 4 * sizeof(WORD);
						for (itLine = lines.begin(); itLine != lines.end(); itLine++)
						{
							pLine = (*itLine);
							size += pLine->buffer.GetLength();
							++rows;
						}
						if (TRUE == pMyTopic->CreateTableHeader(NULL, size))
						{
							BYTE * pDst = pMyTopic->m_table.header.buf;
							WORD val16;
							int n;

							n = sizeof(val16);
							memcpy(pDst, &tdt, n); pDst += n;
							memcpy(pDst, &cb, n); pDst += n;
							memcpy(pDst, &rows, n); pDst += n;
							memcpy(pDst, &cols, n); pDst += n;

							for (itLine = lines.begin(); itLine != lines.end(); itLine++)
							{
								pLine = (*itLine);
								n = pLine->buffer.GetLength();
								memcpy(pDst, pLine->buffer.GetData(), n); pDst += n;
							}
						}
					}
				} // if (pMyTopic->IsCounted())
				//else
				{// Сначала выдаём заголовок таблицы:
					if (pMyTopic->m_table.header.buf != NULL)
						this->Poke(0, pMyTopic->m_strName, NULL, pMyTopic->m_table.header.buf, pMyTopic->m_table.header.count);
				}
				// Теперь последняя таблица:
				if (pMyTopic->m_table.data.buf != NULL && pMyTopic->m_table.data.count > 0)
				{
					BOOL poked = FALSE;
					if (pMyTopic->m_table.header.buf != NULL)
					{
						if ((pMyTopic->m_table.header.count == pMyTopic->m_table.data.count) && 
							0 == memcmp(pMyTopic->m_table.header.buf, pMyTopic->m_table.header.buf, pMyTopic->m_table.data.count))
							poked = TRUE;
					}
					if (! poked)
						this->Poke(0, pMyTopic->m_strName, NULL, pMyTopic->m_table.data.buf, pMyTopic->m_table.data.count);
				}
			}
		} // if (m_caching.enable)
	}
	else if (code == CLIENT_MESSAGE_DISCONNECT)
	{
		msg += CString(TEXT("Disconnect"));
#if 1
		CloseThreadSendDataToClient();
#endif
		SetClientConnected(FALSE);
	}
	else if (code == CLIENT_MESSAGE_CLEAR_CACHE)
	{
		msg += CString(TEXT("Clear cache"));
		ClearCache();
	}
	PRINT_MESSAGE(msg);
}

DWORD WINAPI CMyServer::ThreadFunc(LPVOID lpThreadParameter)
{
	DWORD result = 0;
	TRACE("CMyServer::ThreadFunc: begin\r\n");
	CMyServer * pServer = (CMyServer*)lpThreadParameter;

	HANDLE hClientMutex = ::CreateMutex(NULL, FALSE, DDE_CLIENT_SERVER_MUTEX_NAME);
	HANDLE hClientEvent = ::CreateEvent(NULL, FALSE, FALSE, DDE_CLIENT_SERVER_EVENT_NAME);

	HANDLE hEvents[2] = {
		hClientEvent, pServer->m_thread.hEvent};

	// Создаём проекцию:
	HANDLE hFileMap = NULL;
	LPVOID lpFileMap = NULL;
	hFileMap = ::CreateFileMapping((HANDLE)0xFFFFFFFF, NULL, PAGE_READWRITE, 0, 0x1000, DDE_CLIENT_SERVER_FILE_NAME);
	if (hFileMap != NULL)
	{
		lpFileMap = ::MapViewOfFile(hFileMap, FILE_MAP_READ|FILE_MAP_WRITE, 0, 0, 0);
	}

	if (hClientMutex == NULL || hClientEvent == NULL || hFileMap == NULL || lpFileMap == NULL)
	{
		HWND hWnd = pServer->m_hWnd;
		if (hWnd != NULL && ::IsWindow(hWnd))
			::PostMessage(hWnd, UM_SERVER_FAULT, 0, 0);
		goto end;
	}

	while (TRUE)
	{
		DWORD dwRetCode = ::WaitForMultipleObjects(SIZEOF_ARRAY(hEvents), hEvents, FALSE, INFINITE);
		if (dwRetCode == WAIT_OBJECT_0)
		{// Получено сообщение от клиента:
			if (WAIT_OBJECT_0 == ::WaitForSingleObject(hClientMutex, INFINITE))
			{// Мьютекс захвачен
				{// Обрабатываем сообщение, переданное клиентом
					BYTE * pSrc = (BYTE*)lpFileMap;
					DWORD msgCode;
					DWORD dwSize;
					int size = sizeof(DWORD);
					::CopyMemory(&msgCode, pSrc, size); pSrc += size;
					::CopyMemory(&dwSize, pSrc, size); pSrc += size;
					BYTE * newData = new BYTE [2*size + dwSize];
					if (newData != NULL)
					{
						memcpy(newData, &msgCode, size);
						memcpy(newData + size, &dwSize, size);
						::CopyMemory(newData + (2*size), pSrc, dwSize);

						BOOL sent = FALSE;
						HWND hWnd = pServer->m_hWnd;
						if (hWnd != NULL && ::IsWindow(hWnd))
							sent = ::PostMessage(hWnd, UM_CLIENT_MESSAGE, (WPARAM)newData, (LPARAM)dwSize);
						if (! sent)
							delete [] newData;
					} // if (newData != NULL)

					// Освобождаем мьютекс:
					::ReleaseMutex(hClientMutex);
				}
			}
		}
		else if (dwRetCode == WAIT_OBJECT_0 + 1)
		{// Закрываемся:
			TRACE("CMyServer::ThreadFunc: terminate\r\n");
			break;
		}
		else
		{
			if (dwRetCode != WAIT_TIMEOUT)
			{
				DWORD err = ::GetLastError();
				TRACE("CMyServer::ThreadFunc: error (%d)\r\n", err);
				break;
			}
		}
	} // while (TRUE)

end:
	::CloseHandle(hClientMutex);
	::CloseHandle(hClientEvent);
	::CloseHandle(hFileMap);

	TRACE("CMyServer::ThreadFunc: end\r\n");
	return result;
}

#if SEPARATE_THREAD_TO_SEND_DATA
DWORD WINAPI CMyServer::SendDataToClientThreadFunc(LPVOID lpThreadParameter)
{
	DWORD result = 0;
	TRACE("CMyServer::SendDataToClientThreadFunc: begin\r\n");
	CMyServer * pServer = (CMyServer*)lpThreadParameter;

	LPCTSTR pszTopic;
	void * pData;
	DWORD size;

	HANDLE hEvent = pServer->m_threadSendDataToClient.hEvent;
#if 0
	HANDLE hEvents[2] = {hEvent, pServer->m_ipc.c2s.hEvent};
#endif
	DWORD ret;
	while (TRUE)
	{
#if 1
		ret = ::WaitForSingleObject(hEvent, 10);
#else
		ret = ::WaitForMultipleObjects(SIZEOF_ARRAY(hEvents), hEvents, FALSE, 10);
#endif
		if (ret == WAIT_OBJECT_0)
		{
			TRACE("CMyServer::SendDataToClientThreadFunc: terminate\r\n");
			break;
		}
		else
		{
#if 0
			if (ret == WAIT_OBJECT_0 + 1)
			{
			}
			else
#endif
			if (ret == WAIT_TIMEOUT)
			{
#if 0
				PSLIST_ENTRY pListEntry = pServer->m_slistCtrl.Pop(&pszTopic, &pData, &size);
				if (pListEntry != NULL)
				{
					pServer->SendDataToClient(pszTopic, pData, size);
					pServer->m_slistCtrl.Release(pListEntry);
				}
#else
				if (pServer->m_slistCtrl.GetNrItems() > 0)
				{
					// Дожидаемся ответа от клиента (клиент может и не работать, поэтому ждем ограниченное время):
					ret = ::WaitForSingleObject(pServer->m_ipc.c2s.hEvent, 10);
					if (ret == WAIT_OBJECT_0)
					{// Ответ получен - можно отправлять данные
						// Дожидаемся освобождения мьютекса:
						if (WAIT_OBJECT_0 == ::WaitForSingleObject(pServer->m_ipc.s2c.hMutex, INFINITE))
						{
							// Теперь можно записывать данные:
							PSLIST_ENTRY pListEntry = pServer->m_slistCtrl.Pop(&pszTopic, &pData, &size);
							if (pListEntry != NULL)
							{
								pServer->WriteDataToFileMap(pszTopic, pData, size);
								pServer->m_slistCtrl.Release(pListEntry);
							}
				#if 1
							::ReleaseMutex(pServer->m_ipc.s2c.hMutex);
				#else
							::ReleaseSemaphore(pServer->m_ipc.s2c.hMutex, 1, NULL);
				#endif
				#if 0
							Sleep(10);
				#endif
							// Сигнализируем клиенту, что данные записаны:
							::SetEvent(pServer->m_ipc.s2c.hEvent);
				#if defined _DEBUG && 0
							g_nrEvents++;
				#endif
						}
					}
				}
#endif
			}
			else
			{
				DWORD err = ::GetLastError();
				TRACE("CMyServer::SendDataToClientThreadFunc: error (%d)\r\n", err);
				break;
			}
		}
	} // while (TRUE)

	TRACE("CMyServer::SendDataToClientThreadFunc: end\r\n");
	return result;
}

int CMyServer::CreateThreadSendDataToClient()
{
	int status = -1;
	if (m_threadSendDataToClient.hThread == NULL)
	{
#if 1
		m_slistCtrl.Init();
		m_slistCtrl.Clear();
#endif
		status = my::lib::CreateThread(m_threadSendDataToClient, SendDataToClientThreadFunc, this);
	}
	return status;
}

int CMyServer::CloseThreadSendDataToClient()
{
	int status = -1;
	if (m_threadSendDataToClient.hThread != NULL)
	{
		status = my::lib::CloseThread(m_threadSendDataToClient);
		m_threadSendDataToClient.hThread = NULL;
#if 1
		m_slistCtrl.Clear();
#endif
	}
	return status;
}

#endif // SEPARATE_THREAD_TO_SEND_DATA

int CMyServer::WriteDataToFileMap(LPCTSTR pszTopic, void* pData, DWORD dwSize)
{
	int status;
	if (m_ipc.s2c.hFileMap != NULL)
	{
		BYTE * pDst = (BYTE*)m_ipc.s2c.lpFileMap;
		DWORD val32 = sizeof(TCHAR);
		int size = sizeof(DWORD);
		::CopyMemory(pDst, &val32, size); pDst += size;
		int len = lstrlen(pszTopic);
		val32 = len*sizeof(TCHAR);
		::CopyMemory(pDst, &val32, size); pDst += size;
		size = val32;
		::CopyMemory(pDst, pszTopic, size); pDst += size;
		// Теперь сами данные:
		val32 = dwSize;
		size = sizeof(val32);
		::CopyMemory(pDst, &val32, size); pDst += size;
		::CopyMemory(pDst, pData, dwSize);
		status = S_OK;
	}
	else
		status = E_HANDLE;
	return status;
}

int CMyServer::SendDataToClient(LPCTSTR pszTopic, void* pData, DWORD dwSize)
{
	int status = -1;
	BOOL answerReceived = FALSE;
	DWORD result;
	// Дожидаемся ответа от клиента (клиент может и не работать, поэтому ждем ограниченное время):
	result = ::WaitForSingleObject(m_ipc.c2s.hEvent, 10);
	if (result == WAIT_OBJECT_0)
	{// Ответ получен - можно отправлять данные
		answerReceived = TRUE;
		// Дожидаемся освобождения мьютекса:
		if (::WaitForSingleObject(m_ipc.s2c.hMutex, INFINITE) == WAIT_OBJECT_0)
		{
			// Теперь можно записывать данные:
			WriteDataToFileMap(pszTopic, pData, dwSize);
#if 1
			::ReleaseMutex(m_ipc.s2c.hMutex);
#else
			::ReleaseSemaphore(m_ipc.s2c.hMutex, 1, NULL);
#endif
#if 0
			Sleep(10);
#endif
			// Сигнализируем клиенту, что данные записаны:
			::SetEvent(m_ipc.s2c.hEvent);
#if defined _DEBUG && 0
			g_nrEvents++;
#endif
			status = S_OK;
		}
	}
	return status;
}

BOOL CMyServer::Poke(UINT wFmt, LPCTSTR pszTopic, LPCTSTR pszItem, void* pData, DWORD dwSize)
{
#if 1
	if (pszItem != NULL)
	{
		CMyTopic* pTopic = FindTopic(pszTopic);
		if (! pTopic)
		{
			pTopic = CreateTopic(pszTopic);
		}
		if (pTopic != NULL)
			pTopic->Poke(wFmt, pszItem, pData, dwSize);
	}
#endif
	if (IsClientConnected())
	{
#if SEPARATE_THREAD_TO_SEND_DATA
#if 0
		if (m_threadSendDataToClient.hThread == NULL)
			CreateThreadSendDataToClient();
		if (m_threadSendDataToClient.hThread != NULL)
		{// 
			m_slistCtrl.Push(pszTopic, pData, dwSize);
		}
#else
		m_slistCtrl.Push(pszTopic, pData, dwSize);
#endif
#else
		// Записываем данные в проекцию:
		if (m_ipc.s2c.hFileMap != NULL)
		{
			SendDataToClient(pszTopic, pData, dwSize);
		}
#endif // SEPARATE_THREAD_TO_SEND_DATA
	}
	return TRUE;
}

CMyTopic * CMyServer::FindTopic(LPCTSTR pszTopic)
{
	ListOfMyTopics::const_iterator it;
	for (it = m_listOfMyTopics.begin(); it != m_listOfMyTopics.end(); it++)
	{
		CMyTopic * pMyTopic = *it;
		if (0 == lstrcmp(pMyTopic->m_strName, pszTopic))
		{
			return pMyTopic;
		}
	}
	return NULL;
}

CMyTopic * CMyServer::CreateTopic(LPCTSTR pszTopic)
{
	CMyTopic * pMyTopic = NULL;
#if 0
#if 1
	if (0 == memcmp(pszTopic, TEXT("[Quotes]"), 8*sizeof(TCHAR)))
		pMyTopic = new CMyTopicQuotes();
	else if (0 == memcmp(pszTopic, TEXT("[Current]"), 9*sizeof(TCHAR)) || 0 == memcmp(pszTopic, TEXT("Current"), 7*sizeof(TCHAR)))
		pMyTopic = new CMyTopicCurrent();
	else if (0 == memcmp(pszTopic, TEXT("[Bids]"), 6*sizeof(TCHAR)) || 0 == memcmp(pszTopic, TEXT("Bids"), 4*sizeof(TCHAR)))
		pMyTopic = new CMyTopicBids();
	else if (0 == memcmp(pszTopic, TEXT("[Deals]"), 7*sizeof(TCHAR)) || 0 == memcmp(pszTopic, TEXT("Deals"), 5*sizeof(TCHAR)))
		pMyTopic = new CMyTopicDeals();
	else if (0 == memcmp(pszTopic, TEXT("[Stops]"), 7*sizeof(TCHAR)) || 0 == memcmp(pszTopic, TEXT("Stops"), 5*sizeof(TCHAR)))
		pMyTopic = new CMyTopicStops();
	else if (0 == memcmp(pszTopic, TEXT("[Limits]"), 8*sizeof(TCHAR)) || 0 == memcmp(pszTopic, TEXT("Limits"), 6*sizeof(TCHAR)))
		pMyTopic = new CMyTopicLimits();
	else
#endif
		pMyTopic = new CMyTopic();
#else
	int countedTable = 0;
	if (0 == memcmp(pszTopic, TEXT("[Quotes]"), 8*sizeof(TCHAR)))
		countedTable = 0;
	else if (0 == memcmp(pszTopic, TEXT("[Current]"), 9*sizeof(TCHAR)) || 0 == memcmp(pszTopic, TEXT("Current"), 7*sizeof(TCHAR)))
		countedTable = 1;
	else if (0 == memcmp(pszTopic, TEXT("[Bids]"), 6*sizeof(TCHAR)) || 0 == memcmp(pszTopic, TEXT("Bids"), 4*sizeof(TCHAR)))
		countedTable = 1;
	else if (0 == memcmp(pszTopic, TEXT("[Deals]"), 7*sizeof(TCHAR)) || 0 == memcmp(pszTopic, TEXT("Deals"), 5*sizeof(TCHAR)))
		countedTable = 1;
	else if (0 == memcmp(pszTopic, TEXT("[Stops]"), 7*sizeof(TCHAR)) || 0 == memcmp(pszTopic, TEXT("Stops"), 5*sizeof(TCHAR)))
		countedTable = 1;
	else if (0 == memcmp(pszTopic, TEXT("[Limits]"), 8*sizeof(TCHAR)) || 0 == memcmp(pszTopic, TEXT("Limits"), 6*sizeof(TCHAR)))
		countedTable = 1;
	else if (0 == memcmp(pszTopic, TEXT("[Portfolio]"), 11*sizeof(TCHAR)) || 0 == memcmp(pszTopic, TEXT("Portfolio"), 9*sizeof(TCHAR)))
		countedTable = 1;
	if (countedTable)
		pMyTopic = new CMyTopicCounted();
	else
		pMyTopic = new CMyTopic();
#endif
	if (pMyTopic != NULL)
	{
		m_listOfMyTopics.push_back(pMyTopic);
		pMyTopic->Create(pszTopic);
	}
	return pMyTopic;
}

void CMyServer::ClearCache()
{
	ListOfMyTopics::const_iterator it;
	for (it = m_listOfMyTopics.begin(); it != m_listOfMyTopics.end(); it++)
	{
		CMyTopic * pMyTopic = *it;

		pMyTopic->ClearLines(0);
#if 1
		if (pMyTopic->m_table.header.buf != NULL)
		{
			delete pMyTopic->m_table.header.buf;
			pMyTopic->m_table.header.buf = NULL;
		}
		pMyTopic->m_table.header.size = 0;
		pMyTopic->m_table.header.count = 0;
#else
#endif
		if (pMyTopic->m_table.data.buf != NULL)
		{
			delete pMyTopic->m_table.data.buf;
			pMyTopic->m_table.data.buf = NULL;
		}
		pMyTopic->m_table.data.size = 0;
		pMyTopic->m_table.data.count = 0;

	} // for (it)
}


void CMyStringItem::OnPoke()
{
#ifdef _DEBUG
    TRACE("%s is now %s", (LPCTSTR)m_strName, GetData());
#endif
}

//
// class CMyTopic
//
CMyTopic::CMyTopic() : m_counted(FALSE)
{
	m_table.header.buf = NULL;
	m_table.header.size = 0;
	m_table.header.count = 0;

	m_table.data.buf = NULL;
	m_table.data.size = 0;
	m_table.data.count = 0;


}

CMyTopic::~CMyTopic()
{
	ClearLines();
}

BOOL CMyTopic::Exec(void* pData, DWORD dwSize)
{
   // STATUS("Exec: %s", (char*)pData);
    return TRUE;
}

BOOL CMyTopic::Poke(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize)
{
	// Кэшируем заголовок таблицы (если присутствует):
	if (TRUE == CacheTable(NULL, pData, dwSize))
	{
		m_table.data.count = 0;
		ClearLines(0);
	}
	//else
	{// Кэшируем данные:
		if (IsCounted())
		{
			IterateCounted(wFmt, pszItem, pData, dwSize);
		}
		else
		{		
			if (m_table.data.buf != NULL && dwSize > m_table.data.size)
				delete [] m_table.data.buf;		
			if (m_table.data.buf == NULL)
			{
				DWORD bufSize = max(0x1000, dwSize);
				m_table.data.size = bufSize;
				m_table.data.buf = new BYTE [bufSize];
			}
			m_table.data.count = dwSize;
			if (m_table.data.buf != NULL)
			{
				memcpy(m_table.data.buf, pData, dwSize);
			}
		}
	}
	return TRUE;
}

BOOL CMyTopic::PokeCounted(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize)
{

	return TRUE;
}

BOOL CMyTopic::CacheTable(LPCTSTR name, const void* data, DWORD size)
{
	BOOL cached = FALSE;
	const BYTE * pBegin = (const BYTE*)data;
	const BYTE * pSrc = pBegin;
	const BYTE * pLineData = NULL;
	const BYTE * pRowCount = NULL;
	WORD tdt, cb;
	int icb;
	int cols, rows, icol, irow;
	int header = 0;

	cols = rows = icol = irow = 0;
	
	BOOL newRow = FALSE;
	int nrColStrs = 0;

	while (pSrc < pBegin + size)
	{
		if (icol == 0)
			pLineData = pSrc;
		memcpy(&tdt, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		memcpy(&cb, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		if (tdt == TDT_STRING)
		{
			for (icb = 0; icb < cb; )
			{
				int len = *pSrc++; ++icb;
				pSrc += len; icb += len;
				++nrColStrs;
				++icol;
			} // for (icb)
			if (icol == cols)
			{
				//if (nrColStrs == cols)
				{// Только строки - скорее всего это заголовок таблицы
#if 0
					size = (DWORD)(pSrc - pBegin);
					if (m_table.header.buf != NULL && size > m_table.header.size)
						delete [] m_table.header.buf;		
					if (m_table.header.buf == NULL)
					{
						DWORD bufSize = max(0x1000, size);
						m_table.header.size = bufSize;
						m_table.header.buf = new BYTE [bufSize];
					}
					m_table.header.count = size;
					if (m_table.header.buf != NULL)
					{
						// Сохраняем данные:
						memcpy(m_table.header.buf, pBegin, size); 
						// Подменяем количество строк (только заголовок):
						rows = 1;
						memcpy(m_table.header.buf + (pRowCount - pBegin), &rows, sizeof(WORD)); 
					}
#else
					header = 1;
#endif
				}
			}
			break;
		}
		else if (tdt == TDT_TABLE)
		{
			memcpy(&rows, pSrc, sizeof(WORD)); pRowCount = pSrc; pSrc += sizeof(WORD);
			memcpy(&cols, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		}
		else
		{// Обрабатываем только строки!
			break;
		}
	} // while

#if 1
	if (header)
	{
		if (IsCounted())
		{
			int dataSize = (int)(pSrc - pLineData);
			UpdateLine(0, pLineData, dataSize, NULL, 0);
		}
		else
		{
			CreateTableHeader(pBegin, size);
		}
		cached = TRUE;
	}
#endif
	return cached;
}

BOOL CMyTopic::CreateTableHeader(const void * data, DWORD size)
{
	if (m_table.header.buf != NULL && size > m_table.header.size)
	{
		delete [] m_table.header.buf;
		m_table.header.buf = NULL;
	}
	if (m_table.header.buf == NULL)
	{
		DWORD bufSize = max(0x1000, size);
		m_table.header.size = bufSize;
		m_table.header.buf = new BYTE [bufSize];
	}
	m_table.header.count = size;
	if (m_table.header.buf != NULL)
	{
		if (data != NULL)
		{// Сохраняем данные:
			memcpy(m_table.header.buf, data, size); 
		}
	}

	return TRUE;
}

int CMyTopic::IterateCounted(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD size)
{
	int count = 0;

	const BYTE * pBegin = (const BYTE*)pData;
	const BYTE * pSrc = pBegin;
	const BYTE * pLineData = NULL;
	WORD tdt, cb;
	int icb;
	int cols, rows, icol, irow;
	char str[MAX_PATH];	

	cols = rows = icol = irow = 0;

	int iColLineNum;
	int lineNum = -1;
	BOOL initialized = FALSE;

	iColLineNum = 0;
	
	BOOL newRow = FALSE;
	int nrColStrs = 0;

	int header = 0;

	CHAR name[SIZEOF_ARRAY(str)];
	int nameLen = 0;
	LPCSTR pName = NULL;

	while (pSrc < pBegin + size)
	{
		if (icol == 0)
		{
			pLineData = pSrc;
			nameLen = 0;
			pName = NULL;
		}
		memcpy(&tdt, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		memcpy(&cb, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		if (tdt == TDT_FLOAT)
		{
			double val;
			for (icb = 0; icb < cb; )
			{
				memcpy(&val, pSrc, sizeof(double)); icb += sizeof(double); pSrc += sizeof(double);
				if (icol == iColLineNum)
					lineNum = (int)val;
				++icol;
			} // for (icb) 		
			if (icol == cols)
				newRow = TRUE;
		} // if (tdt == TDT_FLOAT)
		else if (tdt == TDT_STRING)
		{
			for (icb = 0; icb < cb; )
			{
				int len = *pSrc++; ++icb;
				if (len > SIZEOF_ARRAY(str) - 1)
					len = SIZEOF_ARRAY(str) - 1;
				memcpy(str, pSrc, len); pSrc += len; icb += len;
				str[len] = '\0';
				if (irow == 0) // && table.initialized == FALSE)
				{
					if (len == 0 && icol == 0)
					{
						header = 1;
#if 0
						iColLineNum = icol;
#endif
					}
				} // if (irow == 0)
				//if (! header)
				{
					if (icol == 0 && len > 0)
					{
						memcpy(name, str, len);
						name[len] = '\0';
						nameLen = len;
						pName = name;
					}
				}
				if (initialized)
				{
				}
				++icol;
				++nrColStrs;
			} // for (icb)
			if (icol == cols)
				newRow = TRUE;
		}
		else if (tdt == TDT_TABLE)
		{
			memcpy(&rows, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
			memcpy(&cols, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);

			this->m_table.rows = rows;
			this->m_table.cols = cols;
		}

		if (newRow)
		{
			if (irow == 0)
			{
				if (nrColStrs == cols)
				{
					initialized = TRUE;
				}
			}
			if (! header)
			{
#if 0
				if (lineNum > 0)
#endif
				{
					int dataSize = static_cast<int>(pSrc - pLineData);
					UpdateLine(lineNum, pLineData, dataSize, pName, nameLen);
					++count;
				}
			}
			++irow; icol = 0;
			nrColStrs = 0;
			newRow = FALSE;
			header = 0;
			lineNum = -1;
		} // if (newRow)
	} // while
	return count;
}

int CMyTopic::CreateLine(Line * pLine, const void* pData, int size, LPCSTR name, int len)
{
	pLine->buffer.Allocate(size);
	pLine->buffer.Write((const BYTE*)pData, size);
	pLine->nameLen = len;
	if (len > 0)
		strcpy_s(pLine->name, SIZEOF_ARRAY(pLine->name), name);
#if 0
	else
		memset(pLine->name, 0, sizeof(pLine->name));
#endif
	return 0;
}

int CMyTopic::UpdateLine(Line * pLine, const void* pData, int size)
{
	pLine->buffer.Release();
	pLine->buffer.Allocate(size);
	pLine->buffer.Write((const BYTE*)pData, size);
	return 0;
}

int CMyTopic::DestroyLine(Line * pLine)
{
	pLine->buffer.Release();
	return 0;
}

int CMyTopic::AddLine(int n, const void* pData, int size, LPCSTR name, int len)
{
	Line * pLine = new Line();
	if (pLine != NULL)
	{
		if (n < 0)
			n = static_cast<int>(m_listOfLines.size());
		pLine->n = n;
		CreateLine(pLine, pData, size, name, len);
		m_listOfLines.push_back(pLine);
	}
	return 0;
}

int CMyTopic::UpdateLine(int n, const void* pData, int size, LPCSTR name, int len)
{
	Line * pLine = NULL;
#if 0
	if (n >= 0)
#endif
		pLine = FindLine(n, name, len);
	if (pLine != NULL)
		UpdateLine(pLine, pData, size);
	else
		AddLine(n, pData, size, name, len);
	return 0;
}

CMyTopic::Line * CMyTopic::FindLine(int n, LPCSTR name, int len)
{
	ListOfLines::iterator it;
	for (it = m_listOfLines.begin(); it != m_listOfLines.end(); it++)
	{
		CMyTopic::Line * pLine = (*it);
		if (n >= 0)
		{
			if (pLine->n == n)
				return pLine;
		}
		else if (pLine->nameLen > 0 && len > 0)
		{
			if (0 == strcmp(pLine->name, name))
				return pLine;
		}
	} // for (it)
	return NULL;
}

void CMyTopic::ClearLines(int exceptNum)
{
	if (m_listOfLines.size() > 0)
	{
		ListOfLines::iterator it;
		for (it = m_listOfLines.begin(); it != m_listOfLines.end(); )
		{
			CMyTopic::Line * pLine = (*it);
			if (pLine->n != exceptNum)
			{
				m_listOfLines.erase(it++);
				DestroyLine(pLine);
				delete pLine;
			}
			else
				it++;
		}
	}
}

//
// class CMyTopicBids
//

CMyTopicCurrent::CMyTopicCurrent()
{
	m_type = TABLE_CURRENT;
}

BOOL CMyTopicCurrent::Poke(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize)
{
	return CMyTopic::Poke(wFmt, pszItem, pData, dwSize);
}

//
// class CMyTopicQuotes
//

CMyTopicQuotes::CMyTopicQuotes()
{
	m_type = TABLE_QUOTES;
}

BOOL CMyTopicQuotes::Poke(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize)
{
	return CMyTopic::Poke(wFmt, pszItem, pData, dwSize);
}


//
// class CMyTopicCounted
//

CMyTopicCounted::CMyTopicCounted()
{
	m_counted = TRUE;
	AddLine(0, NULL, 0, NULL, 0);
}

BOOL CMyTopicCounted::Poke(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize)
{
	return CMyTopic::Poke(wFmt, pszItem, pData, dwSize);
}

//
// class CMyTopicBids
//

CMyTopicBids::CMyTopicBids()
{
	m_type = TABLE_BIDS;
}

BOOL CMyTopicBids::Poke(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize)
{
	return CMyTopic::Poke(wFmt, pszItem, pData, dwSize);
}

//
// class CMyTopicDeals
//

CMyTopicDeals::CMyTopicDeals()
{
	m_type = TABLE_DEALS;
}

BOOL CMyTopicDeals::Poke(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize)
{
	return CMyTopic::Poke(wFmt, pszItem, pData, dwSize);
}

//
// class CMyTopicStops
//

CMyTopicStops::CMyTopicStops()
{
	m_type = TABLE_STOPS;
}

BOOL CMyTopicStops::Poke(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize)
{
	return CMyTopic::Poke(wFmt, pszItem, pData, dwSize);
}


//
// class CMyTopicLimits
//

CMyTopicLimits::CMyTopicLimits()
{
	m_type = TABLE_LIMITS;
}

BOOL CMyTopicLimits::Poke(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize)
{
	return CMyTopic::Poke(wFmt, pszItem, pData, dwSize);
}

//
// class SListCtrl
//

int SListCtrl::Init()
{
	int status = -1;
	if (this->pListHead == NULL)
	{
		PSLIST_HEADER pListHead = (PSLIST_HEADER)_aligned_malloc(sizeof(SLIST_HEADER), MEMORY_ALLOCATION_ALIGNMENT);
		if (pListHead != NULL)
		{
			::InitializeSListHead(pListHead);
			this->pListHead = pListHead;
			status = S_OK;
		}
	}
	else
		status = S_OK;
	return status;
}

int SListCtrl::Push(LPCTSTR pszTopic, void* pData, DWORD size)
{
	int status = -1;
	PSLIST_HEADER pListHead = this->pListHead;
	if (pListHead != NULL)
	{
		int len = lstrlen(pszTopic);
		TCHAR zero = 0;
		SListItem * pItem = (SListItem*)_aligned_malloc(
			sizeof(SListItem) + sizeof(len) + len*sizeof(TCHAR) + sizeof(zero) + sizeof(size) + size, 
			MEMORY_ALLOCATION_ALIGNMENT);
		if (pItem != NULL)
		{
			BYTE * pDst = ((BYTE*)pItem) + sizeof(SLIST_ENTRY);
			memcpy(pDst, &len, sizeof(len)); pDst += sizeof(len);
			memcpy(pDst, pszTopic, len*sizeof(TCHAR)); pDst += len*sizeof(TCHAR);
			memcpy(pDst, &zero, sizeof(zero)); pDst += sizeof(zero);
			memcpy(pDst, &size, sizeof(size)); pDst += sizeof(size);
			memcpy(pDst, pData, size);
			PSLIST_ENTRY pListEntry = ::InterlockedPushEntrySList(pListHead, &(pItem->ItemEntry));
			status = S_OK;
		}
	}
	else
		status = E_HANDLE;
	return status;
}

PSLIST_ENTRY SListCtrl::Pop(LPCTSTR * ppszTopic, void** ppData, DWORD * pSize)
{
	int status = -1;
	PSLIST_ENTRY pListEntry;
	PSLIST_HEADER pListHead = this->pListHead;
	if (pListHead != NULL)
	{
		pListEntry = ::InterlockedPopEntrySList(pListHead);
		if (pListEntry != NULL)
		{
			BYTE * pSrc = ((BYTE*)pListEntry) + sizeof(SLIST_ENTRY);
			DWORD size;	
			int len;
			TCHAR zero;
			memcpy(&len, pSrc, sizeof(len)); pSrc += sizeof(len);
			LPCTSTR pStr = (LPCTSTR)pSrc;
			*ppszTopic = pStr;
			pSrc += len*sizeof(TCHAR) + sizeof(zero);
			memcpy(&size, pSrc, sizeof(size)); pSrc += sizeof(size);
			*pSize = size;
			*ppData = pSrc;
			status = S_OK;
		}		
	}
	else
		status = E_HANDLE;
	return pListEntry;
}

int SListCtrl::Destroy()
{
	int status = this->Clear();
	return status;
}

int SListCtrl::Clear()
{
	int status = -1;
	PSLIST_HEADER pListHead = this->pListHead;
	if (pListHead != NULL)
	{
		PSLIST_ENTRY pListEntry;
		while (pListEntry = ::InterlockedPopEntrySList(pListHead))
		{
			_aligned_free(pListEntry);
		}
		::InterlockedFlushSList(pListHead);
		status = S_OK;
	}
	else
		status = E_HANDLE;
	return status;
}

void SListCtrl::Release(PSLIST_ENTRY pListEntry)
{
	_aligned_free(pListEntry);
}

int SListCtrl::GetNrItems() const
{
	int n = 0;
	PSLIST_HEADER pListHead = this->pListHead;
	if (pListHead != NULL)
		n = (int)::QueryDepthSList(pListHead);
	return n;
}
