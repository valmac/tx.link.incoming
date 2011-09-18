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
 *  myclient.h
 */

#pragma once

#include <thread.h>
#include "types.h"

#define MYQUIKDDECLIENT TEXT("MyQuikDDEClient")
#define MYQUIKDDESERVER TEXT("MyQuikServer")
#define DDE_SERVER_NAME MYQUIKDDESERVER
#define DDE_SERVER_FILE_NAME MYQUIKDDESERVER TEXT("File")
#define DDE_SERVER_MUTEX_NAME MYQUIKDDESERVER TEXT("Mutex")
#define DDE_SERVER_EVENT_NAME MYQUIKDDESERVER TEXT("Event")

#define DDE_CLIENT_SERVER_FILE_NAME MYQUIKDDESERVER TEXT("ClientToServerFile")
#define DDE_CLIENT_SERVER_MUTEX_NAME MYQUIKDDESERVER TEXT("ClientToServerMutex")
#define DDE_CLIENT_SERVER_EVENT_NAME MYQUIKDDESERVER TEXT("ClientToServerEvent")
#define DDE_CLIENT_SERVER_EVENT_NAME_2 MYQUIKDDESERVER TEXT("ClientToServerEvent2")

enum {
	CLIENT_MESSAGE_ZERO = 100,
	CLIENT_MESSAGE_CONNECT,
	CLIENT_MESSAGE_DISCONNECT,
	CLIENT_MESSAGE_CLEAR_CACHE,
};

#include <list>

class CMyTopic
{
	friend class CMyClient;
public:
    CMyTopic();

	int Create(LPCTSTR pszTopic);

protected:
	String m_name;
	int m_msg;
};

class CMyTopicCurrent : public CMyTopic
{
public:
    CMyTopicCurrent();
};

class CMyTopicQuotes : public CMyTopic
{
public:
    CMyTopicQuotes();
};

class CMyTopicBids : public CMyTopic
{
public:
    CMyTopicBids();
};

class CMyTopicDeals : public CMyTopic
{
public:
    CMyTopicDeals();
};

class CMyTopicStops : public CMyTopic
{
public:
    CMyTopicStops();
};

//
// class CMyClient
//
class CMyClient
{
public:
	struct DataPtr {
		BYTE * data;
	};
public:
    CMyClient();
    virtual ~CMyClient();

	int Create(HWND hWnd);
	int Close();
	void Shutdown();

	int ClearCacheServer();

	BOOL Poke(void* pData);
	BOOL Poke(LPCTSTR pszTopic, LPCTSTR pszItem, void* pData, DWORD dwSize);

	int GetServerStatus() const { return m_server.status; }

protected:
	CMyTopic * CreateTopic(LPCTSTR pszTopic);
	CMyTopic* FindTopic(LPCTSTR pszTopic);
	void AddTopic(CMyTopic * pTopic);

protected:
	int SendMessageToServer(int code, void * pData, int size);

	static DWORD WINAPI ThreadFunc(LPVOID lpThreadParameter);

protected:
	my::lib::Thread m_thread;

	struct IPC {
		HANDLE hFileMap;
		LPVOID lpFileMap;
		HANDLE hMutex;
		HANDLE hEvent;

		IPC() : hFileMap(NULL), lpFileMap(NULL), hMutex(NULL), hEvent(NULL)
		{}
	};

	struct IPCCtrl {
		IPC s2c; // server to client;
		IPC c2s; // client to server;
	} m_ipc;

	typedef std::list<CMyTopic*> ListOfMyTopics;
	ListOfMyTopics m_listOfMyTopics;

	HWND m_hWnd;

	BOOL m_initialized;

	struct Server {
		int status;
	} m_server;
};
