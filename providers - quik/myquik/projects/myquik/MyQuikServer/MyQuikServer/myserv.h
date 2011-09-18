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
 *  myserv.h
 */

#ifndef _MYSERV_
#define _MYSERV_

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
#include <mydde.h>
#include <buffer.h>
#include <thread.h>

enum {
	TABLE_CURRENT,
	TABLE_BIDS,
	TABLE_DEALS,
	TABLE_STOPS,
	TABLE_QUOTES,
	TABLE_LIMITS,
	TABLE_PORTFOLIO,
};

class CMyStringItem : public CDDEStringItem
{
protected:
    virtual void OnPoke();
};

class CMyTopic : public CDDETopic
{
	friend class CMyServer;
public:
    CMyTopic();
	~CMyTopic();
    virtual BOOL Exec(void* pData, DWORD dwSize);
	virtual BOOL Poke(UINT wFmt, LPCTSTR pszItem, void* data, DWORD dwSize);
	BOOL CacheTable(LPCTSTR name, const void* data, DWORD size);

	struct Line {
		my::lib::Buffer<BYTE, int> buffer;
		int n;
		CHAR name[256];
		int nameLen;
	};

	typedef std::list<Line*> ListOfLines;

	BOOL IsCounted() const { return m_counted; }

protected:
	BOOL PokeCounted(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize);
	int IterateCounted(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize);
	
	int CreateLine(Line * pLine, const void* pData, int size, LPCSTR name, int len);
	int UpdateLine(Line * pLine, const void* pData, int size);
	int DestroyLine(Line * pLine);
	int AddLine(int n, const void* pData, int size, LPCSTR name, int len);
	int UpdateLine(int n, const void* pData, int size, LPCSTR name, int len);
	Line * FindLine(int n, LPCSTR name = NULL, int len = 0);
	void ClearLines(int exceptNum = -1);

	BOOL CreateTableHeader(const void * data, DWORD size);

protected:
	int m_type;
	BOOL m_counted;

	struct Table {
		struct Header {
			BYTE * buf;
			DWORD size;
			DWORD count;
		} header;

		struct Data {
			BYTE * buf;
			DWORD size;
			DWORD count;
		} data;

		int rows;
		int cols;
	} m_table;

	ListOfLines m_listOfLines;
};

class CMyTopicCounted : public CMyTopic
{
public:
    CMyTopicCounted();
	virtual BOOL Poke(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize);
};

class CMyTopicCurrent : public CMyTopicCounted
{
public:
    CMyTopicCurrent();
	virtual BOOL Poke(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize);
};

class CMyTopicQuotes : public CMyTopic
{
public:
    CMyTopicQuotes();
	virtual BOOL Poke(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize);
};

class CMyTopicBids : public CMyTopicCounted
{
public:
    CMyTopicBids();
	virtual BOOL Poke(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize);
};

class CMyTopicDeals : public CMyTopicCounted
{
public:
    CMyTopicDeals();
	virtual BOOL Poke(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize);
};

class CMyTopicStops : public CMyTopicCounted
{
public:
    CMyTopicStops();
	virtual BOOL Poke(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize);
};

class CMyTopicLimits : public CMyTopicCounted
{
public:
    CMyTopicLimits();
	virtual BOOL Poke(UINT wFmt, LPCTSTR pszItem, void* pData, DWORD dwSize);
};

struct SListItem {
	SLIST_ENTRY ItemEntry;
};

class SListCtrl {
public:
	SListCtrl() : pListHead(NULL)
	{}

	int Init();
	int Destroy();
	int Clear();
	int Push(LPCTSTR pszTopic, void* pData, DWORD dwSize);
	PSLIST_ENTRY Pop(LPCTSTR * ppszTopic, void** ppData, DWORD * pSize);
	int GetNrItems() const;

	static void Release(PSLIST_ENTRY pListEntry);

public:
	PSLIST_HEADER pListHead;
};

//
// class CMyServer
//
class CMyServer : public CDDEServer
{
public:
    CMyServer();
    virtual ~CMyServer();

	virtual void Status(const char * pszFormat, ...);
	virtual void Shutdown(int flags = 0);
    virtual BOOL OnCreate();
	virtual BOOL Poke(UINT wFmt, LPCTSTR pszTopic, LPCTSTR pszItem, void* pData, DWORD dwSize);
	BOOL PokeCache(LPCTSTR pszTopic, void* pData, DWORD dwSize);

	void SetParentWindow(HWND hWnd) { m_hWnd = hWnd; }

	void OnClientMessage(void * pData);

	void EnableCaching(BOOL enable = TRUE);

protected:
	void SetClientConnected(int connected = TRUE);
	BOOL IsClientConnected() const;

	CMyTopic * FindTopic(LPCTSTR pszTopic);
	CMyTopic * CreateTopic(LPCTSTR pszTopic);

	void ClearCache();

	int SendDataToClient(LPCTSTR pszTopic, void* pData, DWORD dwSize);
	int WriteDataToFileMap(LPCTSTR pszTopic, void* pData, DWORD dwSize);

	static DWORD WINAPI ThreadFunc(LPVOID lpThreadParameter);
#if SEPARATE_THREAD_TO_SEND_DATA
	static DWORD WINAPI SendDataToClientThreadFunc(LPVOID lpThreadParameter);
	int CreateThreadSendDataToClient();
	int CloseThreadSendDataToClient();
#endif

	virtual int OnXTypConnect(LPCTSTR name);
	virtual int OnXTypDisconnect(HCONV hConv);

protected:
	typedef std::list<CMyTopic*> ListOfMyTopics;

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

	my::lib::Thread m_thread;
#if SEPARATE_THREAD_TO_SEND_DATA
	my::lib::Thread m_threadSendDataToClient;
	SListCtrl m_slistCtrl;
#endif

	HWND m_hWnd;

	struct Client {
		volatile int connected;
	} m_client;

	struct Caching {
		volatile BOOL enable;
	} m_caching;

public:
    CMyTopic m_DataTopic;
    CMyStringItem m_StringItem1;
    CMyStringItem m_StringItem2;
	ListOfMyTopics m_listOfMyTopics;
};


#endif // _MYSERV_
