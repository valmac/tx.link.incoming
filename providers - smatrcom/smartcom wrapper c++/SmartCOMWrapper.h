// SmartCOMWrapper.h : declaration file
//

#pragma once

#include <stdint.h>

#import "C:\\Program Files\\SmartCOM\\stcln-2.dll" no_namespace named_guids

namespace tx { namespace smartcom {

typedef IStServerPtr SC_PTR;
typedef uint32_t COOKIE;

/**
\class smartif
Wrapper class for the SMARTCOM connector

\note Board point from application to the SMARTCOM, uses storage and receiving from the ringbuffers
\note Used proprietary messaging protocol and a data translation with data types defined in another headers
*/
class smartif
{
public:
	smartif();
	virtual ~smartif();

	/// Returns connection ready status
	bool ready() const { return __ready; }
	
	/// Returns connection idle status
	bool idle() const { return __idle; }
	
	/// Returns connection is online
	bool online() const { return __online; }
	
	/// Initializes connection
	bool init();
	
	/// Terminates connection
	void terminate();
	
	/// Logges message with given timestamp
	bool log(const time_t ts_, const LPCTSTR msg_);

private:
	SC_PTR		__com_ptr;							/// Pointer to COM server instance
	HANDLE		__smart_handle;						/// Handle of COM server instance
	FILE*		__logfile;							/// system log file handle

	COOKIE		__cookie;							/// Cookie?

	bool		__ready;							/// Ready signature
	bool		__idle;								/// Idle signature
	bool		__online;							/// Online signature

public:
	BOOL m_bMessageBoxOnComException;
	BOOL m_bLogIsConnected;
	BOOL m_bLogconnect, m_bLogConnected;
	BOOL m_bLogdisconnect, m_bLogDisconnected;
	BOOL m_bLogGetSymbols, m_bLogAddSymbol;
	BOOL m_bLogListenQuotes, m_bLogCancelQuotes, m_bLogUpdateQuote;
	BOOL m_bLogListenBidAsks, m_bLogCancelBidAsks, m_bLogUpdateBidAsk;
	BOOL m_bLogListenTicks, m_bLogCancelTicks, m_bLogAddTick;
	BOOL m_bLogGetBars, m_bLogAddBar, m_bLogAddTickHistory;
	BOOL m_bLogGetPortfolioList, m_bLogAddPortfolio;
	BOOL m_bLogListenPortfolio, m_bLogCancelPortfolio, m_bLogSetPortfolio;
	BOOL m_bLogUpdatePosition, m_bLogAddTrade;
	BOOL m_bLogPlaceOrder, m_bLogOrderSucceeded, m_bLogOrderFailed;
	BOOL m_bLogCancelOrder, m_bLogOrderCancelSucceeded, m_bLogOrderCancelFailed;
	BOOL m_bLogMoveOrder, m_bLogOrderMoveSucceeded, m_bLogOrderMoveFailed, m_bLogUpdateOrder;
	BOOL m_bLogCancelAllOrders;
	BOOL m_bLogGetTrades;
	BOOL m_bLogGetMyOrders, m_bLogSetMyOrder;
	BOOL m_bLogGetMyTrades, m_bLogSetMyTrade;
	BOOL m_bLogGetMyOpenPos, m_bLogSetMyOpenPos;
	BOOL m_bLogGetMyClosePos, m_bLogSetMyClosePos;
	BOOL m_bLogCheckSubscription, m_bLogSetSubscriptionCheckResult;

private:
	bool ListenQuotes(LPCTSTR symbol);
	bool CancelQuotes(LPCTSTR symbol);
	bool ListenBidAsks(LPCTSTR symbol);
	bool CancelBidAsks(LPCTSTR symbol);
	bool ListenTicks(LPCTSTR symbol);
	bool CancelTicks(LPCTSTR symbol);
	bool GetBars(LPCTSTR symbol, StBarInterval interval, long count, time_t since = static_cast<time_t>(-1));
	bool ListenPortfolio(LPCTSTR portfolio);
	bool CancelPortfolio(LPCTSTR portfolio);
	bool PlaceOrder(LPCTSTR portfolio, LPCTSTR symbol, StOrder_Action action, StOrder_Type type, StOrder_Validity validity, double price, double amount, double stop, long cookie);
	bool CancelOrder(LPCTSTR portfolio, LPCTSTR symbol, LPCTSTR orderid);
	bool MoveOrder(LPCTSTR portfolio, LPCTSTR orderid, double targetPrice);
	bool GetSymbols();
	bool IsConnected();
	bool connect(LPCTSTR ip, short port, LPCTSTR login, LPCTSTR password);
	bool disconnect();
	bool GetPortfolioList();
	bool CancelAllOrders();
	bool CheckSubscription(LPCTSTR subscriptionId);
	bool GetMyOrders(BOOL onlyActive, LPCTSTR portfolio);
	bool GetMyTrades(LPCTSTR portfolio);
	bool GetMyOpenPos(LPCTSTR portfolio);
	bool GetMyClosePos(LPCTSTR portfolio);
	bool GetTrades(LPCTSTR symbol, long count, time_t since = static_cast<time_t>(-1));

private:
	virtual HRESULT Connected();
	virtual HRESULT Disconnected(LPCTSTR reason);
	virtual HRESULT UpdateQuote(LPCTSTR symbol, DATE datetime, double open, double high, double low, double close, double last, double volume, double size, double bid, double ask, double bidsize, double asksize, double open_int, double go_buy, double go_sell, double go_base, double go_base_backed, double high_limit, double low_limit, long trading_status);
	virtual HRESULT UpdateBidAsk(LPCTSTR symbol, long row, long nrows, double bid, double bidsize, double ask, double asksize);
	virtual HRESULT AddTick(LPCTSTR symbol, DATE datetime, double price, double volume, LPCTSTR tradeno, StOrder_Action action);
	virtual HRESULT AddBar(long row, long nrows, LPCTSTR symbol, StBarInterval interval, DATE datetime, double open, double high, double low, double close, double volume);
	virtual HRESULT SetPortfolio(LPCTSTR portfolio, double cash, double leverage, double comission, double saldo);
	virtual HRESULT AddTrade(LPCTSTR portfolio, LPCTSTR symbol, LPCTSTR orderid, double price, double amount, DATE datetime, LPCTSTR tradeno);
	virtual HRESULT UpdateOrder(LPCTSTR portfolio, LPCTSTR symbol, StOrder_State state, StOrder_Action action, StOrder_Type type, StOrder_Validity validity, double price, double amount, double stop, double filled, DATE datetime, LPCTSTR orderid, LPCTSTR orderno, long status_mask);
	virtual HRESULT UpdatePosition(LPCTSTR portfolio, LPCTSTR symbol, double avprice, double amount, double planned);
	virtual HRESULT AddTickHistory(long row, long nrows, LPCTSTR symbol, DATE datetime, double price, double volume, LPCTSTR tradeno, StOrder_Action action);
	virtual HRESULT AddSymbol(long row, long nrows, LPCTSTR symbol, LPCTSTR short_name, LPCTSTR long_name, LPCTSTR type, long decimals, long lot_size, double punkt, double step, LPCTSTR sec_ext_id, LPCTSTR sec_exch_name, DATE expiry_date, double days_before_expiry);
	virtual HRESULT OrderSucceeded(long cookie, LPCTSTR orderid);
	virtual HRESULT OrderFailed(long cookie, LPCTSTR orderid, LPCTSTR reason);
	virtual HRESULT AddPortfolio(long row, long nrows, LPCTSTR portfolioName, LPCTSTR portfolioExch);
	virtual HRESULT SetSubscriptionCheckResult(BOOL result);
	virtual HRESULT SetMyTrade(long row, long nrows, LPCTSTR portfolio, LPCTSTR symbol, DATE datetime, double price, double volume, LPCTSTR tradeno, StOrder_Action buysell, LPCTSTR orderno);
	virtual HRESULT SetMyOrder(long row, long nrows, LPCTSTR portfolio, LPCTSTR symbol, StOrder_State state, StOrder_Action action, StOrder_Type type, StOrder_Validity validity, double price, double amount, double stop, double filled, DATE datetime, LPCTSTR id, LPCTSTR no);
	virtual HRESULT SetMyOpenPos(long row, long nrows, LPCTSTR portfolio, LPCTSTR symbol, double avprice, double amount, double planned);
	virtual HRESULT SetMyClosePos(long row, long nrows, LPCTSTR portfolio, LPCTSTR symbol, double amount, double price_buy, double price_sell, DATE postime, LPCTSTR order_open, LPCTSTR order_close);
	virtual HRESULT OrderCancelSucceeded(LPCTSTR orderid);
	virtual HRESULT OrderCancelFailed(LPCTSTR orderid);
	virtual HRESULT OrderMoveSucceeded(LPCTSTR orderid);
	virtual HRESULT OrderMoveFailed(LPCTSTR orderid);

public:
	static LPCTSTR szFmtDate;
	static LPCTSTR szFmtTime;
	static LPCTSTR szFmtDateTime;

public:
	static LPCTSTR OrderState(StOrder_State state);
	static LPCTSTR OrderAction(StOrder_Action action);
	static LPCTSTR OrderType(StOrder_Type type);
	static LPCTSTR OrderValidity(StOrder_Validity validity);
	static LPCTSTR BarInterval(StBarInterval interval);
	static time_t SystemTime2time_t(SYSTEMTIME& SysTime);
	static bool time_t2SystemTime(time_t DateTime, SYSTEMTIME& SysTime);

};
