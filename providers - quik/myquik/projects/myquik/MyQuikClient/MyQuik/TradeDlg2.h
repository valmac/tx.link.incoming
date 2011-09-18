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
 *  TradeDlg2.h
 */

#pragma once

#include "TradeDlg.h"


class CTradeDlg2 : public CTradeDlg
{
public:
	CTradeDlg2();

protected:
	virtual BOOL InitDialog(CWindow wndFocus, LPARAM lInitParam);

protected:
	virtual void IterateTableQuotes(LPCTSTR name, const BYTE * data, DWORD size);
	virtual void IterateTableCurrent(LPCTSTR name, const BYTE * data, DWORD size);
	virtual void IterateTableBids(LPCTSTR name, const BYTE * data, DWORD size, const Transaction * pExtTa = NULL);
	virtual void IterateTableDeals(LPCTSTR name, const BYTE * data, DWORD size, const Transaction * pExtTa = NULL);
	virtual void IterateTableStops(LPCTSTR name, const BYTE * data, DWORD size, const Transaction * pExtTa = NULL);
	virtual void IterateTablePortfolio(LPCTSTR name, const BYTE * data, DWORD size);
	virtual void IterateTableLimits(LPCTSTR name, const BYTE * data, DWORD size);

protected:
	class Table
	{
	public:
		Table() {initialized = FALSE; iColSecCode = iColClass = iColAccount = iColClient = iColDate = iColTime = -1;}
		int iColType, iColSecCode, iColClass, iColAccount, iColClient, iColDate, iColTime;
		BOOL initialized;
	};

	class TableCurrent : public Table
	{
	public:
		TableCurrent() 	
		{iColSecCode = iColClass = iColAccount = iColPrice = iColPriceMin = iColPriceMax = iColDemand = iColSupply = iColPercent = iColPriceBid = iColPriceOffer = -1;}
		int iColPrice, iColPriceMin, iColPriceMax, iColDemand, iColSupply, iColPercent, iColPriceBid, iColPriceOffer;
	};

	class TableBD : public Table
	{
	public:
		TableBD() {iColId = 0; iColLine = iColNumber = iColOperation = iColPrice = iColQuantity = iColVolume = iColStatus = iColResult = -1;}
		int iColId, iColLine, iColNumber, iColOperation, iColPrice, iColQuantity, iColVolume, iColStatus, iColResult;
	};

	class TableBids : public TableBD
	{
	public:
		TableBids() {}
	};

	class TableDeals : public TableBD
	{
	public:
		TableDeals() {}
	};

	class TableStops : public TableBD
	{
	public:
		TableStops() {iColOrderNumber = iColStopPrice = -1;}
		int iColOrderNumber, iColStopPrice;
	};

	class TableLimitsBase : public Table
	{
	public:
		TableLimitsBase() {iColLimit = iColLimitPrev = iColVarMargin = iColIncome = iColDues = -1;}
		int iColLimit, iColLimitPrev, iColVarMargin, iColIncome, iColDues;
	};

	class TablePortfolio : public TableLimitsBase
	{
	public:
		TablePortfolio() {}
	};

	class TableLimits : public TableLimitsBase
	{
	public:
		TableLimits() {}
	};

	TableCurrent m_tableCurrent;
	TableBids m_tableBids;
	TableDeals m_tableDeals;
	TableStops m_tableStops;
	TablePortfolio m_tablePortfolio;
	TableLimits m_tableLimits;

protected:
#if LOAD_TRADE_IN_SEPARATE_THREAD
	static DWORD WINAPI LoadTradeThreadFunc(LPVOID lpThreadParameter);
	virtual int BeginLoadTrade();
	virtual int EndLoadTrade();
#endif
	virtual void OnBeginLoadTrade();
	virtual void OnEndLoadTrade();

	virtual int SaveTrade();
	virtual int LoadTrade();

protected:
	struct LoadTradeCtrl {
		my::lib::Thread thread;
	} m_loadTrade;

protected:
	virtual void BuySell(int buy, int flags);
	virtual void BuySell(int buy, double price, double quantity, int flags);
	virtual void StopBid(int buy, int flags);
	virtual void StopBid(int buy, double price, double quantity, int flags);

public:
	virtual int OnAction(Action::Type action, int flags = 0, HWND hWnd = NULL);

protected:
	typedef int (CTradeDlg2::*PFuncOnAction)(int flags, HWND hWnd);
	typedef std::vector<PFuncOnAction> TableOfActions;

	int OnActionUnknown(int flags, HWND hWnd);

	int OnActionBuy(int flags, HWND hWnd);
	int OnActionSell(int flags, HWND hWnd);
	int OnActionBuyMarket(int flags, HWND hWnd);
	int OnActionSellMarket(int flags, HWND hWnd);
	int OnActionBuyFixed(int flags, HWND hWnd);
	int OnActionSellFixed(int flags, HWND hWnd);
	int OnActionBuyCurrent(int flags, HWND hWnd);
	int OnActionSellCurrent(int flags, HWND hWnd);
	int OnActionBuyTa(int flags, HWND hWnd);
	int OnActionSellTa(int flags, HWND hWnd);
	int OnActionStopOrder(int flags, HWND hWnd);
	int OnActionStopBuy(int flags, HWND hWnd);
	int OnActionStopSell(int flags, HWND hWnd);
	int OnActionStopBuyOffset(int flags, HWND hWnd);
	int OnActionStopSellOffset(int flags, HWND hWnd);
	int OnActionStopBuyTa(int flags, HWND hWnd);
	int OnActionStopSellTa(int flags, HWND hWnd);
	int OnActionStopBuyOffsetTa(int flags, HWND hWnd);
	int OnActionStopSellOffsetTa(int flags, HWND hWnd);
	int OnActionStopBidTa(int flags, HWND hWnd);
	int OnActionChangeBid(int flags, HWND hWnd);
	int OnActionCancelBids(int flags, HWND hWnd);
	int OnActionCancelBidsAll(int flags, HWND hWnd);
	int OnActionCancelBidsBuy(int flags, HWND hWnd);
	int OnActionCancelBidsSell(int flags, HWND hWnd);
	int OnActionCancelBidsAllStop(int flags, HWND hWnd);
	int OnActionTradeZero(int flags, HWND hWnd);
	int OnActionTradeReverse(int flags, HWND hWnd);
	int OnActionInstrumentPrevios(int flags, HWND hWnd);
	int OnActionInstrumentNext(int flags, HWND hWnd);
	int OnActionShowInstrument(int flags, HWND hWnd);
	int OnActionEditInstrument(int flags, HWND hWnd);
	int OnActionShowHideWindowDeals(int flags, HWND hWnd);
	int OnActionShowHideWindowGlass(int flags, HWND hWnd);
	int OnActionShowHideWindowMessages(int flags, HWND hWnd);
	int OnActionShowHideWindowHistory(int flags, HWND hWnd);
	int OnActionChangeButtons(int flags, HWND hWnd);
	int OnActionShowTransactionInfo(int flags, HWND hWnd);
	int OnActionShowActiveBidsAndDeals(int flags, HWND hWnd);
	int OnActionShowRarefiedGlass(int flags, HWND hWnd);
	int OnActionShowNeutralZone(int flags, HWND hWnd);
	int OnActionSettings(int flags, HWND hWnd);	

	int OnActionGlassBidFixed(int flags, HWND hWnd);
	int OnActionGlassBuyFixed(int flags, HWND hWnd);
	int OnActionGlassSellFixed(int flags, HWND hWnd);
	int OnActionGlassBidMarket(int flags, HWND hWnd);
	int OnActionGlassBuyMarket(int flags, HWND hWnd);
	int OnActionGlassSellMarket(int flags, HWND hWnd);
	int OnActionGlassBidCurrent(int flags, HWND hWnd);
	int OnActionGlassBuyCurrent(int flags, HWND hWnd);
	int OnActionGlassSellCurrent(int flags, HWND hWnd);
	int OnActionGlassStopBid(int flags, HWND hWnd);
	int OnActionGlassStopBuy(int flags, HWND hWnd);
	int OnActionGlassStopSell(int flags, HWND hWnd);
	int OnActionGlassStopBidOffset(int flags, HWND hWnd);
	int OnActionGlassStopBuyOffset(int flags, HWND hWnd);
	int OnActionGlassStopSellOffset(int flags, HWND hWnd);
	int OnActionGlassCancelBid(int flags, HWND hWnd);
	int OnActionGlassCancelAll(int flags, HWND hWnd);

	int OnActionGlass(int flags, HWND hWnd, Action::Type action);

	int OnActionGlassSelectItem(int flags, HWND hWnd);
	int OnActionGlassPopupMenu(int flags, HWND hWnd);

	int OnActionFreeze(int flags, HWND hWnd);
#if USE_ACTION_CAPTURE_VALUE
	int OnActionCaptureValue(int flags, HWND hWnd);
#endif
protected:
	void InitTableOfActions(int size);


protected:
	TableOfActions m_actions;

}; // class CTradeDlgEx

