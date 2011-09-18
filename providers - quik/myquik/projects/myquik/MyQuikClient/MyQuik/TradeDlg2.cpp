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
 *  TradeDlg2.cpp
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "TradeDlg2.h"
#include "DlgGlass.h"
#include "DlgHistory.h"
#include "DlgStop2.h"
#include "DlgLog.h"
#include "DlgProperties.h"
#include "DlgAbout.h"
#include "PropertySheetSettings.h"

#include "xltable.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#endif

extern CDlgGlass dlgGlass;
extern CDlgHistory dlgHistory;
extern CDlgLog dlgLog;
extern CDlgStop2 dlgStop;

extern CPropertySheetSettings * dlgSettings;
extern CDlgProperties * dlgProperties;

CTradeDlg2::CTradeDlg2()
{
	m_loadTrade.thread.hThread = NULL;
}

BOOL CTradeDlg2::InitDialog(CWindow wndFocus, LPARAM lInitParam)
{
	InitTableOfActions(theApp.GetCurrentLayout().size());

	return CTradeDlg::InitDialog(wndFocus, lInitParam);
}


//
// IterateTable(s)
//

void CTradeDlg2::IterateTableQuotes(LPCTSTR name, const BYTE * data, DWORD size)
{
	const BYTE * pBegin = data;
	const BYTE * pSrc = pBegin;
	WORD tdt, cb;
	int icb;
	int cols, rows, icol, irow;
	char str[MAX_PATH];
	cols = rows = icol = irow = 0;

	int iColPrice, iColPriceBuy, iColPriceSell, iColBuy, iColSell, iColVolume;
	iColPrice = iColPriceBuy = iColPriceSell = -1;
	iColVolume = -1;
	iColBuy = iColSell = -1;

#if 0
	static TCHAR names[NR_QUOTE_COLUMNS_MAX][MAX_PATH];
#endif
	double vals[NR_QUOTE_COLUMNS_MAX];
	double val;

	BOOL newTableHeader = FALSE;

	BOOL newRow = FALSE;

	double price, pricePrev, priceBuyBest, priceSellBest;
	double volumeMin, volumeMax;
	double valBuy, valSell, valBuyPrev, valSellPrev;
	double priceMiddle1, priceMiddle2;

	price = pricePrev = priceBuyBest = priceSellBest = 0;
	volumeMin = DBL_MAX; volumeMax = 0;
	valBuy = valSell = valBuyPrev = valSellPrev = 0;
	priceMiddle1 = priceMiddle2 = 0;

	int irowMiddle = 0;

	QuoteTable::Properties properties;
	QuoteTable * pTable = theApp.FindTable(name);
	if (pTable != NULL)
	{// Таблица с таким названием уже есть
		properties = *(pTable->GetProperties());
	}
	else
	{		
		LoadTableProperties(name, &properties);
		pTable = theApp.CreateTable(name, &properties);
	}

	if (pTable == NULL)
		return;

	if (theApp.GetCurrentTable() == NULL)
	{
		if (! theApp.IsAnotherToolName(properties.name))
			SelectInstrument(pTable);
	}	

	double sstep;
	const double step = properties.price.step;
	const int iPrice = QuoteTable::ITEM_PRICE;
	const int maxSize = 50;
	int dd, count;
	int n;

	int genericTable = 0;

	QuoteTable::Array * pArray;
	QuoteTable::Array * pArrayBuy, * pArraySell, * pArrayGen;	
	QuoteTable::Array::Item * pItem;

	static QuoteTable::Array arrays[QuoteTable::I_LAST];

	pArrayBuy = &arrays[QuoteTable::I_BUY];
	pArraySell = &arrays[QuoteTable::I_SELL];
	pArrayGen = &arrays[QuoteTable::I_NEUTRAL];

	if (pArrayBuy->IsEmpty())
		pArrayBuy->Create(maxSize);
	if (pArraySell->IsEmpty())
		pArraySell->Create(maxSize);
	if (pArrayGen->IsEmpty())
		pArrayGen->Create(2*maxSize);

	pArrayBuy->Clear(); pArraySell->Clear(); 
	pArrayGen->Clear();

	USES_CONVERSION;

	while (pSrc < pBegin + size)
	{
		val = 0;
		memcpy(&tdt, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		memcpy(&cb, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		if (tdt == TDT_FLOAT)
		{
			for (icb = 0; icb < cb; )
			{
				memcpy(&val, pSrc, sizeof(double)); icb += sizeof(double); pSrc += sizeof(double);
				vals[icol] = val;
				++icol;
#if defined _DEBUG && 0
				TRACE("%s\t", strText);
#endif
			} // for (icb)
			if (icol == cols)
				newRow = TRUE;
#if defined _DEBUG && 0
			TRACE("\r\n");
#endif
		} // if (tdt == TDT_FLOAT)
		else if (tdt == TDT_STRING)
		{			
			for (icb = 0; icb < cb; )
			{
				int len = *pSrc++; ++icb;
				if (len == 0)
					continue;
				memcpy(str, pSrc, len); pSrc += len; icb += len;
				str[len] = '\0';
				LPCTSTR pStr = A2T(str);
				if (irow == 0)
				{// Заголовок таблицы
					if (0 == memcmp(str, "BUY_PRICE", 9) || 0 == memcmp(str, "Цена покупки", 12))
					{
						iColPriceBuy = icol;
					}
					else if (0 == memcmp(str, "SELL_PRICE", 10) || 0 == memcmp(str, "Цена продажи", 12))
					{
						iColPriceSell = icol;
					}
					else if (0 == memcmp(str, "BUY_VOLUME", 10) || 0 == memcmp(str, "Покупка", 7))
					{
						iColBuy = icol;
					}
					else if (0 == memcmp(str, "SELL_VOLUME", 11) || 0 == memcmp(str, "Продажа", 7))
					{
						iColSell = icol;
					}
					else if (0 == memcmp(str, "PRICE", 5) || 0 == memcmp(str, "Цена", 4))
					{
						iColPrice = icol;
					}
					else if (0 == memcmp(str, "VOLUME", 6) || 0 == memcmp(str, "Объем", 5))
					{
						iColVolume = icol;
					}					
#if 0
					_tcscpy_s(&names[icol][0], SIZEOF_ARRAY(names[0]) - 1, pStr);
#endif
				} // if (irow == 0)
				else
				{
				}
				++icol;
			} // for (icb)
			if (icol == cols)
				newRow = TRUE;
		}
		else if (tdt == TDT_TABLE)
		{
			memcpy(&rows, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
			memcpy(&cols, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
			irowMiddle = rows / 2;
		}
		if (newRow)
		{
			if (irow == 0)
			{
				// Очищаем таблицу:
				pTable->Clear();

				if (iColVolume >= 0)
				{// 
#if 0
					TRACE("Unsupported table format!\r\n");
					return;
#else
					genericTable = TRUE;
#endif
				}
			} // if (irow == 0)
			else
			{
				int inc = 1;
				int incBuy, incSell, incGen;
				incBuy = incSell = incGen = 0;
				pArray = NULL;
				for (icol = 0; icol < cols; icol++)
				{
					int i = -1;
					pArray = NULL;					
					if (icol == iColPrice)
					{
						if (! genericTable)
							pArray = (vals[iColBuy] == 0) ? pArraySell : pArrayBuy;
						else
							pArray = pArrayGen;
						i = QuoteTable::ITEM_PRICE;						
					}
					else if (icol == iColPriceBuy)
					{
						pArray = pArrayBuy;
						i = QuoteTable::ITEM_PRICE;
					}
					else if (icol == iColPriceSell)
					{
						pArray = pArraySell;
						i = QuoteTable::ITEM_PRICE;
					}
					else if (icol == iColBuy)
					{
						pArray = pArrayBuy;
						i = QuoteTable::ITEM_VOLUME;
					}
					else if (icol == iColSell)
					{
						pArray = pArraySell;
						i = QuoteTable::ITEM_VOLUME;
					}
					else if (icol == iColVolume)
					{
						pArray = pArrayGen;
						i = QuoteTable::ITEM_VOLUME;
					}
					
					if (i >= 0)
					{
						if (icol == 0)
						{
							if (pArray)
							{
								pArray->ClearItem(pArray->count);
							}
						}
						val = vals[icol];
						if (! (i == QuoteTable::ITEM_VOLUME && val == 0))
						{
							if (pArray->count + inc < pArray->GetCapacity())
							{
								pItem = pArray->GetItem(pArray->count);
								pItem->values[i] = val;							
								if (i == QuoteTable::ITEM_PRICE)
								{
									if (pArray == pArrayBuy)
										incBuy += inc;
									else if (pArray == pArraySell)
										incSell += inc;
									else if (pArray == pArrayGen)
										incGen += inc;
								}
								else if (i == QuoteTable::ITEM_VOLUME)
								{
									if (val < volumeMin)
										volumeMin = val;
									if (val > volumeMax)
										volumeMax = val;
								}
							}
						}
					}
				} // for (icol)	
				if (incBuy)
					pArrayBuy->count += incBuy;
				if (incSell)
					pArraySell->count += incSell;
				if (incGen)
					pArrayGen->count += incGen;
			}			
			++irow; icol = 0;
			newRow = FALSE;
		} // if (newRow)
	} // while

	// Копируем данные и заполняем пропуски:
	QuoteTable::Array * pSrcArray;
	QuoteTable::Array::Item * pSrcItem;
	for (int iArray = 0; iArray < SIZEOF_ARRAY(arrays); iArray++)
	{
		pSrcArray = &arrays[iArray];
		n = pSrcArray->count;
		if (n > 0)
		{
			int nDst = 0;

			pArray = pTable->GetArray(iArray);
			if (pArray->IsEmpty())
				pArray->Create(max(pSrcArray->GetCapacity(), n));
			else
				pArray->Clear();

			double val0, val1;
			val0 = pSrcArray->GetItem(0)->values[iPrice];
			if (n == 1)
				val1 = val0 + step;
			else
				val1 = pSrcArray->GetItem(1)->values[iPrice];
								
			int iFirst, iItem, iPrev;
			int inc;
			if (iArray == QuoteTable::I_BUY)
			{
				if (val1 > val0)
				{
					iFirst = n - 1;
					inc = -1;
				}
				else
				{
					iFirst = 0;
					inc = +1;
				}
				sstep = -step;
			}
			else // if (iArray == QuoteTable::I_SELL)
			{
				if (val1 > val0)
				{
					iFirst = 0;
					inc = +1;
				}
				else
				{
					iFirst = n - 1;
					inc = -1;
				}
				sstep = step;
			}
			iItem = iFirst;
			int i;
			for (i = 0; i < n; i++)
			{
				pSrcItem = pSrcArray->GetItem(iItem);
				val = pSrcItem->values[iPrice];
				if (i)
				{
					QuoteTable::Array::Item * pPrev = pSrcArray->GetItem(iPrev);
					double prev = pPrev->values[iPrice];
					double delta = val - prev;
					if (delta < 0)
						delta = -delta;
					dd = static_cast<int>((delta / step + 0.5));
					if (dd > 1)
					{
						double last, cur;
						last = val; cur = prev;
						for (int j = 0; j < dd - 1; j++)
						{
							if (pArray->count + (n - nDst) < pArray->GetCapacity())
							{
								pItem = pArray->GetItem(pArray->count);
								pItem->Clear();
								cur += sstep;
								pItem->values[iPrice] = cur;
								pArray->count++;
							}
							else
								break;
						} // for (j)
					}
				} // if (i)

				pItem = pArray->GetItem(pArray->count);
				*pItem = *pSrcItem;					
				pArray->count++;
				++nDst;

				iPrev = iItem;
				iItem += inc;
			} // for (i)

				
			if (! genericTable)
			{
				// Доводим до ровного счёта:
				i = pArray->count;
				count = pArray->GetCapacity();
				for (; i < count; i++)
				{
					val += sstep;
					pItem = pArray->GetItem(pArray->count);
					pItem->Clear();
					pItem->values[iPrice] = val;
					pArray->count++;
				} // for (i)
			}				
		}
	} // for (iArray)

	// Определяем лучшие цены:
	pArray = pTable->GetArray(QuoteTable::I_BUY);
#if defined _DEBUG && 0
	pArray->count = 0;
#endif
	priceBuyBest = (pArray->count) ? pArray->GetItem(0)->values[iPrice] : 0;
	pArray = pTable->GetArray(QuoteTable::I_SELL);
#if defined _DEBUG && 0
	pArray->count = 0;
#endif
	priceSellBest = (pArray->count) ? pArray->GetItem(0)->values[iPrice] : 0;
#if defined _DEBUG && DBG_BEST_PRICE
	TRACE("The best price to buy is %f\r\n", priceBuyBest);
	TRACE("The best price to sell is %f\r\n", priceSellBest);
#endif

	// Формируем нейтральную зону (между заявками):
	pSrcArray = &arrays[QuoteTable::I_NEUTRAL];
	pArrayGen = pTable->GetArray(QuoteTable::I_NEUTRAL);
	pArray = pArrayGen;
	if (pArray->count == 0 && (priceSellBest || priceBuyBest))
	{
		if (pArray->IsEmpty())
			pArray->Create(max(pSrcArray->GetCapacity(), pSrcArray->count));
		else
			pArray->Clear();		

		double delta = 0;
		if (priceSellBest && priceBuyBest)
			delta = priceSellBest - priceBuyBest;
		else
		{
			delta = (pArray->GetCapacity() / 2) * step;
			if (priceSellBest)
			{
				priceBuyBest = priceSellBest - delta;
				if (priceBuyBest < 0)
				{
					priceBuyBest = 0;
					delta = priceSellBest - priceBuyBest;
				}
			}
			else if (priceBuyBest)
			{
				priceSellBest = priceBuyBest + delta;
			}
		}
#if 1
		if (delta < 0)
			delta = -delta;
#endif
		int dd = static_cast<int>((delta / step + 0.5));
		if (dd > 1)
		{
			val = priceBuyBest;
			count = min(dd - 1, pArray->GetCapacity());
			for (int j = 0; j < count; j++)
			{										
				val += step;
				int n = pArray->count;
				pItem = pArray->GetItem(n);
				pItem->Clear();
				pItem->values[iPrice] = val;
				pArray->count++;
			} // for (j)
		}
	} // if (pArray->count == 0)
#if defined _DEBUG && 0
	int maxCount = 1;
	if (pArray->count > maxCount)
		pArray->count = maxCount;
#endif
	theApp.SetCurrentTrade(pTable->GetName());

	// Проверяем/обновляем свои заявки:
	const TradeItem * pTradeItem;

	theApp.BeginFindItem();	
	for (;;)
	{
		pTradeItem = theApp.FindItem(F_SEARCH_ANY, 0, 0, 0, 0, 0, 0);
		if (pTradeItem != NULL)
		{
			BOOL found = FALSE;
			for (int iArray = 0; iArray < QuoteTable::I_LAST; iArray++)
			{
				pArray = pTable->GetArray(iArray);
				for (int iItem = 0; iItem < pArray->count; iItem++)
				{
					pItem = pArray->GetItem(iItem);
					price = pItem->values[iPrice];
					if ((float)pTradeItem->price == (float)price)
					{
						found = TRUE;
						double quantity = pTradeItem->quantity;
						int iUser = -1;
						if (pTradeItem->action == Transaction::NEW_STOP_ORDER)
							iUser = QuoteTable::ITEM_USER_STOP_BUY;
						else if (Transaction::IsActive(pTradeItem->status))
							iUser = QuoteTable::ITEM_USER_BID_BUY; 
						else
							iUser = QuoteTable::ITEM_USER_DEAL_BUY;
						if (iUser >= 0)
						{
							iUser += static_cast<int>(pTradeItem->operation);
							pItem->values[iUser] += quantity;
							pItem->user += quantity;
						}
						break;
					}
				} // for (iItem)
				if (found)
					break;
			} // for (iArray)
		}
		else
			break;
	} // for (;;)

	if (pTable != NULL)
	{
		// Запоминаем лучшие цены:
		pTable->SetBestPrices(priceBuyBest, priceSellBest);	
#if 1
		pTable->SetPricesBidOffer(priceBuyBest, priceSellBest);
#endif
		// Запоминаем объёмы:
		pTable->SetVolumeMinMax(volumeMin, volumeMax);

		Account * pAccount = theApp.FindAccount(pTable->GetProperties()->account, 0);
		if (pAccount != NULL)
			pTable->SetAccount(pAccount);
#if 1
		if (IsGlassFrozen())
		{
			QuoteTable * pTableTmp = theApp.GetTableTmp();
			CopyPrices(*pTableTmp, *pTable);
		}
#endif
		UpdateOSD();
	}
}

void CTradeDlg2::IterateTableCurrent(LPCTSTR name, const BYTE * data, DWORD size)
{
	const BYTE * pBegin = data;
	const BYTE * pSrc = pBegin;
	WORD tdt, cb;
	int icb;
	int cols, rows, icol, irow;
	char str[MAX_PATH];	

	TableCurrent & table = m_tableCurrent;

	cols = rows = icol = irow = 0;
	
	BOOL newRow = FALSE;
	int nrColStrs = 0;

	TSTRING_SMALL(strSecCode); 
	TSTRING_SMALL(strClassCode);
	LPCTSTR pStrSecCode, pStrClassCode;
	pStrSecCode = pStrClassCode = NULL;

	double price, priceMin, priceMax, priceBid, priceOffer;
	double demand, supply;
	double percent;

	price = priceMin = priceMax = priceBid = priceOffer = 0;
	percent = 0;
	demand = supply = 0;

	while (pSrc < pBegin + size)
	{
		memcpy(&tdt, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		memcpy(&cb, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		if (tdt == TDT_FLOAT)
		{
			double val;
			for (icb = 0; icb < cb; )
			{
				memcpy(&val, pSrc, sizeof(double)); icb += sizeof(double); pSrc += sizeof(double);
				if (icol == table.iColPrice)
					price = val;
				else if (icol == table.iColPriceMin)
					priceMin = val;
				else if (icol == table.iColPriceMax)
					priceMax = val;
				else if (icol == table.iColDemand)
					demand = val;
				else if (icol == table.iColSupply)
					supply = val;
				else if (icol == table.iColPercent)
					percent = val;
				else if (icol == table.iColPriceBid)
					priceBid = val;
				else if (icol == table.iColPriceOffer)
					priceOffer = val;
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
				memcpy(str, pSrc, len); pSrc += len; icb += len;
				str[len] = '\0';
				if (irow == 0 && len > 0)// && table.initialized == FALSE)
				{
					if (0 == memcmp(str, "CODE", 4) || 0 == memcmp(str, "Код бумаги", 10))
					{table.iColSecCode = icol; table.initialized = FALSE;}
					else if (0 == memcmp(str, "low", 3) || 0 == memcmp(str, "Мин. цена", 9))
					{table.iColPriceMin = icol; table.initialized = FALSE;}
					else if (0 == memcmp(str, "high", 4) || 0 == memcmp(str, "Макс. цена", 10))
					{table.iColPriceMax = icol; table.initialized = FALSE;}
					else if (0 == memcmp(str, "biddeptht", 9) || 0 == memcmp(str, "Общ. спрос", 10))
					{table.iColDemand = icol; table.initialized = FALSE;}
					else if (0 == memcmp(str, "offerdeptht", 11) || 0 == memcmp(str, "Общ. предл.", 11))
					{ table.iColSupply = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "lastchange", 10) || 0 == memcmp(str, "% измен.закр.", 13))
					{ table.iColPercent = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "last", 4) || 0 == memcmp(str, "Цена послед.", 12))
					{ table.iColPrice = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "bid", 3) || 0 == memcmp(str, "Спрос", 5))
					{ table.iColPriceBid = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "offer", 5) || 0 == memcmp(str, "Предл.", 6))
					{ table.iColPriceOffer = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "CLASS_CODE", 4) || 0 == memcmp(str, "Код класса", 10))
					{table.iColClass = icol; table.initialized = FALSE;}
				} // if (irow == 0)
				if (table.initialized)
				{
					USES_CONVERSION;
					if (icol == table.iColSecCode)
					{
						SAFE_TSTRCPY(strSecCode, A2T(str));
						pStrSecCode = strSecCode;
					}
					else if (icol == table.iColClass)
					{
						SAFE_TSTRCPY(strClassCode, A2T(str));
						pStrClassCode = strClassCode;
					}
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
		}

		if (newRow)
		{
			BOOL initialized = table.initialized;
			if (irow == 0)
			{
				if (nrColStrs == cols)
					table.initialized = TRUE;
			}
			if (initialized)
			{
				if (table.iColSecCode >= 0)
				{
					QuoteTable * pTable = OnSecCode(NULL, pStrSecCode, pStrClassCode, NULL, NULL, 
						price, priceMin, priceMax, demand, supply, percent, priceBid, priceOffer);
#if 1
					if (pTable)
					{
						Account * pAccount = theApp.FindAccount(pTable->GetProperties()->account, 0);
						if (pAccount != NULL)
							pTable->SetAccount(pAccount);
					}
#endif
					if (theApp.GetCurrentTable() == pTable)
						this->PostMessage(UM_UPDATE_OSD);
				}
			}

			++irow; icol = 0;
			nrColStrs = 0;
			newRow = FALSE;

			price = priceMin = priceMax = priceBid = priceOffer = 0;
			percent = 0;
			demand = supply = 0;

		} // if (newRow)
	} // while
}

void CTradeDlg2::IterateTableBids(LPCTSTR name, const BYTE * data, DWORD size, const Transaction * pExtTa)
{
	const BYTE * pBegin = data;
	const BYTE * pSrc = pBegin;
	WORD tdt, cb;
	int icb;
	int cols, rows, icol, irow;
	char str[MAX_PATH];	

	TableBids & table = m_tableBids;

	cols = rows = icol = irow = 0;

	LPCTSTR pStrClassCode, pStrClientCode, pStrAccount;
	pStrClassCode = pStrClientCode = pStrAccount = NULL;
	
	Transaction ta;
	if (pExtTa == NULL)
	{
		memset(&ta, 0, sizeof(ta));
		ta.status = Transaction::STATUS_EXECUTED;
	}
	else
	{
		memcpy(&ta, pExtTa, sizeof(ta));
		pStrClassCode = ta.strClassCode;
		pStrClientCode = ta.strClientCode;
		pStrAccount = ta.strAccount;
		goto preparedTransactionEntryPoint;
	}

	BOOL newRow = FALSE;
	int nrColStrs = 0;

	while (pSrc < pBegin + size)
	{
		memcpy(&tdt, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		memcpy(&cb, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		if (tdt == TDT_FLOAT)
		{
			double val;
			for (icb = 0; icb < cb; )
			{
				memcpy(&val, pSrc, sizeof(double)); icb += sizeof(double); pSrc += sizeof(double);
				if (icol == table.iColNumber)
					ta.orderKey = val;
				else if (icol == table.iColPrice)
					ta.price = val;
				else if (icol == table.iColQuantity)
					ta.quantity = val;
				else if (icol == table.iColVolume)
					ta.volume = val;
				else if (icol == table.iColId)
					ta.line = Transaction::MakeLine(I_TABLE_BIDS, (DWORD)val);
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
				memcpy(str, pSrc, len); pSrc += len; icb += len;
				str[len] = '\0';
				if (irow == 0) // && table.initialized == FALSE)
				{
					if (len == 0)
					{ ; }
					else if (0 == memcmp(str, "ORDERNUM", 8) || 0 == memcmp(str, "Номер", 5))
					{ table.iColNumber = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "ORDERDATE", 9) || 0 == memcmp(str, "Дата", 4))
					{ table.iColDate = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "ORDERTIME", 9) || 0 == memcmp(str, "Выставлена (время)", 18))
					{ table.iColTime = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "SECCODE", 7) || 0 == memcmp(str, "Код бумаги", 10))
					{ table.iColSecCode = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "CLASS_CODE", 4) || 0 == memcmp(str, "Код класса", 10))
					{table.iColClass = icol; table.initialized = FALSE;}
					if (0 == memcmp(str, "CLIENTCODE", 10) || 0 == memcmp(str, "Код клиента", 11))
					{table.iColClient = icol; table.initialized = FALSE;}
					else if (0 == memcmp(str, "ACCOUNT", 7) ||0 == memcmp(str, "Счет", 4))
					{ table.iColAccount = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "BUYSELL", 7) || 0 == memcmp(str, "Операция", 8))
					{ table.iColOperation = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "PRICE", 5) || 0 == memcmp(str, "Цена", 4))
					{ table.iColPrice = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "QTY", 3) || 0 == memcmp(str, "Кол-во", 6))
					{ table.iColQuantity = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "VALUE", 5) || 0 == memcmp(str, "Объем", 5))
					{ table.iColVolume = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "STATUS", 6) || 0 == memcmp(str, "Состояние", 9))
					{ table.iColStatus = icol; table.initialized = FALSE; }
				} // if (irow == 0)
				if (table.initialized)
				{
					USES_CONVERSION;
					if (icol == table.iColDate)
					{
						char buf[16];
						char * pEnd;
						buf[0] = str[0]; buf[1] = str[1]; buf[2] = '\0';
						ta.time.wDay = (WORD)strtol(buf, &pEnd, 10);
						buf[0] = str[3]; buf[1] = str[4]; buf[2] = '\0';
						ta.time.wMonth = (WORD)strtol(buf, &pEnd, 10);
						buf[0] = str[6]; buf[1] = str[7]; buf[2] = str[8]; buf[3] = str[9]; buf[4] = '\0';
						ta.time.wYear = (WORD)strtol(buf, &pEnd, 10);
					}
					else if (icol == table.iColTime)
					{
						char buf[16];
						char * pEnd;
						buf[0] = str[0]; buf[1] = str[1]; buf[2] = '\0';
						ta.time.wHour = (WORD)strtol(buf, &pEnd, 10);
						buf[0] = str[3]; buf[1] = str[4]; buf[2] = '\0';
						ta.time.wMinute = (WORD)strtol(buf, &pEnd, 10);
						buf[0] = str[6]; buf[1] = str[7]; buf[2] = '\0';
						ta.time.wSecond = (WORD)strtol(buf, &pEnd, 10);
					}
					else if (icol == table.iColSecCode)
						SAFE_TSTRCPY(ta.strSecCode, A2T(str));
					else if (icol == table.iColClass)
					{
						SAFE_TSTRCPY(ta.strClassCode, A2T(str));
						pStrClassCode = ta.strClassCode;
					}
					else if (icol == table.iColClient)
					{
						SAFE_TSTRCPY(ta.strClientCode, A2T(str));
						pStrClientCode = ta.strClientCode;
					}
					else if (icol == table.iColAccount)
					{
						SAFE_TSTRCPY(ta.strAccount, A2T(str));
						pStrAccount = ta.strAccount;
					}
					else if (icol == table.iColOperation)
					{
						if (0 == memcmp(str, "Купля", 5))
							ta.operation = Transaction::OPERATION_BUY;
						else if (0 == memcmp(str, "Продажа", 7))
							ta.operation = Transaction::OPERATION_SELL;
					}
					else if (icol == table.iColStatus)
					{
						if (0 == memcmp(str, "Исполнена", 9))
							ta.status = Transaction::STATUS_EX_BID_EXECUTED;
						else if (0 == memcmp(str, "Активна", 7))
							ta.status = Transaction::STATUS_EX_BID_ACTIVE;
						else if (0 == memcmp(str, "Снята", 5))
							ta.status = Transaction::STATUS_EX_BID_CANCELED;
					}
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
		}

		if (newRow)
		{
			BOOL initialized = table.initialized;
			if (irow == 0)
			{
				if (nrColStrs == cols && table.iColNumber >= 0)
				{
					table.initialized = TRUE; // (table.initialized) ? FALSE : TRUE;
					// Очищаем таблицу заявок:
					//UNDER_CONSTRUCTION
				}
			}
			if (initialized)
			{
				if (table.iColNumber >= 0)
				{
preparedTransactionEntryPoint:
					if (ta.status != Transaction::STATUS_EX_UNKNOWN)
					{
						int update = 1;
#if 0
						if (ta.status == Transaction::STATUS_EX_BID_ACTIVE ||
							ta.status == Transaction::STATUS_EX_BID_EXECUTED)
#else
						if (ta.status != Transaction::STATUS_EX_BID_EXECUTED) // исполненные заявки обрабатываются через таблицу сделок;
#endif
						{
							DWORD d = GetTimeStampDate(&ta);
							Transaction * pTa;

							pTa = m_transCtrl.FindTransactionByOrderNumberEx(ta.orderKey, ta.line, d, Transaction::F_SENT);

							if (pTa == NULL)
							{
								const QuoteTable::Properties * pProperties = theApp.FindTableProperty(
									NULL, ta.strSecCode, pStrClassCode, pStrClientCode, pStrAccount);
								LPCTSTR name = (pProperties) ? pProperties->name : NULL;
								pTa = m_transCtrl.CreateNewTransaction(&ta, name, 0);
								// Добавляем в список
								this->AddTransaction(pTa, pProperties);
							}
							else
							{
								if (0 != COMPARE_TRANSACTIONS(pTa, &ta))
								{
									pTa->time = ta.time;
									pTa->price = ta.price;
									pTa->quantity = ta.quantity;							
									pTa->volume = ta.volume;
									pTa->status = ta.status;
									pTa->line = ta.line;
								}
								else
									update = 0;
							}
							if (update)
							{
								//pTa->status = ta.status;
								if (! pExtTa)
									SetHistoryModified(pTa);

								// Имитируем поступление ответа:
								long nTransactionResult, nTransactionExtendedErrorCode, nTransactionReplyCode;
								DWORD dwTransId;
								double dOrderNum;
								LPCSTR lpcstrTransactionReplyMessage;
								nTransactionResult = 0;
								nTransactionExtendedErrorCode = 0;
								nTransactionReplyCode = pTa->status;
								dwTransId = pTa->id;
								dOrderNum = pTa->orderKey;
								lpcstrTransactionReplyMessage = NULL;
								Transaction::Answer * pAnswer = new Transaction::Answer(nTransactionResult, nTransactionExtendedErrorCode, 
									nTransactionReplyCode, dwTransId, dOrderNum, lpcstrTransactionReplyMessage, pTa->line);
								
								this->SendMessage(UM_TRANS2QUIK_EX, (WPARAM)pAnswer, (LPARAM)pTa);

								if (pExtTa != NULL)
									break;
							}
						}
					}
				}
			}

			++irow; icol = 0;
			nrColStrs = 0;
			newRow = FALSE;
		} // if (newRow)
	} // while
}

void CTradeDlg2::IterateTableDeals(LPCTSTR name, const BYTE * data, DWORD size, const Transaction * pExtTa)
{
	const BYTE * pBegin = data;
	const BYTE * pSrc = pBegin;
	WORD tdt, cb;
	int icb;
	int cols, rows, icol, irow;
	char str[MAX_PATH];	

	TableDeals & table = m_tableDeals;

	cols = rows = icol = irow = 0;

	LPCTSTR pStrClassCode, pStrClientCode, pStrAccount;
	pStrClassCode = pStrClientCode = pStrAccount = NULL;

	Transaction ta;

	if (! pExtTa)
	{
		memset(&ta, 0, sizeof(ta));
		ta.status = Transaction::STATUS_EXECUTED;
	}
	else
	{
		memcpy(&ta, pExtTa, sizeof(ta));
		pStrClassCode = ta.strClassCode;
		pStrClientCode = ta.strClientCode;
		pStrAccount = ta.strAccount;
		goto preparedTransactionEntryPoint;
	}

	BOOL newRow = FALSE;
	int nrColStrs = 0;

	int update = 0;

	while (pSrc < pBegin + size)
	{
		memcpy(&tdt, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		memcpy(&cb, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		if (tdt == TDT_FLOAT)
		{
			double val;
			for (icb = 0; icb < cb; )
			{
				memcpy(&val, pSrc, sizeof(double)); icb += sizeof(double); pSrc += sizeof(double);
				if (icol == table.iColNumber)
					ta.orderKey = val;
				else if (icol == table.iColPrice)
					ta.price = val;
				else if (icol == table.iColQuantity)
					ta.quantity = val;
				else if (icol == table.iColVolume)
					ta.volume = val;
				else if (icol == table.iColId)
					ta.line = Transaction::MakeLine(I_TABLE_DEALS, (DWORD)val);
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
				memcpy(str, pSrc, len); pSrc += len; icb += len;
				str[len] = '\0';
				if (irow == 0)// && table.initialized == FALSE)
				{
					if (len == 0)
					{ ; }
					else
					if (0 == memcmp(str, "ORDERNUM", 8) || 0 == memcmp(str, "Заявка", 6))
					{ table.iColNumber = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "TRADEDATE", 9) || 0 == memcmp(str, "Дата торгов", 11))
					{ table.iColDate = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "TRADETIME", 9) || 0 == memcmp(str, "Время", 5))
					{ table.iColTime = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "SECCODE", 7) || 0 == memcmp(str, "Код бумаги", 10))
					{ table.iColSecCode = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "CLASS_CODE", 4) || 0 == memcmp(str, "Код класса", 10))
					{table.iColClass = icol; table.initialized = FALSE;}
					if (0 == memcmp(str, "CLIENTCODE", 10) || 0 == memcmp(str, "Код клиента", 11))
					{table.iColClient = icol; table.initialized = FALSE;}
					else if (0 == memcmp(str, "ACCOUNT", 7) || 0 == memcmp(str, "Счет", 4))
					{ table.iColAccount = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "BUYSELL", 7) || 0 == memcmp(str, "Операция", 8))
					{ table.iColOperation = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "PRICE", 5) || 0 == memcmp(str, "Цена", 4))
					{ table.iColPrice = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "QTY", 3) || 0 == memcmp(str, "Кол-во", 6))
					{ table.iColQuantity = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "VALUE", 5) || 0 == memcmp(str, "Объем", 5))
					{ table.iColVolume = icol; table.initialized = FALSE; }
				} // if (irow == 0)
				if (table.initialized)
				{
					USES_CONVERSION;
					if (icol == table.iColDate)
					{
						char buf[16];
						char * pEnd;
						buf[0] = str[0]; buf[1] = str[1]; buf[2] = '\0';
						ta.time.wDay = (WORD)strtol(buf, &pEnd, 10);
						buf[0] = str[3]; buf[1] = str[4]; buf[2] = '\0';
						ta.time.wMonth = (WORD)strtol(buf, &pEnd, 10);
						buf[0] = str[6]; buf[1] = str[7]; buf[2] = str[8]; buf[3] = str[9]; buf[4] = '\0';
						ta.time.wYear = (WORD)strtol(buf, &pEnd, 10);
					}
					else if (icol == table.iColTime)
					{
						char buf[16];
						char * pEnd;
						buf[0] = str[0]; buf[1] = str[1]; buf[2] = '\0';
						ta.time.wHour = (WORD)strtol(buf, &pEnd, 10);
						buf[0] = str[3]; buf[1] = str[4]; buf[2] = '\0';
						ta.time.wMinute = (WORD)strtol(buf, &pEnd, 10);
						buf[0] = str[6]; buf[1] = str[7]; buf[2] = '\0';
						ta.time.wSecond = (WORD)strtol(buf, &pEnd, 10);
					}
					else if (icol == table.iColSecCode)
						SAFE_TSTRCPY(ta.strSecCode, A2T(str));
					else if (icol == table.iColClass)
					{
						SAFE_TSTRCPY(ta.strClassCode, A2T(str));
						pStrClassCode = ta.strClassCode;
					}
					else if (icol == table.iColClient)
					{
						SAFE_TSTRCPY(ta.strClientCode, A2T(str));
						pStrClientCode = ta.strClientCode;
					}
					else if (icol == table.iColAccount)
					{
						SAFE_TSTRCPY(ta.strAccount, A2T(str));
						pStrAccount = ta.strAccount;
					}
					else if (icol == table.iColOperation)
					{
						if (0 == memcmp(str, "Купля", 5))
							ta.operation = Transaction::OPERATION_BUY;
						else if (0 == memcmp(str, "Продажа", 7))
							ta.operation = Transaction::OPERATION_SELL;
					}
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
		}

		if (newRow)
		{
			BOOL initialized = table.initialized;
			if (irow == 0)
			{
				if (nrColStrs == cols)
					table.initialized = TRUE; // (table.initialized) ? FALSE : TRUE;
			}
			if (initialized)
			{
				if (table.iColNumber >= 0 && ta.orderKey > 0)
				{
preparedTransactionEntryPoint:
					update = 1;
					Transaction * pTa;
					DWORD d = GetTimeStampDate(&ta);

					pTa = m_transCtrl.FindTransactionByOrderNumberEx(ta.orderKey, ta.line, d, Transaction::F_SENT|Transaction::F_ACTIVE);

					const QuoteTable::Properties * pProperties = theApp.FindTableProperty(NULL, 
						ta.strSecCode, pStrClassCode, pStrClientCode, pStrAccount);
					if (pTa == NULL)
					{
						LPCTSTR name = (pProperties) ? pProperties->name : NULL;
						pTa = m_transCtrl.CreateNewTransaction(&ta, name, 0);												
						this->AddTransaction(pTa, pProperties);
					}
					else
					{
						if (0 != COMPARE_TRANSACTIONS(pTa, &ta))
						{							
							pTa->time = ta.time;
							pTa->price = ta.price;
							pTa->quantity = ta.quantity;							
							pTa->volume = ta.volume;
							pTa->status = ta.status;
							pTa->line = ta.line;
						}
						else
							update = 0;
					}
					if (update)
					{
						if (! pExtTa)
							SetHistoryModified(pTa);

						// Имитируем поступление ответа:
						long nTransactionResult, nTransactionExtendedErrorCode, nTransactionReplyCode;
						DWORD dwTransId;
						double dOrderNum;
						LPCSTR lpcstrTransactionReplyMessage;

						nTransactionResult = 0;
						nTransactionExtendedErrorCode = 0;
						nTransactionReplyCode = Transaction::STATUS_EXECUTED;
						dwTransId = pTa->id;
						dOrderNum = pTa->orderKey;
#if 1
						TSTRING_STD2(str, size);
						TSTRING_STD(buf);
						StrPrintTransactionParameters(pTa, buf, SIZEOF_ARRAY(buf), pProperties ? pProperties->price.nd : 0);
						_stprintf_s(str, size, TEXT("Заявка N %.0f исполнена (%s)"), pTa->orderKey, buf); 
						USES_CONVERSION;
						lpcstrTransactionReplyMessage = T2A((LPTSTR)(LPCTSTR)str);
#ifdef _DEBUG
						TRACE("%s\r\n", lpcstrTransactionReplyMessage);
#endif
#else
						lpcstrTransactionReplyMessage = NULL;
#endif
						Transaction::Answer * pAnswer = new Transaction::Answer(nTransactionResult, nTransactionExtendedErrorCode, 
							nTransactionReplyCode, dwTransId, dOrderNum, lpcstrTransactionReplyMessage, pTa->line);
						
						this->SendMessage(UM_TRANS2QUIK_EX, (WPARAM)pAnswer, (LPARAM)pTa);
					}
				}
			}
			//else
			//	table.initialized = TRUE;

			++irow; icol = 0;
			nrColStrs = 0;
			newRow = FALSE;
		} // if (newRow)
	} // while
}

void CTradeDlg2::IterateTableStops(LPCTSTR name, const BYTE * data, DWORD size, const Transaction * pExtTa)
{
	const BYTE * pBegin = data;
	const BYTE * pSrc = pBegin;
	WORD tdt, cb;
	int icb;
	int cols, rows, icol, irow;
	char str[MAX_PATH];	

	int result = Transaction::STATUS_SENT;

	TableStops & table = m_tableStops;

	cols = rows = icol = irow = 0;

	LPCTSTR pStrClassCode, pStrClientCode, pStrAccount;
	pStrClassCode = pStrClientCode = pStrAccount = NULL;

	Transaction ta;
	if (pExtTa == NULL)
	{
		memset(&ta, 0, sizeof(ta));
		ta.status = Transaction::STATUS_EXECUTED;
	}
	else
	{
		memcpy(&ta, pExtTa, sizeof(ta));
		pStrClassCode = ta.strClassCode;
		pStrClientCode = ta.strClientCode;
		pStrAccount = ta.strAccount;
		goto preparedTransactionEntryPoint;
	}

	BOOL newRow = FALSE;
	int nrColStrs = 0;

	while (pSrc < pBegin + size)
	{
		memcpy(&tdt, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		memcpy(&cb, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		if (tdt == TDT_FLOAT)
		{
			double val;
			for (icb = 0; icb < cb; )
			{
				memcpy(&val, pSrc, sizeof(double)); icb += sizeof(double); pSrc += sizeof(double);
				if (icol == table.iColNumber)
					ta.stopOrderKey = ta.orderKey = val;
				//if (icol == table.iColOrderNumber)
				//	ta.orderKey = val;
				else if (icol == table.iColPrice)
					ta.price = val;
				else if (icol == table.iColStopPrice)
					ta.stop.price = val;
				else if (icol == table.iColQuantity)
					ta.quantity = val;
				else if (icol == table.iColVolume)
					ta.volume = val;
				else if (icol == table.iColId)
					ta.line = Transaction::MakeLine(I_TABLE_STOPS, (DWORD)val);
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
				memcpy(str, pSrc, len); pSrc += len; icb += len;
				str[len] = '\0';
				if (irow == 0)// && table.initialized == FALSE)
				{
					if (len == 0)
					{ ; }
					else
					if (0 == memcmp(str, "LINKED_ORDER", 12) || 0 == memcmp(str, "Номер заявки", 12))
					{ table.iColOrderNumber = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "STOP_ORDERNUM", 13) || 0 == memcmp(str, "Номер", 5))
					{ table.iColNumber = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "STOP_ORDERDATE", 14) || 0 == memcmp(str, "Дата", 4))
					{ table.iColDate = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "STOP_ORDERTIME", 14) || 0 == memcmp(str, "Время", 5))
					{ table.iColTime = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "STOP_ORDERKIND", 14) || 0 == memcmp(str, "Тип стоп-заявки", 15))
					{ table.iColType = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "SECCODE", 7) || 0 == memcmp(str, "Код бумаги", 10))
					{ table.iColSecCode = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "CLASS_CODE", 4) || 0 == memcmp(str, "Код класса", 10))
					{table.iColClass = icol; table.initialized = FALSE;}
					if (0 == memcmp(str, "CLIENTCODE", 10) || 0 == memcmp(str, "Код клиента", 11))
					{ table.iColClient = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "ACCOUNT", 7) || 0 == memcmp(str, "Счет", 4))
					{ table.iColAccount = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "BUYSELL", 7) || 0 == memcmp(str, "Операция", 8))
					{ table.iColOperation = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "PRICE", 5) || 0 == memcmp(str, "Цена", 4))
					{ table.iColPrice = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "CONDITION_PRICE", 15) || 0 == memcmp(str, "Стоп-цена", 9))
					{ table.iColStopPrice = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "QTY", 3) || 0 == memcmp(str, "Кол-во", 6))
					{ table.iColQuantity = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "VOLUME", 6) || 0 == memcmp(str, "Объем", 5))
					{ table.iColVolume = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "STATUS_DESC", 11) || 0 == memcmp(str, "Результат", 9))
					{ table.iColResult = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "STATUS", 6) || 0 == memcmp(str, "Состояние", 9))
					{ table.iColStatus = icol; table.initialized = FALSE; }
				} // if (irow == 0)
				if (table.initialized)
				{
					USES_CONVERSION;
					if (icol == table.iColDate)
					{
						char buf[16];
						char * pEnd;
						buf[0] = str[0]; buf[1] = str[1]; buf[2] = '\0';
						ta.time.wDay = (WORD)strtol(buf, &pEnd, 10);
						buf[0] = str[3]; buf[1] = str[4]; buf[2] = '\0';
						ta.time.wMonth = (WORD)strtol(buf, &pEnd, 10);
						buf[0] = str[6]; buf[1] = str[7]; buf[2] = str[8]; buf[3] = str[9]; buf[4] = '\0';
						ta.time.wYear = (WORD)strtol(buf, &pEnd, 10);
					}
					else if (icol == table.iColTime)
					{
						char buf[16];
						char * pEnd;
						buf[0] = str[0]; buf[1] = str[1]; buf[2] = '\0';
						ta.time.wHour = (WORD)strtol(buf, &pEnd, 10);
						buf[0] = str[3]; buf[1] = str[4]; buf[2] = '\0';
						ta.time.wMinute = (WORD)strtol(buf, &pEnd, 10);
						buf[0] = str[6]; buf[1] = str[7]; buf[2] = '\0';
						ta.time.wSecond = (WORD)strtol(buf, &pEnd, 10);
					}
					else if (icol == table.iColType)
					{
						if (0 == memcmp(str, "Стоп-лимит", 10))
							ta.baseAction = ta.action = Transaction::NEW_STOP_ORDER;
					}
					else if (icol == table.iColSecCode)
						SAFE_TSTRCPY(ta.strSecCode, A2T(str));
					else if (icol == table.iColClass)
					{
						SAFE_TSTRCPY(ta.strClassCode, A2T(str));
						pStrClassCode = ta.strClassCode;
					}
					else if (icol == table.iColClient)
					{
						SAFE_TSTRCPY(ta.strClientCode, A2T(str));
						pStrClientCode = ta.strClientCode;
					}
					else if (icol == table.iColAccount)
					{
						SAFE_TSTRCPY(ta.strAccount, A2T(str));
						pStrAccount = ta.strAccount;
					}
					else if (icol == table.iColOperation)
					{
						if (0 == memcmp(str, "Купля", 5))
							ta.operation = Transaction::OPERATION_BUY;
						else if (0 == memcmp(str, "Продажа", 7))
							ta.operation = Transaction::OPERATION_SELL;
					}
					else if (icol == table.iColStatus)
					{
						if (0 == memcmp(str, "Исполнена", 9))
							ta.status = Transaction::STATUS_EXECUTED;//STATUS_EX_BID_EXECUTED;
						else if (0 == memcmp(str, "Активна", 7))
							ta.status = Transaction::STATUS_EX_BID_ACTIVE;
						else if (0 == memcmp(str, "Снята", 5))
							ta.status = Transaction::STATUS_EX_BID_CANCELED;
					}
					else if (icol == table.iColResult)
					{
						if (0 == memcmp(str, "Выставлена заявка в ТС", 22))
							result = Transaction::STATUS_EXECUTED;
						else if (0 == memcmp(str, "Отвергнута ТС", 13))
							result = Transaction::STATUS_REJECTED; // ?
						else if (0 == memcmp(str, "Снята", 5))
							result = Transaction::STATUS_EX_BID_CANCELED;
					}
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
		}

		if (newRow)
		{
			BOOL initialized = table.initialized;
			if (irow == 0)
			{
				if (nrColStrs == cols)
					table.initialized = TRUE; // (table.initialized) ? FALSE : TRUE;
			}
			if (initialized)
			{
				if (table.iColNumber >= 0 && ta.orderKey > 0)
				{
preparedTransactionEntryPoint:
					int update = 1;
#if 1
					ta.volume = ta.price * ta.quantity;
#endif
					Transaction * pTa;
					DWORD d = GetTimeStampDate(&ta);

					pTa = m_transCtrl.FindTransactionByOrderNumberEx(ta.orderKey, ta.line, d, Transaction::F_SENT);

					if (pTa == NULL)
					{
						const QuoteTable::Properties * pProperties = theApp.FindTableProperty(
							NULL, ta.strSecCode, pStrClassCode, pStrClientCode, pStrAccount);
						LPCTSTR name = (pProperties) ? pProperties->name : NULL;
						pTa = m_transCtrl.CreateNewTransaction(&ta, name, 0);						
						this->AddTransaction(pTa, pProperties);
					}
					else
					{
						if (0 != COMPARE_TRANSACTIONS(pTa, &ta))
						{
							pTa->time = ta.time;
							pTa->price = ta.price;
							pTa->quantity = ta.quantity;
							pTa->volume = ta.volume;
							pTa->status = ta.status;
							pTa->line = ta.line;
						}
						else
							update = 0;
					}
					if (update)
					{
						//pTa->status = ta.status;
						if (result != Transaction::STATUS_SENT)
							ta.status = (Transaction::Status)result;

						if (! pExtTa)
							SetHistoryModified(pTa);

						// Имитируем поступление ответа:
						long nTransactionResult, nTransactionExtendedErrorCode, nTransactionReplyCode;
						DWORD dwTransId;
						double dOrderNum;
						LPCSTR lpcstrTransactionReplyMessage;

						nTransactionResult = 0;
						nTransactionExtendedErrorCode = 0;
						nTransactionReplyCode = ta.status;
						dwTransId = pTa->id;
						dOrderNum = pTa->orderKey;
						lpcstrTransactionReplyMessage = NULL;
						Transaction::Answer * pAnswer = new Transaction::Answer(nTransactionResult, nTransactionExtendedErrorCode, 
							nTransactionReplyCode, dwTransId, dOrderNum, lpcstrTransactionReplyMessage, pTa->line);
						
						this->SendMessage(UM_TRANS2QUIK_EX, (WPARAM)pAnswer, (LPARAM)pTa);
					}
				}
			}

			result = Transaction::STATUS_SENT;

			++irow; icol = 0;
			nrColStrs = 0;
			newRow = FALSE;
		} // if (newRow)
	} // while
}

void CTradeDlg2::IterateTablePortfolio(LPCTSTR name, const BYTE * data, DWORD size)
{
	const BYTE * pBegin = data;
	const BYTE * pSrc = pBegin;
	WORD tdt, cb;
	int icb;
	int cols, rows, icol, irow;
	char str[MAX_PATH];	

	TablePortfolio & table = m_tablePortfolio;

	cols = rows = icol = irow = 0;

	int type = -1;
	TSTRING_SMALL(account);
	TSTRING_SMALL(strClientCode);
	BOOL validClientCode = FALSE;
	BOOL validAccount = FALSE;
	double limit = 0;
	double limitPrev = 0;
	double varMargin = 0;
	double profit = 0;
	double dues = 0;

	BOOL updated = FALSE;

	BOOL newRow = FALSE;
	int nrColStrs = 0;

	while (pSrc < pBegin + size)
	{
		memcpy(&tdt, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		memcpy(&cb, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		if (tdt == TDT_FLOAT)
		{
			double val;
			for (icb = 0; icb < cb; )
			{
				memcpy(&val, pSrc, sizeof(double)); icb += sizeof(double); pSrc += sizeof(double);
				if (icol == table.iColLimit)
					limit = val;
				else if (icol == table.iColLimitPrev)
					limitPrev = val;
				else if (icol == table.iColIncome)
					profit = val;
				else if (icol == table.iColDues)
					dues = val;
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
				memcpy(str, pSrc, len); pSrc += len; icb += len;
				str[len] = '\0';
				if (irow == 0 && len > 0)// && table.initialized == FALSE)
				{
					if (0 == memcmp(str, "CLIENTCODE", 10) || 0 == memcmp(str, "Код клиента", 11))
					{ table.iColClient = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "INASSETS", 8) || 0 == memcmp(str, "Вход.активы", 11))
					{ table.iColLimit = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "PROFITLOSS", 10) || 0 == memcmp(str, "Прибыль/убытки", 14))
					{ table.iColIncome = icol; table.initialized = FALSE; }
				} // if (irow == 0)
				if (table.initialized)
				{
					if (icol == table.iColClient)
					{
						USES_CONVERSION;
						SAFE_TSTRCPY(strClientCode, A2T(str));
						validClientCode = TRUE;
					}
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
		}

		if (newRow)
		{
			BOOL initialized = table.initialized;
			if (irow == 0)
			{
				if (nrColStrs == cols)
					table.initialized = TRUE; // (table.initialized) ? FALSE : TRUE;
			}
			if (initialized && validClientCode)
			{
				if (! validAccount)
				{
					const QuoteTable::Properties * pProperties = theApp.FindTableProperty(strClientCode, PROPERTY_CLIENTCODE);
					if (pProperties != NULL && pProperties->account[0])
					{
						USES_CONVERSION;
						SAFE_TSTRCPY(account, pProperties->account);
						validAccount = TRUE;
					}

				}
				if (validAccount)
				{
					Account * pAccount = theApp.FindAccount(account, 0);
					if (pAccount == NULL)
						pAccount = theApp.CreateAccount(account);
					if (pAccount != NULL)
					{
						//if (limit != 0)
						{
							profit = varMargin + profit + dues;
							pAccount->Set(limit, limitPrev, profit);
						}
						theApp.UpdateTables(pAccount);
						updated = TRUE;
					}
				}
			}

			++irow; icol = 0;
			nrColStrs = 0;
			newRow = FALSE;
		} // if (newRow)
	} // while

	if (updated)
		UpdateOSD();
}

void CTradeDlg2::IterateTableLimits(LPCTSTR name, const BYTE * data, DWORD size)
{
	const BYTE * pBegin = data;
	const BYTE * pSrc = pBegin;
	WORD tdt, cb;
	int icb;
	int cols, rows, icol, irow;
	char str[MAX_PATH];	

	TableLimits & table = m_tableLimits;

	cols = rows = icol = irow = 0;

	int type = -1;
	TSTRING_SMALL(account);
	BOOL validAccount = FALSE;
	double limit = 0;
	double limitPrev = 0;
	double varMargin = 0;
	double profit = 0;
	double dues = 0;

	BOOL newRow = FALSE;
	int nrColStrs = 0;

	BOOL updated = FALSE;

	while (pSrc < pBegin + size)
	{
		memcpy(&tdt, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		memcpy(&cb, pSrc, sizeof(WORD)); pSrc += sizeof(WORD);
		if (tdt == TDT_FLOAT)
		{
			double val;
			for (icb = 0; icb < cb; )
			{
				memcpy(&val, pSrc, sizeof(double)); icb += sizeof(double); pSrc += sizeof(double);
				if (icol == table.iColLimit)
					limit = val;
				else if (icol == table.iColLimitPrev)
					limitPrev = val;
				else if (icol == table.iColVarMargin)
					varMargin = val;
				else if (icol == table.iColIncome)
					profit = val;
				else if (icol == table.iColDues)
					dues = val;
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
				memcpy(str, pSrc, len); pSrc += len; icb += len;
				str[len] = '\0';
				if (irow == 0 && len > 0)// && table.initialized == FALSE)
				{
					if (0 == memcmp(str, "LIMIT_TYPE", 10) || 0 == memcmp(str, "Тип лимита", 10))
					{ table.iColType = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "TRDACCID", 8) || 0 == memcmp(str, "Торговый счет", 13))
					{ table.iColAccount = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "CBP_PREV_LIMIT", 14) || 0 == memcmp(str, "Предыд. лимит откр. поз.", 24))
					{ table.iColLimitPrev = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "CBPLIMIT", 8) || 0 == memcmp(str, "Лимит откр. поз.", 16))
					{ table.iColLimit = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "VARMARGIN", 9) || 0 == memcmp(str, "Вариац. маржа", 13))
					{ table.iColVarMargin = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "ACCRUEDINT", 10) || 0 == memcmp(str, "Накоплен. доход", 15))
					{ table.iColIncome = icol; table.initialized = FALSE; }
					else if (0 == memcmp(str, "TS_COMISSION", 12) || 0 == memcmp(str, "Биржевые сборы", 14))
					{ table.iColDues = icol; table.initialized = FALSE; }
				} // if (irow == 0)
				if (table.initialized)
				{
					if (icol == table.iColType)
					{
						if (0 == memcmp(str, "Рубли", 5))
							type = 0;
						else if (0 == memcmp(str, "Ден.средства", 12))
							type = 0; // ?
#if 0
						else if (0 == memcmp(str, "Залоговые рубли", 15))
							type = 1;
						else if (0 == memcmp(str, "Клиринговые рубли", 17))
							type = 2;
						else if (0 == memcmp(str, "Клиринговые залоговые рубли", 27))
							type = 3;
#endif
					}
					else if (icol == table.iColAccount)
					{
						USES_CONVERSION;
						SAFE_TSTRCPY(account, A2T(str));
						validAccount = TRUE;
					}
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
		}

		if (newRow)
		{
			BOOL initialized = table.initialized;
			if (irow == 0)
			{
				if (nrColStrs == cols)
					table.initialized = TRUE; // (table.initialized) ? FALSE : TRUE;
			}
			if (initialized && (type == 0)) //  || type == 2
			{
				if (validAccount)
				{
					Account * pAccount = theApp.FindAccount(account, 0);
					if (pAccount == NULL)
						pAccount = theApp.CreateAccount(account);
					if (pAccount != NULL)
					{
						//if (limit != 0)
						{
							profit = varMargin + profit + dues;
							pAccount->Set(limit, limitPrev, profit);
						}
						theApp.UpdateTables(pAccount);
						updated = TRUE;
					}
				}
			}

			type = -1;

			++irow; icol = 0;
			nrColStrs = 0;
			newRow = FALSE;
		} // if (newRow)
	} // while

	if (updated)
		UpdateOSD();
}


//
// Buy/Sell/Stop
//

void CTradeDlg2::BuySell(int buy, double price, double quantity, int flags)
{	
	QuoteTable * pTable = theApp.GetCurrentTable();
	const QuoteTable::Properties * pProperties;
#if 1
	if (pTable == NULL)
	{
		pProperties = NULL;
		LPCTSTR msg = TEXT("Необходимо выбрать рабочий инструмент!");
		LogMessage(msg, CODE_USER_WARNING);
		MessageBox(msg, MB_ICONWARNING|MB_OK);
		return;
	}
	else
		pProperties = pTable->GetProperties();
#endif
	Transaction ta;
	memset(&ta, 0, sizeof(ta));

	const Settings::Trading & settings = theApp.m_settings.trading;	

	CWindow wnd;
	CWindow activeWnd = GetActiveWindow();

	BOOL unknownPrice = FALSE;
	BOOL invalidParams = FALSE;

	double spread = (pProperties) ? pProperties->trading.spread : 0;

	if (flags == OPERATION_ANY)
		flags |= OPERATION_FIXED;

	if (flags & (OPERATION_FIXED|OPERATION_TA))
	{
#if 1		
		// Если стакан активен, то фиксированную цену берём из него:
		if (my::wnd::IsVisible(dlgGlass) && (dlgGlass == activeWnd))
		{
			if (pTable != NULL)
				ta.price = pTable->priceUser;
		}
		if (ta.price == 0)
#endif
		{
			if (flags & OPERATION_TA)
				ta.price = GetPriceTa();
			else
				ta.price = GetPriceUser();
		}
		if (ta.price == 0)
		{
			if (settings.autoPrice == TRUE)
#if 0
				flags = settings.useSpread ? OPERATION_SPREAD : OPERATION_MARKET;
#else
				flags = OPERATION_MARKET;
#endif
			else
			{
				unknownPrice = TRUE;
				theApp.LogMessage(TEXT("Не указана цена заявки!"), CODE_USER_WARNING);
				if (flags & OPERATION_FIXED)
				{
					if (this->IsWindowVisible() && *this == activeWnd)
					{
						wnd = GetDlgItem(IDC_EDIT_TRADE_PRICE1);
						wnd.SetFocus();
					}
				}
			}
		}
	}
	else if (flags & OPERATION_CURRENT)
	{
		ta.price = GetPriceCurrent();
		if (ta.price == 0)
		{
			theApp.LogMessage(TEXT("Цена последней сделки неизвестна!"), CODE_USER_WARNING);
			unknownPrice = TRUE;
		}
	}

	if (unknownPrice)
	{		
		invalidParams = TRUE;
	}
	else
	{
		if (flags & OPERATION_MARKET)
		{
			if (settings.useSpread)
				flags |= OPERATION_SPREAD;

			if (pTable != NULL)
			{
				double priceBuy, priceSell;
				if (settings.bestIsCurrent)
					priceBuy = priceSell = pTable->GetCurrentPrice();
				else
				{
#if 0
					pTable->GetBestPrices(priceBuy, priceSell);
#else
					pTable->GetPricesBidOffer(priceBuy, priceSell);
#endif
				}
				if (buy)
				{
					ta.price = priceSell;
					if (ta.price)
					{
						if (flags & OPERATION_SPREAD)
#if 0
							if (ta.price - spread > priceBuy)
#endif
								ta.price -= spread;
					}
					else
						unknownPrice = TRUE;
				}
				else
				{
					ta.price = priceBuy;
					if (ta.price)
					{
						if (flags & OPERATION_SPREAD)
#if 0
							if (ta.price + spread < priceSell)
#endif
									ta.price += spread;
					}
					else
						unknownPrice = TRUE;
				}
			}
		}
		if (! unknownPrice)
		{		
			if (quantity > 0)
				ta.quantity = quantity;
			else if (! this->IsHidden())
				ta.quantity = GetQuantity();
			if (ta.quantity == 0)
			{
				if (pProperties != NULL)
					ta.quantity = pProperties->trading.quantity;
			}
			ta.baseAction = ta.action = Transaction::NEW_ORDER;
			ta.operation = (buy) ? Transaction::OPERATION_BUY : Transaction::OPERATION_SELL;
			if (ta.quantity > 0)
			{
#if 0
				ta.status = Transaction::STATUS_SENT; // ???
#endif
				ta.volume = ta.price * ta.quantity;
				theApp.DoTransaction(&ta);
#if 1
				if (pTable)
					KeepUserValues(pTable);
#endif
			}
			else
			{
				invalidParams = TRUE;
				theApp.LogMessage(TEXT("Не указано количество лотов в заявке!"), CODE_USER_WARNING);
				if (this->IsWindowVisible() && *this == activeWnd)
				{
					wnd = GetDlgItem(IDC_EDIT_TRADE_QUANTITY);
					wnd.SetFocus();
				}
			}
		}
		else
		{
			invalidParams = TRUE;
			theApp.LogMessage(TEXT("Неверная цена заявки (0)!"), CODE_USER_WARNING);
		}
	}
	if (invalidParams)
	{
		if (theApp.m_settings.presentation.main.flags & F_VIEW_ENABLE_FLASHING)
			theApp.FlashWindow(activeWnd);
	}
}

void CTradeDlg2::BuySell(int buy, int flags)
{	
	BuySell(buy, 0, 0, flags);
}

void CTradeDlg2::StopBid(int buy, double price, double quantity, int flags)
{
	const QuoteTable * pTable = theApp.GetCurrentTable();
	const QuoteTable::Properties * pProperties = NULL;
#if 1
	if (pTable == NULL)
	{
		pProperties = NULL;
		LPCTSTR msg = TEXT("Необходимо выбрать рабочий инструмент!");
		LogMessage(msg, CODE_USER_WARNING);
		MessageBox(msg, MB_ICONWARNING|MB_OK);
		return;
	}
	else
		pProperties = pTable->GetProperties();
#endif
	Transaction ta;
	memset(&ta, 0, sizeof(ta));

	const Settings::Trading & settings = theApp.m_settings.trading;

	CWindow wnd;
	CWindow activeWnd = GetActiveWindow();

	BOOL invalidParams = FALSE;

	if (flags & (OPERATION_FIXED|OPERATION_TA))
	{
#if 1
		// Если стакан активен, то фиксированную цену берём из него:
		if (my::wnd::IsVisible(dlgGlass) && (dlgGlass == activeWnd))
		{			
			if (pTable != NULL)
				price = pTable->priceUser;
		}
		if (price == 0)
#endif
		{
			if (flags & OPERATION_TA)
				price = GetPriceTa();
			else
				price = GetPriceUser();
		}
		if (price == 0)
		{
			theApp.LogMessage(TEXT("Не указана цена стоп-заявки!"), CODE_USER_WARNING);
			if (flags & OPERATION_FIXED)
			{
				if (this->IsWindowVisible() && *this == activeWnd)
				{
					wnd = GetDlgItem(IDC_EDIT_TRADE_PRICE1);
					wnd.SetFocus();
				}
			}
		}
	}

	if (price)
	{
		double offset = 0;
		double slippage = 0;
		double Q = 0;

		if (pProperties)
		{
			if (flags & OPERATION_OFFSET)
			{
				if (pProperties->trading.autos.stop.isRelative)
					offset = pProperties->trading.autos.stop.relative * price / 100;
				else
					offset = pProperties->trading.autos.stop.absolute;
				if (! buy)
					offset = -offset;
			}
			slippage = pProperties->trading.autos.stop.slippage;
			Q = pProperties->trading.quantity;
		}
		ta.price = ta.stop.price = price + offset;

		if (! buy)
			slippage =- slippage;
		ta.price = ta.stop.price + slippage;

		if (quantity > 0)
			ta.quantity = quantity;
		else if (! this->IsHidden())
			ta.quantity = GetQuantity();

		if (ta.quantity == 0)
			ta.quantity = Q;

		ta.baseAction = ta.action = Transaction::NEW_STOP_ORDER;
		ta.operation = (buy) ? Transaction::OPERATION_BUY : Transaction::OPERATION_SELL;
		if (ta.quantity)
		{
#if 0
			ta.status = Transaction::STATUS_SENT; // ???
#endif
			ta.volume = ta.price * ta.quantity;
			ta.expire.type = Transaction::EXPIRE_TODAY;
			theApp.DoTransaction(&ta);
		}
		else
		{
			invalidParams = TRUE;
			theApp.LogMessage(TEXT("Не указано количество лотов в стоп-заявке!"), CODE_USER_WARNING);
			if (this->IsWindowVisible() && *this == activeWnd)
			{
				wnd = GetDlgItem(IDC_EDIT_TRADE_QUANTITY);
				wnd.SetFocus();
			}
		}
	}
	else
	{
		invalidParams = TRUE;
		theApp.LogMessage(TEXT("Неверная цена стоп-заявки (0)!"), CODE_USER_WARNING);
	}

	if (invalidParams)
	{
		if (theApp.m_settings.presentation.main.flags & F_VIEW_ENABLE_FLASHING)
			theApp.FlashWindow(activeWnd);
	}
}

void CTradeDlg2::StopBid(int buy, int flags)
{	
	StopBid(buy, 0, 0, flags);
}

//
// Load/Save Trade
//

#define HISTORY_FILE_NAME TEXT("history.txt")
#define HISTORY_FILE_MASK TEXT("%04d%02d%02d.hst.txt") //"20100708.hst.txt"

int CTradeDlg2::SaveTrade()
{
	int status = S_OK;
	const Settings & settings = theApp.GetSettings();

	if (settings.trading.history.keep == FALSE)
		return status;

	History * history = GetHistory();	
	if (! history)
		return E_POINTER;
	else if (history->IsNotModified())
		return status;

	LogMessage(TEXT("Сохранение истории транзакций"));

	my::lib::File file;

	Transaction * pTa;
	Transactions::Iterator it;

	DEFINE_PATH_EX(name);

	History::ListOfDates::iterator mIt;
	DWORD date;
	while (TRUE)
	{
		if (S_OK == history->FindFirstModifiedDate(mIt, &date))
		{
			// Данные разбиты по дням, для каждого из которых создается свой файл.
			_stprintf_s(name, SIZEOF_ARRAY(name), TEXT("%s\\")HISTORY_FILE_MASK, settings.common.path.history, 
				GetYearFromTimeStamp(date), GetMonthFromTimeStamp(date), GetDayFromTimeStamp(date));

			pTa = history->FindTransactionByDate(date, it);
			if (pTa)
			{
				for (;;)
				{
					pTa = history->FindNextTransaction(it, 0);
					if (pTa != NULL)
					{
						if (pTa->status == Transaction::STATUS_SENT)
							continue;
						if (Transaction::IsCanceled(pTa->status) == FALSE || settings.trading.history.keepCancel == TRUE)
						{
							if (! file.IsValid())
								if (S_OK != file.New(name))
									break;
							Write(pTa, file);
						}
					}
					else
						break;
				} // for (;;)
				file.Close();
			} // if (pTa)
			else
			{// Удаляем файл
#if 0
				file.Remove(name);
#else
				::DeleteFile(name);
#endif
				//break;
			}
			history->RemoveAt(mIt);
		}
		else
			break;
	} // while (TRUE)

	LogMessage(TEXT("История транзакций сохранена"));

	return status;
}
#if LOAD_TRADE_IN_SEPARATE_THREAD
int CTradeDlg2::LoadTrade()
{
	int status = S_OK;
	const Settings & settings = theApp.GetSettings();

	if (settings.trading.history.keep == FALSE)
		return status;

	// Запускаем поток:
	status = BeginLoadTrade();
	return status;
}

int CTradeDlg2::BeginLoadTrade()
{
	int status = E_PENDING;

	status = my::lib::CreateThread(m_loadTrade.thread, &CTradeDlg2::LoadTradeThreadFunc, this);

	return status;
}

int CTradeDlg2::EndLoadTrade()
{
	int status = S_OK;

	status = my::lib::CloseThread(m_loadTrade.thread);

	return status;
}

DWORD WINAPI CTradeDlg2::LoadTradeThreadFunc(LPVOID lpThreadParameter)
{
	DWORD result = 0;
	TRACE(__FUNCTION__": begin\r\n");
	CTradeDlg2 * pTradeDlg = (CTradeDlg2*)lpThreadParameter;

	HANDLE hEvent = pTradeDlg->m_loadTrade.thread.hEvent;

	const Settings & settings = theApp.GetSettings();

	my::lib::File file;

	WIN32_FIND_DATA ffd;
	HANDLE hFind;
	BOOL success;

	DEFINE_PATH_EX(name);

	_stprintf_s(name, SIZEOF_ARRAY(name), TEXT("%s\\????????.hst.txt"), settings.common.path.history);

	BOOL modified = theApp.GetModifySettings();

	hFind = ::FindFirstFile(name, &ffd);
	if (hFind != INVALID_HANDLE_VALUE)
	{
#if 0
		pTradeDlg->OnBeginLoadTrade();
#else
		pTradeDlg->PostMessage(UM_LOAD_TRADE, LOAD_TRADE_ON_BEGIN);
#endif		
		Transaction ta;
		
		success = TRUE;
		while (success) 
		{
#if 1
			DWORD dwRetCode = ::WaitForSingleObject(hEvent, 10);
			if (dwRetCode == WAIT_TIMEOUT)
			{// Продолжаем работу
			}
			else if (dwRetCode == WAIT_OBJECT_0)
			{// Прекращаем работу:
				TRACE(__FUNCTION__": event terminate\r\n");
				::FindClose(hFind);
				goto end;
			}
			else
			{
				DWORD err = ::GetLastError();
				TRACE(__FUNCTION__": error (%d)\r\n", err);
				::FindClose(hFind);
				goto end;
			}
#endif
			//if (0 == memcmp())
			{
				_stprintf_s(name, SIZEOF_ARRAY(name), TEXT("%s\\%s"), settings.common.path.history, ffd.cFileName);
				if (S_OK == file.Open(name))
				{
					while (S_OK == Read(ta, file))
					{
#if 0
					DWORD dwRetCode = ::WaitForSingleObject(hEvent, 10);
					if (dwRetCode == WAIT_TIMEOUT)
					{// Продолжаем работу
					}
					else if (dwRetCode == WAIT_OBJECT_0)
					{// Прекращаем работу:
						TRACE(__FUNCTION__": event terminate\r\n");
						file.Close();
						::FindClose(hFind);
						goto end;
					}
					else
					{
						DWORD err = ::GetLastError();
						TRACE(__FUNCTION__": error (%d)\r\n", err);
						file.Close();
						::FindClose(hFind);
						goto end;
					}
#endif
						// Имитируем поступление данных:
						if (Transaction::IsCanceled(ta.status) == FALSE || settings.trading.history.keepCancel == TRUE)
						{
							QuoteTable::Properties properties;
							LPCTSTR pStrName = NULL;
							LPCTSTR pStrSecCode = NULL;
							if (ta.strName[0] != 0)
								pStrName = ta.strName;
							else if (ta.strSecCode[0] != 0)
								pStrSecCode = ta.strSecCode;
							const QuoteTable::Properties * pProperties = theApp.FindTableProperty(pStrName, pStrSecCode, NULL, NULL, NULL);
							if (pProperties == NULL)
							{
								properties.Initialize(pStrName, pStrSecCode ? pStrSecCode : TEXT("???"));
								pProperties = &properties;
								theApp.UpdateListOfTableProperties(pStrName, pProperties, ListOfTableProperties::F_UPDATE);
							}
							if (pProperties != NULL)
							{
								if (! ta.strSecCode[0])
									SAFE_TSTRCPY(ta.strSecCode, pProperties->seccode);
								if (! ta.strClassCode[0])
									SAFE_TSTRCPY(ta.strClassCode, pProperties->classcode);
								if (! ta.strClientCode[0])
									SAFE_TSTRCPY(ta.strClientCode, pProperties->clientcode);
								if (! ta.strAccount[0])
									SAFE_TSTRCPY(ta.strAccount, pProperties->account);
#if 1
								pTradeDlg->OnSecCode(pProperties->name, pProperties->seccode, 
									pProperties->classcode, pProperties->clientcode, pProperties->account);
#endif
								LPCTSTR name = pProperties->name;
								if (Transaction::IsActionStop(ta.action))
								{
									ta.line = Transaction::MakeLine(I_TABLE_STOPS, ta.line);
									if (my::wnd::IsValid(*pTradeDlg))
										pTradeDlg->IterateTableStops(name, NULL, 0, &ta);
								}
								else
								{
									if (Transaction::IsActive(ta.status))
									{
										ta.line = Transaction::MakeLine(I_TABLE_BIDS, ta.line);
										if (my::wnd::IsValid(*pTradeDlg))
											pTradeDlg->IterateTableBids(name, NULL, 0, &ta);
									}
									else
									{
										if (Transaction::IsCanceled(ta.status) == FALSE)
										{
											ta.line = Transaction::MakeLine(I_TABLE_DEALS, ta.line);
											if (my::wnd::IsValid(*pTradeDlg))
												pTradeDlg->IterateTableDeals(name, NULL, 0, &ta);
										}
										else
										{
											ta.line = Transaction::MakeLine(I_TABLE_BIDS, ta.line);
											if (my::wnd::IsValid(*pTradeDlg))
												pTradeDlg->IterateTableBids(name, NULL, 0, &ta);
										}
									}
								}
							}
						}
					} // while (Read)
					file.Close();
				}
			}
			success = ::FindNextFile(hFind, &ffd);
		} // while(Find)
		::FindClose(hFind);
#if 0
		pTradeDlg->OnEndLoadTrade();
#else
		pTradeDlg->PostMessage(UM_LOAD_TRADE, LOAD_TRADE_ON_END);
#endif
	} // if (hFind != INVALID_HANDLE_VALUE)

	pTradeDlg->PostMessage(UM_LOAD_TRADE, LOAD_TRADE_DONE);
end:
	theApp.SetModifySettings(NULL, modified);
	TRACE(__FUNCTION__": end\r\n");
	return result;
}
#else
int CTradeDlg2::LoadTrade()
{
	int status = S_OK;
	History * history = GetHistory();	

	if (! history)
		return E_POINTER;

	const Settings & settings = theApp.GetSettings();
#if 1
	if (settings.trading.history.keep == FALSE)
		return status;
#endif
	my::lib::File file;
	CFileException ex;

	WIN32_FIND_DATA ffd;
	HANDLE hFind;
	BOOL success;

	DEFINE_PATH_EX(name);

	_stprintf_s(name, SIZEOF_ARRAY(name), TEXT("%s\\????????.hst.txt"), settings.common.path.history);

	BOOL modified = theApp.GetModifySettings();

	hFind = ::FindFirstFile(name, &ffd);
	if (hFind != INVALID_HANDLE_VALUE)
	{
#if 1
		this->OnBeginLoadTrade();
#endif		
		History * history = GetHistory();	
		Transaction ta;
		
		success = TRUE;
		while (success) 
		{
			//if (0 == memcmp())
			{
				_stprintf_s(name, SIZEOF_ARRAY(name), TEXT("%s\\%s"), settings.common.path.history, ffd.cFileName);
				if (TRUE == file.Open(name, my::lib::File::modeRead, &ex))
				{
					while (S_OK == Read(ta, file))
					{
						// Имитируем поступление:
						if (Transaction::IsCanceled(ta.status) == FALSE || settings.trading.history.keepCancel == TRUE)
						{
							const QuoteTable::Properties * pProperties = theApp.FindTableProperty(ta.strSecCode, PROPERTY_SECCODE);
							if (pProperties != NULL)
							{
#if 1
								OnSecCode(pProperties->seccode);
#endif
								LPCTSTR name = pProperties->name;
								if (Transaction::IsActionStop(ta.action))
								{
									ta.line = Transaction::MakeLine(I_TABLE_STOPS, ta.line);
									IterateTableStops(name, NULL, 0, &ta);
								}
								else
								{
									if (Transaction::IsActive(ta.status))
									{
										ta.line = Transaction::MakeLine(I_TABLE_BIDS, ta.line);
										IterateTableBids(name, NULL, 0, &ta);
									}
									else
									{
										if (Transaction::IsCanceled(ta.status) == FALSE)
										{
											ta.line = Transaction::MakeLine(I_TABLE_DEALS, ta.line);
											IterateTableDeals(name, NULL, 0, &ta);
										}
									}
								}
							}
						}
					} // while (Read)
					file.Close();
				}
			}
			success = ::FindNextFile(hFind, &ffd);
		} // while(Find)
		::FindClose(hFind);
#if 1
		this->OnEndLoadTrade();
#endif
	} // if (hFind != INVALID_HANDLE_VALUE)
#if 1
	this->PostMessage(UM_LOAD_TRADE, LOAD_TRADE_DONE);
#endif
	theApp.SetModifySettings(NULL, modified);
	return status;
}
#endif // LOAD_TRADE_IN_SEPARATE_THREAD

void CTradeDlg2::OnBeginLoadTrade()
{
	SetLoading(TRUE);
	if (my::wnd::IsValid(*this))
	{
		m_listCtrl.SetNoSort();
		m_listCtrl.SetNoScroll();
		if (! IsHidden())
		{
			if (theApp.GetSettings().presentation.main.show.wnd & F_MAIN_SHOW_LIST)
			{
				m_listCtrl.ShowWindow(SW_HIDE);
				m_listCtrl.UpdateWindow();
			}
			this->UpdateWindow();
			GetDlgItem(IDC_LIST_FRAME).UpdateWindow();
			CWindow wndLoading = GetDlgItem(IDC_STATIC_LOADING);
			//wndLoading.EnableWindow(FALSE);
			wndLoading.ShowWindow(SW_SHOW);
			wndLoading.UpdateWindow();
		}
	}

	if (my::wnd::IsValid(dlgHistory)) 
	{
		dlgHistory.SetNoSort();
		dlgHistory.SetNoScroll();
	}

	LogMessage(TEXT("Загрузка истории транзакций"));
#if defined _DEBUG && 1
	TRACE("Load history: begin (%d)\r\n", SetTicks1());
#endif
}

void CTradeDlg2::OnEndLoadTrade()
{
#if defined _DEBUG && 1
	DWORD ticks2 = SetTicks2();
	TRACE("Load history: end (%d, dt=%d)\r\n", ticks2, GetTicksDelta());
#endif

	if (my::wnd::IsValid(dlgHistory)) 
	{
		dlgHistory.SetNoSort(FALSE);
		dlgHistory.SetNoScroll(FALSE);
		dlgHistory.DoSort();
		dlgHistory.DoScroll();
	}

	if (my::wnd::IsValid(*this))
	{
		m_listCtrl.SetNoSort(FALSE);
		m_listCtrl.SetNoScroll(FALSE);
		m_listCtrl.DoSort();
		m_listCtrl.DoScroll();
		if (! IsHidden())
		{
			CWindow wndLoading = GetDlgItem(IDC_STATIC_LOADING);
			wndLoading.ShowWindow(SW_HIDE);
			if (theApp.GetSettings().presentation.main.show.wnd & F_MAIN_SHOW_LIST)
			{
				m_listCtrl.ShowWindow(SW_SHOW);
			}
			this->UpdateWindow();
		}

		LogMessage(TEXT("Загрузка истории транзакций завершена"));
	}
	SetLoading(FALSE);
}


//
// Actions
//

int CTradeDlg2::OnAction(Action::Type action, int flags, HWND hWnd)
{
	PFuncOnAction func = m_actions[action];
	if (func != NULL)
		return (this->*func)(flags, hWnd);
	else
		return -1;
}

int CTradeDlg2::OnActionBuy(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedBuy())
			OnBuy();
	}
	return 0;
}

int CTradeDlg2::OnActionSell(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedSell())
			OnSell();
	}
	return 0;
}

int CTradeDlg2::OnActionBuyMarket(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedBuy())
			OnBuyMarket();
	}
	return 0;
}

int CTradeDlg2::OnActionSellMarket(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedSell())
			OnSellMarket();
	}
	return 0;
}

int CTradeDlg2::OnActionBuyFixed(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedBuy())
			OnBuyFixed();
	}
	return 0;
}

int CTradeDlg2::OnActionSellFixed(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedSell())
			OnSellFixed();
	}
	return 0;
}

int CTradeDlg2::OnActionBuyCurrent(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedBuy())
			OnBuyCurrent();
	}
	return 0;
}

int CTradeDlg2::OnActionSellCurrent(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedSell())
			OnSellCurrent();
	}
	return 0;
}

int CTradeDlg2::OnActionBuyTa(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedBuy() && my::ctrl::GetFocusedItem(m_listCtrl) >= 0)
			OnBuyTa();
	}
	return 0;
}

int CTradeDlg2::OnActionSellTa(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedSell() && my::ctrl::GetFocusedItem(m_listCtrl) >= 0)
			OnSellTa();
	}
	return 0;
}

int CTradeDlg2::OnActionStopOrder(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedStop())
			OnBnClickedButtonStop(0, 0, hWnd);
	}
	return 0;
}

int CTradeDlg2::OnActionStopBuy(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedStop() && IsAllowedBuy())
			OnStopBuy();
	}
	return 0;
}

int CTradeDlg2::OnActionStopSell(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedStop() && IsAllowedSell())
			OnStopSell();
	}
	return 0;
}

int CTradeDlg2::OnActionStopBuyOffset(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedStop() && IsAllowedBuy())
			OnStopBuyOffset();
	}
	return 0;
}

int CTradeDlg2::OnActionStopSellOffset(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedStop() && IsAllowedSell())
			OnStopSellOffset();
	}
	return 0;
}

int CTradeDlg2::OnActionStopBuyTa(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedStop() && IsAllowedBuy() && my::ctrl::GetFocusedItem(m_listCtrl) >= 0)
			OnStopBuyTa();
	}
	return 0;
}

int CTradeDlg2::OnActionStopSellTa(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedStop() && IsAllowedSell() && my::ctrl::GetFocusedItem(m_listCtrl) >= 0)
			OnStopSellTa();
	}
	return 0;
}

int CTradeDlg2::OnActionStopBuyOffsetTa(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedStop() && IsAllowedBuy() && my::ctrl::GetFocusedItem(m_listCtrl) >= 0)
			OnStopBuyOffsetTa();
	}
	return 0;
}

int CTradeDlg2::OnActionStopSellOffsetTa(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedStop() && IsAllowedSell() && my::ctrl::GetFocusedItem(m_listCtrl) >= 0)
			OnStopSellOffsetTa();
	}
	return 0;
}

int CTradeDlg2::OnActionStopBidTa(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedStop() && IsAllowedBuy() && IsAllowedSell())
			OnStopBidTa();
	}
	return 0;
}

int CTradeDlg2::OnActionChangeBid(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedBuy() || IsAllowedSell())
			OnChangeActiveBid();
	}
	return 0;
}

int CTradeDlg2::OnActionCancelBids(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedCancel())
			OnCancelOrder();
	}
	return 0;
}

int CTradeDlg2::OnActionCancelBidsAll(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedCancelAll())
			OnCancelOrdersAll();
	}
	return 0;
}

int CTradeDlg2::OnActionCancelBidsBuy(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedCancel())
			OnCancelOrdersBuy();
	}
	return 0;
}

int CTradeDlg2::OnActionCancelBidsSell(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedCancelAll())
			OnCancelOrdersSell();
	}
	return 0;
}

int CTradeDlg2::OnActionCancelBidsAllStop(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		//CWindow wnd = GetDlgItem(IDC_BUTTON_CANCEL_ALL);
		//if (wnd.IsWindowEnabled())
		//	OnCancelOrdersAll();
	}
	return 0;
}

int CTradeDlg2::OnActionTradeZero(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedZero())
			OnTradeZero();
	}
	return 0;
}

int CTradeDlg2::OnActionTradeReverse(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (IsAllowedReverse())
			OnTradeReverse();
	}
	return 0;
}

int CTradeDlg2::OnActionInstrumentPrevios(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
		OnInstrumentPrevios();
	return 0;
}

int CTradeDlg2::OnActionInstrumentNext(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
		OnInstrumentNext();
	return 0;
}

int CTradeDlg2::OnActionShowInstrument(int flags, HWND hWnd)
{
#if 0
	if (! (dlgHistory && wnd == dlgHistory))
#endif
	{
		m_selectInstrument = ACTION_FLAG_END(flags) ? 0 : 1;
		UpdateOSD();
	}
	return 0;
}

int CTradeDlg2::OnActionEditInstrument(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		if (theApp.GetCurrentTable() != NULL)
#if 0
			ShowProperties();
#else
			::PostMessage(*this, UM_SHOW_PROPERTIES, 0, (LPARAM)hWnd);
#endif
	}
	return 0;
}

int CTradeDlg2::OnActionShowHideWindowDeals(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
		OnShowMain();
	return 0;
}

int CTradeDlg2::OnActionShowHideWindowGlass(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
		OnGlass();
	return 0;
}

int CTradeDlg2::OnActionShowHideWindowMessages(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
		OnMsglog();
	return 0;
}

int CTradeDlg2::OnActionShowHideWindowHistory(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
		OnHistory();
	return 0;
}

int CTradeDlg2::OnActionChangeButtons(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
		ChangeMode();
	return 0;
}

int CTradeDlg2::OnActionShowTransactionInfo(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
		OnTransactionInfo();
	return 0;
}

int CTradeDlg2::OnActionShowActiveBidsAndDeals(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
		OnUserTrade();
	return 0;
}

int CTradeDlg2::OnActionShowRarefiedGlass(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
		OnViewGlass(ID_GLASS_RAREFY);
	return 0;
}

int CTradeDlg2::OnActionShowNeutralZone(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
		OnViewGlass(ID_GLASS_NEUTRAL_ZONE);
	return 0;
}

int CTradeDlg2::OnActionSettings(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
		OnSettings();
	return 0;
}

int CTradeDlg2::OnActionGlassBidFixed(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_BID_FIXED);}
int CTradeDlg2::OnActionGlassBuyFixed(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_BUY_FIXED);}
int CTradeDlg2::OnActionGlassSellFixed(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_SELL_FIXED);}

int CTradeDlg2::OnActionGlassBidCurrent(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_BID_CURRENT);}
int CTradeDlg2::OnActionGlassBuyCurrent(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_BUY_CURRENT);}
int CTradeDlg2::OnActionGlassSellCurrent(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_SELL_CURRENT);}

int CTradeDlg2::OnActionGlassBidMarket(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_BID_MARKET);}
int CTradeDlg2::OnActionGlassBuyMarket(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_BUY_MARKET);}
int CTradeDlg2::OnActionGlassSellMarket(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_SELL_MARKET);}

int CTradeDlg2::OnActionGlassStopBid(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_STOP_BID);}
int CTradeDlg2::OnActionGlassStopBuy(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_STOP_BID_BUY);}
int CTradeDlg2::OnActionGlassStopSell(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_STOP_BID_SELL);}
int CTradeDlg2::OnActionGlassStopBidOffset(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_STOP_BID_OFFSET);}
int CTradeDlg2::OnActionGlassStopBuyOffset(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_STOP_BID_BUY_OFFSET);}
int CTradeDlg2::OnActionGlassStopSellOffset(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_STOP_BID_SELL_OFFSET);}

int CTradeDlg2::OnActionGlassCancelBid(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_CANCEL_BID);}
int CTradeDlg2::OnActionGlassCancelAll(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_CANCEL_ALL);}

int CTradeDlg2::OnActionGlassSelectItem(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_SELECT_ITEM);}
int CTradeDlg2::OnActionGlassPopupMenu(int flags, HWND hWnd) {return OnActionGlass(flags, hWnd, ACTION_GLASS_POPUP_MENU);}

int CTradeDlg2::OnActionGlass(int flags, HWND hWnd, Action::Type action)
{
	BOOL end = ACTION_FLAG_END(flags);

	BOOL pass;
	if (action == ACTION_GLASS_POPUP_MENU)
		pass = TRUE;
	else if (! end)
		pass = TRUE;
	else if (action == ACTION_GLASS_SELECT_ITEM)
		pass = TRUE;
	else
		pass = FALSE;
	if (pass)
	{
		CWindow wnd = dlgGlass;
		if (my::wnd::IsValid(wnd) && hWnd == wnd)
		{
			dlgGlass.SetFocus();

			if (action == ACTION_GLASS_POPUP_MENU)
			{
				if (end)
					dlgGlass.TrackPopupMenu();
				else
				{
					if (theApp.m_settings.presentation.glass.marker.follow & MARKER_FOLLOW_MENU)
						dlgGlass.SelectItem(NULL, CDlgGlass::F_DRAW_NOW);
				}
			}
			else
			{
#if 1
				if (! end)
				{
					DWORD shortcut = theApp.GetShortcutKey(action);
					if (shortcut & MAKESHORTCUT(0, 0, SCK_LBUTTON|SCK_RBUTTON|SCK_MBUTTON|SCK_XBUTTON1|SCK_XBUTTON2))
						dlgGlass.SelectItem(NULL, CDlgGlass::F_DRAW_NOW);
				}
#endif
				int iActive = CDlgGlass::I_ACTIVE_MARKER;
				if (action == ACTION_GLASS_SELECT_ITEM)
				{
					if (! end)
					{					
					}
					else
					{// Перенос своих заявок
#if 0
						dlgGlass.SelectItem(NULL, CDlgGlass::F_DRAW_NOW);
						iActive = CDlgGlass::I_ACTIVE_MARKER;
#endif
					}
				}
				else
				{
				}

				const CDlgGlass::Active::Item & active = dlgGlass.GetActiveItem(iActive);
				if (active.flags & CDlgGlass::F_ACTIVE_ITEM_IS_VALID)
				{
					if (IsAllowedBuy() || IsAllowedSell())
					{
						BOOL buy = (active.flags & CDlgGlass::DrawInfo::Area::TYPE_BUY);
						switch (action)
						{
						case ACTION_GLASS_BID_FIXED: 
							if (buy)
								OnSellFixed();
							else
								OnBuyFixed(); 								
							break;
						case ACTION_GLASS_BUY_FIXED: OnBuyFixed(); 
							break;
						case ACTION_GLASS_SELL_FIXED: OnSellFixed(); 
							break;
						case ACTION_GLASS_BID_CURRENT: if (buy) OnSellCurrent(); else OnBuyCurrent();								
							break;
						case ACTION_GLASS_BUY_CURRENT: OnBuyCurrent(); 
							break;
						case ACTION_GLASS_SELL_CURRENT: OnSellCurrent(); 
							break;
						case ACTION_GLASS_BID_MARKET: if (buy) OnSellMarket(); else OnBuyMarket();								
							break;
						case ACTION_GLASS_BUY_MARKET: OnBuyMarket(); 
							break;
						case ACTION_GLASS_SELL_MARKET: OnSellMarket(); 
							break;
						case ACTION_GLASS_STOP_BID: if (buy) OnStopSell(); else OnStopBuy();								
							break;
						case ACTION_GLASS_STOP_BID_BUY: OnStopBuy();
							break;
						case ACTION_GLASS_STOP_BID_SELL: OnStopSell();
							break;
						case ACTION_GLASS_STOP_BID_OFFSET: if (buy) OnStopSellOffset(); else OnStopBuyOffset();								
							break;
						case ACTION_GLASS_STOP_BID_BUY_OFFSET: OnStopBuyOffset();
							break;
						case ACTION_GLASS_STOP_BID_SELL_OFFSET: OnStopSellOffset();
							break;
						case ACTION_GLASS_CANCEL_BID: CancelOrderGlass();
							break;
						case ACTION_GLASS_CANCEL_ALL: OnCancelOrdersAll();
							break;
						} // switch (action)
					}
				}
			}

			return 0;
		}
	}
	return -1;
}

int CTradeDlg2::OnActionFreeze(int flags, HWND hWnd) 
{
	if (! ACTION_FLAG_END(flags))
		ToggleFreeze();
	return 0;
}
#if USE_ACTION_CAPTURE_VALUE
int CTradeDlg2::OnActionCaptureValue(int flags, HWND hWnd)
{
	if (! ACTION_FLAG_END(flags))
	{
		CWindow activeWnd = GetActiveWindow();
		if (my::wnd::IsVisible(dlgGlass) && (dlgGlass == activeWnd))
		{
		}
		else
		{
			CPoint point;
			GetCursorPos(&point);
			ScreenToClient(&point);

			CRect rect;
			m_listCtrl.GetWindowRect(rect);
			ScreenToClient(rect);
			if (rect.PtInRect(point) && m_listCtrl == GetFocus())
			{
				const Transaction * pTa = GetSelectedTa();
				if (pTa)
				{
					//m_listCtrl.HitTest();
				}
			}
		}
	}
	return 0;
}
#endif // USE_ACTION_CAPTURE_VALUE
void CTradeDlg2::InitTableOfActions(int size)
{
	m_actions.clear();
	m_actions.insert(m_actions.begin(), (TableOfActions::size_type)size, NULL);

	m_actions[ACTION_BUY] = &CTradeDlg2::OnActionBuy;
	m_actions[ACTION_SELL] = &CTradeDlg2::OnActionSell;
	m_actions[ACTION_BUY_MARKET] = &CTradeDlg2::OnActionBuyMarket;
	m_actions[ACTION_SELL_MARKET] = &CTradeDlg2::OnActionSellMarket;
	m_actions[ACTION_BUY_FIXED] = &CTradeDlg2::OnActionBuyFixed;
	m_actions[ACTION_SELL_FIXED] = &CTradeDlg2::OnActionSellFixed;
	m_actions[ACTION_BUY_CURRENT] = &CTradeDlg2::OnActionBuyCurrent;
	m_actions[ACTION_SELL_CURRENT] = &CTradeDlg2::OnActionSellCurrent;
	m_actions[ACTION_BUY_TA] = &CTradeDlg2::OnActionBuyTa;
	m_actions[ACTION_SELL_TA] = &CTradeDlg2::OnActionSellTa;

	m_actions[ACTION_STOP_ORDER] = &CTradeDlg2::OnActionStopOrder;
	m_actions[ACTION_STOP_BID_BUY] = &CTradeDlg2::OnActionStopBuy;
	m_actions[ACTION_STOP_BID_SELL] = &CTradeDlg2::OnActionStopSell;
	m_actions[ACTION_STOP_BID_BUY_OFFSET] = &CTradeDlg2::OnActionStopBuyOffset;
	m_actions[ACTION_STOP_BID_SELL_OFFSET] = &CTradeDlg2::OnActionStopSellOffset;
	m_actions[ACTION_STOP_BID_BUY_TA] = &CTradeDlg2::OnActionStopBuyTa;
	m_actions[ACTION_STOP_BID_SELL_TA] = &CTradeDlg2::OnActionStopSellTa;
	m_actions[ACTION_STOP_BID_BUY_OFFSET_TA] = &CTradeDlg2::OnActionStopBuyOffsetTa;
	m_actions[ACTION_STOP_BID_SELL_OFFSET_TA] = &CTradeDlg2::OnActionStopSellOffsetTa;
	m_actions[ACTION_STOP_BID_TA] = &CTradeDlg2::OnActionStopBidTa;
#if USE_CHANGE_ACTIVE_BID
	m_actions[ACTION_CHANGE_BID] = &CTradeDlg2::OnActionChangeBid;
#endif
	m_actions[ACTION_CANCEL_ORDER] = &CTradeDlg2::OnActionCancelBids;
	m_actions[ACTION_CANCEL_ORDERS_ALL] = &CTradeDlg2::OnActionCancelBidsAll;
	m_actions[ACTION_CANCEL_ORDERS_BUY] = &CTradeDlg2::OnActionCancelBidsBuy;
	m_actions[ACTION_CANCEL_ORDERS_SELL] = &CTradeDlg2::OnActionCancelBidsSell;
	m_actions[ACTION_CANCEL_ORDERS_ALL_STOP] = &CTradeDlg2::OnActionCancelBidsAllStop;

	m_actions[ACTION_TRADE_ZERO] = &CTradeDlg2::OnActionTradeZero;
	m_actions[ACTION_TRADE_REVERSE] = &CTradeDlg2::OnActionTradeReverse;

	m_actions[ACTION_INSTRUMENT_NEXT] = &CTradeDlg2::OnActionInstrumentNext;
	m_actions[ACTION_INSTRUMENT_PREVIOS] = &CTradeDlg2::OnActionInstrumentPrevios;
	m_actions[ACTION_SHOW_INSTRUMENT] = &CTradeDlg2::OnActionShowInstrument;
	m_actions[ACTION_EDIT_INSTRUMENT] = &CTradeDlg2::OnActionEditInstrument;

	m_actions[ACTION_SHOW_HIDE_WINDOW_DEALS] = &CTradeDlg2::OnActionShowHideWindowDeals;
	m_actions[ACTION_SHOW_HIDE_WINDOW_GLASS] = &CTradeDlg2::OnActionShowHideWindowGlass;
	m_actions[ACTION_SHOW_HIDE_WINDOW_MESSAGES] = &CTradeDlg2::OnActionShowHideWindowMessages;
	m_actions[ACTION_SHOW_HIDE_WINDOW_HISTORY] = &CTradeDlg2::OnActionShowHideWindowHistory;
	
	m_actions[ACTION_CHANGE_BUTTONS] = &CTradeDlg2::OnActionChangeButtons;	

	m_actions[ACTION_SHOW_TRANSACTION_INFO] = &CTradeDlg2::OnActionShowTransactionInfo;
	m_actions[ACTION_SHOW_ACTIVE_BIDS_AND_DEALS] = &CTradeDlg2::OnActionShowActiveBidsAndDeals;
	m_actions[ACTION_SHOW_RAREFIED_GLASS] = &CTradeDlg2::OnActionShowRarefiedGlass;
	m_actions[ACTION_SHOW_NEUTRAL_ZONE] = &CTradeDlg2::OnActionShowNeutralZone;

	m_actions[ACTION_SETTINGS] = &CTradeDlg2::OnActionSettings;

	m_actions[ACTION_GLASS_BID_FIXED] = &CTradeDlg2::OnActionGlassBidFixed;
	m_actions[ACTION_GLASS_BUY_FIXED] = &CTradeDlg2::OnActionGlassBuyFixed;
	m_actions[ACTION_GLASS_SELL_FIXED] = &CTradeDlg2::OnActionGlassSellFixed;
	m_actions[ACTION_GLASS_BID_CURRENT] = &CTradeDlg2::OnActionGlassBidCurrent;
	m_actions[ACTION_GLASS_BUY_CURRENT] = &CTradeDlg2::OnActionGlassBuyCurrent;
	m_actions[ACTION_GLASS_SELL_CURRENT] = &CTradeDlg2::OnActionGlassSellCurrent;
	m_actions[ACTION_GLASS_BID_MARKET] = &CTradeDlg2::OnActionGlassBidMarket;
	m_actions[ACTION_GLASS_BUY_MARKET] = &CTradeDlg2::OnActionGlassBuyMarket;
	m_actions[ACTION_GLASS_SELL_MARKET] = &CTradeDlg2::OnActionGlassSellMarket;
	m_actions[ACTION_GLASS_STOP_BID] = &CTradeDlg2::OnActionGlassStopBid;
	m_actions[ACTION_GLASS_STOP_BID_BUY] = &CTradeDlg2::OnActionGlassStopBuy;
	m_actions[ACTION_GLASS_STOP_BID_SELL] = &CTradeDlg2::OnActionGlassStopSell;
	m_actions[ACTION_GLASS_STOP_BID_OFFSET] = &CTradeDlg2::OnActionGlassStopBidOffset;
	m_actions[ACTION_GLASS_STOP_BID_BUY_OFFSET] = &CTradeDlg2::OnActionGlassStopBuyOffset;
	m_actions[ACTION_GLASS_STOP_BID_SELL_OFFSET] = &CTradeDlg2::OnActionGlassStopSellOffset;
	m_actions[ACTION_GLASS_CANCEL_BID] = &CTradeDlg2::OnActionGlassCancelBid;
	m_actions[ACTION_GLASS_CANCEL_ALL] = &CTradeDlg2::OnActionGlassCancelAll;
	m_actions[ACTION_GLASS_SELECT_ITEM] = &CTradeDlg2::OnActionGlassSelectItem;
	m_actions[ACTION_GLASS_POPUP_MENU] = &CTradeDlg2::OnActionGlassPopupMenu;

	m_actions[ACTION_FREEZE] = &CTradeDlg2::OnActionFreeze;
#if USE_ACTION_CAPTURE_VALUE
	m_actions[ACTION_CAPTURE_VALUE] = &CTradeDlg2::OnActionCaptureValue;
#endif
}
