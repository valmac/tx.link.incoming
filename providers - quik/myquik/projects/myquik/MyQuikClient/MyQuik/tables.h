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
 *  tables.h
 */

#pragma once

#include <list>
#include <vector>
#include <std.h>
#include <common.h>
#include "accounts.h"

#define NR_QUOTE_COLUMNS_MAX 10

enum {
	FCOL_NULL = 0x0,
	FCOL_PRICE = 0x1,
	FCOL_BUY = 0x2,
	FCOL_SELL = 0x4,
	FCOL_VOLUME = 0x8,
	FCOL_STOP_BUY = 0x10,
	FCOL_STOP_SELL = 0x20,
};

enum {
	PROPERTY_NAME = 0x01,
	PROPERTY_ACCOUNT = 0x02,
	PROPERTY_CLIENTCODE = 0x04,
	PROPERTY_CLASSCODE = 0x08,
	PROPERTY_SECCODE = 0x10,
	PROPERTY_EXISTING = 0x100,
};

enum {
	I_PROPERTY_NAME,
	I_PROPERTY_ACCOUNT,
	I_PROPERTY_CLIENTCODE,
	I_PROPERTY_CLASSCODE,
	I_PROPERTY_SECCODE,
	I_PROPERTY_PRICE_NRDIGITS,
	I_PROPERTY_PRICE_STEP,
	I_PROPERTY_TRADE_QUANTITY,
	I_PROPERTY_TRADE_SPRED,
	I_PROPERTY_STOP_VALUE_RELATIVE,
	I_PROPERTY_STOP_VALUE_ABSOLUTE,
	I_PROPERTY_STOP_IS_RELATIVE,
	I_PROPERTY_PROFIT_VALUE_RELATIVE,
	I_PROPERTY_PROFIT_VALUE_ABSOLUTE,
	I_PROPERTY_PROFIT_IS_RELATIVE,
};

enum {
	TRADING_AUTO_STOP,
	TRADING_AUTO_PROFIT,
	TRADING_AUTO_LAST,
};

typedef std::list<double> ListOfDoubles;
//
// class ListOfUserValues
//
class ListOfUserValues : public ListOfDoubles
{
public:
	ListOfUserValues ();

	int Add (double val);

	double GetCurrentValue () const;

	int GetSize () const { return static_cast<int>(size());}
	BOOL IsEmpty () const {return empty();}
	BOOL NotEmpty () const {return ! IsEmpty();}

protected:
	enum {MAX_SIZE = 10};

protected:
	double current;

}; // class ListOfUserValues


//
// class QuoteTable
//
class QuoteTable 
{
	friend class Tables;
public:
	enum {
		I_BUY,
		I_SELL,
		I_NEUTRAL,
		I_LAST,
	};
	enum {
		ITEM_PRICE,
		ITEM_VOLUME,
		ITEM_USER_BID_BUY,
		ITEM_USER_BID_SELL,
		ITEM_USER_STOP_BUY,
		ITEM_USER_STOP_SELL,
		ITEM_USER_DEAL_BUY,
		ITEM_USER_DEAL_SELL,
		ITEM_LAST,
	};

	enum {ITEM_USER_FIRST = ITEM_USER_BID_BUY};
	enum {ITEM_USER_LAST = ITEM_USER_DEAL_SELL};

	enum {MAX_DEPTH = 100};

	QuoteTable();
	QuoteTable(LPCTSTR name);
	~QuoteTable();

	struct Array {
		struct Item {
			double values[ITEM_LAST];
			double user;
			int flags;
			void Clear() {memset(values, 0, sizeof(values)); user = 0; flags = 0x0;}
		};
		typedef ::std::vector<Item> Items;
		Items items;
		int count;
		int id;

		void Create(int size);
		void Clear(int full = 0); 
		void ClearItem(int line);

		Item * GetItem(int i) {return &items[i];}
		const Item * GetItem(int i) const {return &items[i];}

		BOOL IsEmpty() const {return items.empty();}
		int GetCapacity() const {return static_cast<int>(items.capacity());}

		Array & operator = (const Array & arr);
	};

	struct Properties {
		TCHAR name[128];
		TCHAR classcode[32];
		TCHAR seccode[32];
		TCHAR account[32];
		TCHAR clientcode[32];
		struct Price {
			int nd;
			double step;
		} price;
		struct Trading {			
			double quantity;
			double spread; 
			struct Autos {
				struct Item {
					double relative;
					double absolute;
					int isRelative;
					double slippage;
				};
				Item stop;
				Item profit;
			} autos;
		} trading;
		void Initialize(LPCTSTR name = NULL, LPCTSTR seccode = NULL, LPCTSTR classcode = NULL, 
			LPCTSTR clientcode = NULL, LPCTSTR account = NULL, int nd = 0);
	};

	struct Data {
		Array arrays[I_LAST];
	};

	int Create(LPCTSTR name, LPCTSTR seccode = NULL, LPCTSTR classcode = NULL, 
		LPCTSTR clientcode = NULL, LPCTSTR account = NULL, int nd = 0, int flags = 0x0);

	const Properties * GetProperties() const { return &properties; }
	void SetProperties(const Properties * pProperties);

	LPCTSTR GetName() const { return properties.name; }

	void Clear(int full = 0);

	//Array * CreateArray(LPCTSTR name);

	Array * GetArray(int i);
	const Array * GetArray(int i) const;

	//int GetSize() const { return SIZEOF_ARRAY(columns.items); }

	void SetBestPrices(double buy, double sell) { theBestBuyPrice = buy; theBestSellPrice = sell; }
	void GetBestPrices(double & buy, double & sell) const { buy = theBestBuyPrice; sell = theBestSellPrice; }

	void SetCurrentPrice(double price);
	double GetCurrentPrice() const { return this->price; }
	double GetPreviosPrice() const { return this->prevPrice; }
	double GetPreviosChangePrice() const { return this->prevChangePrice; }

	void SetCurrentPercent(double percent) { this->percent = percent; }
	double GetCurrentPercent() const { return this->percent; }

	void SetMinMax(double minimum, double maximum) { this->priceMin = minimum; this->priceMax = maximum; }
	void GetMinMax(double & minimum, double & maximum) const { minimum = this->priceMin; maximum = this->priceMax; }

	void SetVolumeMinMax(double minimum, double maximum) { this->volumeMin = minimum; this->volumeMax = maximum; }
	void GetVolumeMinMax(double & minimum, double & maximum) const { minimum = this->volumeMin; maximum = this->volumeMax; }

	void SetDemandAndSupply(double demand, double supply) { this->demand = demand; this->supply = supply; }
	void GetDemandAndSupply(double & demand, double & supply) const { demand = this->demand; supply = this->supply; }

	void SetPricesBidOffer(double priceBid, double priceOffer) { this->priceBid = priceBid; this->priceOffer = priceOffer; }
	void GetPricesBidOffer(double & priceBid, double & priceOffer) const { priceBid = this->priceBid; priceOffer = this->priceOffer; }

	void SetPriceUser(double price) {this->priceUser = price;}

	void SetAccount(const Account * pAccount) { this->pAccount = pAccount; }
	const Account * GetAccount() const { return this->pAccount; }

public:
	QuoteTable & operator = (const QuoteTable & src);

protected:
	void Init(LPCTSTR name, int flags = 0x0);

public:
	friend void Copy(QuoteTable & dst, const QuoteTable & src);
	friend void CopyPrices(QuoteTable & dst, const QuoteTable & src);
	friend void CopyVolumes(QuoteTable & dst, const QuoteTable & src);

	double theBestBuyPrice;
	double theBestSellPrice;
	double price;
	double prevPrice;
	double prevChangePrice;
	double demand;
	double supply;
	double percent;
	double priceMin;
	double priceMax;
	double priceBid;
	double priceOffer;
	double volumeMin;
	double volumeMax;
	double priceUser;

public:
	struct User {
		ListOfUserValues ps; // список цен;
		ListOfUserValues qs;
		ListOfUserValues sps;
		ListOfUserValues sps2;
		ListOfUserValues sqs;
	} user;

protected:
	Data data;
	Properties properties;

	const Account * pAccount;

}; // class QuoteTable

class ListOfTableProperties : public std::list<QuoteTable::Properties>
{
public:
	enum {
		F_ADD = 0x1,
		F_UPDATE = 0x2,
		F_REMOVE = 0x4,
		F_CLEAR = 0x8,
	};

	ListOfTableProperties() : pCurrent(NULL)
	{}

	int Add(const QuoteTable::Properties * pProperties);
	int Update(LPCTSTR name, const QuoteTable::Properties * pProperties);
	int Remove(LPCTSTR name);
	int Clear();

	int UpdateProperties(LPCTSTR name, const QuoteTable::Properties * pProperties, int flags);

	const QuoteTable::Properties * GetItem(LPCTSTR name, int flags);
	const QuoteTable::Properties * GetItem(LPCTSTR name, LPCTSTR seccode, LPCTSTR classcode, LPCTSTR clientcode, LPCTSTR account);
	const QuoteTable::Properties * GetCurrentItem();

protected:
	QuoteTable::Properties * pCurrent;
	QuoteTable::Properties properties;
};

typedef std::list<QuoteTable*> ListOfTables;
class Tables : public ListOfTables
{
public:
	typedef ListOfTables::iterator Iterator;

	Tables();
	~Tables();

	QuoteTable * FindTable(LPCTSTR name, int flags);
	QuoteTable * FindTable(LPCTSTR name, LPCTSTR seccode, LPCTSTR classcode, LPCTSTR clientcode, LPCTSTR account);
	QuoteTable * CreateTable(LPCTSTR name, const QuoteTable::Properties * pProperties);

	int GetCount() const { return size(); }

	const QuoteTable * GetCurrentTable() const { return pCurrent; }
	QuoteTable * GetCurrentTable() { return pCurrent; }
	void SetCurrentTable(QuoteTable * pCurrent) { this->pCurrent = pCurrent; }

	QuoteTable * GetFirstTable(Tables::Iterator & it);
	QuoteTable * GetNextTable(Tables::Iterator & it, int cycle = 1);
	QuoteTable * GetPreviosTable(Tables::Iterator & it, int cycle = 1);

	QuoteTable * GetFirstTable();
	QuoteTable * GetNextTable(int cycle = 1);
	QuoteTable * GetPreviosTable(int cycle = 1);

	void UpdateTables(const Account * pAccount);
	void UpdateTables(const ListOfTableProperties * pListOfProperties);	

protected:
	QuoteTable * pCurrent;

public:
	QuoteTable table;
};


extern void LoadTableProperties(LPCTSTR name, QuoteTable::Properties * pProperties);
extern void SaveTableProperties(LPCTSTR name, const QuoteTable::Properties * pProperties);
extern void RemoveTableProperties(LPCTSTR name);

extern void LoadTableProperties(ListOfTableProperties * pListOfProperties);
extern void SaveTableProperties(const ListOfTableProperties * pListOfProperties);

