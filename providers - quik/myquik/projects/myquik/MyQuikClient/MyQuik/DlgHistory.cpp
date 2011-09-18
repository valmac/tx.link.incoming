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
 *  DlgHistory.cpp : implementation file
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "DlgHistory.h"
#include "dlg.h"
#include "history.h"
#include <time.h>
#include <atltime.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

LPCTSTR GetMonthName(int i) 
{
	static LPCTSTR names[] = {
		TEXT("Январь"),
		TEXT("Февраль"),
		TEXT("Март"),
		TEXT("Апрель"),
		TEXT("Май"),
		TEXT("Июнь"),
		TEXT("Июль"),
		TEXT("Август"),
		TEXT("Сентябрь"),
		TEXT("Октябрь"),
		TEXT("Ноябрь"),
		TEXT("Декабрь"),
	};
	return names[i];
}

LPCTSTR GetMonthName2(int i) 
{
	static LPCTSTR names[] = {
		TEXT("января"),
		TEXT("февраля"),
		TEXT("марта"),
		TEXT("апреля"),
		TEXT("мая"),
		TEXT("июня"),
		TEXT("июля"),
		TEXT("августа"),
		TEXT("сентября"),
		TEXT("октября"),
		TEXT("ноября"),
		TEXT("декабря"),
	};
	return names[i];
}

const int MILLENIUM = 2000;
int calendar[200][12][32]; // 1900-2099

struct FilterItem {
	HTREEITEM hItem;
	int is;
	int active;
};

inline int YEAR2IYEAR(int year)
{
	int iYear;
	if (year >= MILLENIUM)
		iYear = 100 + year - MILLENIUM;
	else
		iYear = year - 1900;
	return iYear;
}

inline int IYEAR2YEAR(int iYear)
{
	int year;
	if (iYear < 100)
		year = 1900 + iYear;
	else
		year = MILLENIUM + iYear - 100;
	return year;
}

inline int IMONTH2MONTH(int iMonth)
{
	int month = iMonth + 1;
	return month;
}

inline int MONTH2IMONTH(int month)
{
	int iMonth = month - 1;
	return iMonth;
}

#define MAKE_TREECTRL_ITEMDATA(Y, M, D) (DWORD_PTR)(0x80000000 | ((Y) << 9) | ((M) << 5) | (D))

#define TREECTRL_ITEMDATA_TO_YEAR(data) (int)(((data) >> 9) & 0xffff)
#define TREECTRL_ITEMDATA_TO_MONTH(data) (int)(((data) >> 5) & 0xf)
#define TREECTRL_ITEMDATA_TO_DAY(data) (int)((data) & 0x1f)

#define TREECTRL_ITEMDATA_IS_DATE(data) (0x80000000 & (data))

static void GetTimeYesterday(const CTime & time, SYSTEMTIME & stime)
{
	CTimeSpan span(1, 0, 0, 0);
	CTime time2 = time - span;
	time2.GetAsSystemTime(stime);
}

static void GetTime7Days(const CTime & time, SYSTEMTIME & stime)
{
	CTimeSpan span7(7, 0, 0, 0);
	CTime time7 = time - span7;
	time7.GetAsSystemTime(stime);
}

static void GetTime31Days(const CTime & time, SYSTEMTIME & stime)
{
	CTimeSpan span31(31, 0, 0, 0);
	CTime time31 = time - span31;
	time31.GetAsSystemTime(stime);
}

static void GetTimeMonth(const CTime & time, SYSTEMTIME & stimeMonth)
{
	CTimeSpan spanMonth(time.GetDay() - 1, 0, 0, 0);
	CTime timeMonth = time - spanMonth;
	timeMonth.GetAsSystemTime(stimeMonth);
}

static void GetTimeYear(const SYSTEMTIME & stime, SYSTEMTIME & stimeYear)
{
	CTime timeYear(stime.wYear, 1, 1, 0, 0, 0);
	timeYear.GetAsSystemTime(stimeYear);
}

static void GetTimeLastYear(const SYSTEMTIME & stime, SYSTEMTIME & stime1, SYSTEMTIME & stime2)
{
	stime1.wYear = stime.wYear - 1; stime1.wMonth = 12; stime1.wDay = 31;
	stime1.wHour = 23; stime1.wMinute = 59; stime1.wSecond = 59;
	stime1.wDayOfWeek = 0;

	stime2.wYear = stime.wYear - 1; stime2.wMonth = 1; stime2.wDay = 1;
	stime2.wHour = 0; stime2.wMinute = 0; stime2.wSecond = 0;
	stime2.wDayOfWeek = 0;
}

static void GetTimeOldest(const SYSTEMTIME & stime, SYSTEMTIME & stime1, SYSTEMTIME & stime2)
{
	stime1.wYear = stime.wYear - 2; stime1.wMonth = 12; stime1.wDay = 31;
	stime1.wHour = 23; stime1.wMinute = 59; stime1.wSecond = 59;
	stime1.wDayOfWeek = 0;

	stime2.wYear = 1900; stime2.wMonth = 1; stime2.wDay = 1;
	stime2.wHour = 0; stime2.wMinute = 0; stime2.wSecond = 0;
	stime2.wDayOfWeek = 0;
}

//
// class CHistoryTreeCtrl
//

CHistoryTreeCtrl::CHistoryTreeCtrl()
{
}

void CHistoryTreeCtrl::OnMouseMove(UINT nFlags, CPoint point)
{
	if (nFlags & MK_LBUTTON)
	{
		CWindow wnd = GetParent();
		::SendMessage(wnd, UM_UPDATE, (WPARAM)m_hWnd, WM_SIZING);
	}
}

void CHistoryTreeCtrl::OnLButtonUp(UINT nFlags, CPoint point)
{
	CWindow wnd = GetParent();
	::SendMessage(wnd, UM_UPDATE, (WPARAM)m_hWnd, WM_LBUTTONUP);
}

//
// class CHistoryListCtrl
//

CHistoryListCtrl::CHistoryListCtrl()
{
}

void CHistoryListCtrl::OnMouseMove(UINT nFlags, CPoint point)
{
	if (nFlags & MK_LBUTTON)
	{
		CWindow wnd = GetParent();
		::SendMessage(wnd, UM_UPDATE, (WPARAM)m_hWnd, WM_SIZING);
	}
}

void CHistoryListCtrl::OnLButtonUp(UINT nFlags, CPoint point)
{
	CWindow wnd = GetParent();
	SendMessage(wnd, UM_UPDATE, (WPARAM)m_hWnd, WM_LBUTTONUP);
}

//
// class CMyDelimiter
//

CMyDelimiter::CMyDelimiter()
{
}

int CMyDelimiter::GetWidth() const
{
	CRect rect;
	GetWindowRect(&rect);
	return rect.Width();
}

void CMyDelimiter::OnMouseMove(UINT nFlags, CPoint point)
{
#if 0
	if (nFlags & MK_LBUTTON)
	{
		CWindow wnd = GetParent();
		::SendMessage(wnd, UM_UPDATE, (WPARAM)m_hWnd, WM_SIZING);
	}
#endif
}

void CMyDelimiter::OnLButtonDown(UINT nFlags, CPoint point)
{
	CWindow wnd = GetParent();
	::SendMessage(wnd, UM_UPDATE, (WPARAM)m_hWnd, WM_LBUTTONDOWN);
}

void CMyDelimiter::OnLButtonUp(UINT nFlags, CPoint point)
{
	CWindow wnd = GetParent();
	::SendMessage(wnd, UM_UPDATE, (WPARAM)m_hWnd, WM_LBUTTONUP);
}

void CMyDelimiter::OnPaint(CDCHandle )
{
	CPaintDC dc(*this);

	CRect rect;
	GetClientRect(&rect);

	COLORREF colorBtnFace = ::GetSysColor(COLOR_BTNFACE);
	COLORREF colorLight = ::GetSysColor(COLOR_3DLIGHT);
	COLORREF colorHighLight = ::GetSysColor(COLOR_3DHILIGHT);
	COLORREF colorBorder = ::GetSysColor(COLOR_ACTIVEBORDER);

	CBrush brush;
	brush.CreateSolidBrush(colorBtnFace); // COLOR_BTNFACE COLOR_BTNHILIGHT COLOR_SCROLLBAR COLOR_ACTIVEBORDER
	dc.SelectBrush(brush);

	dc.FillRect(&rect, brush);

	CPen penLight, penHighLight, penBorder;
	penLight.CreatePen(PS_SOLID, 1, colorLight);
	penHighLight.CreatePen(PS_SOLID, 1, colorHighLight);
	penBorder.CreatePen(PS_SOLID, 1, colorBorder);

	// Рисуем вертикальный разделитель:
	//dc.SelectPen(penLight);
	//dc.MoveTo(rect.left, rect.top);
	//dc.LineTo(rect.left, rect.bottom);
	dc.SelectPen(penBorder);
	dc.MoveTo(rect.right - 2, rect.top);
	dc.LineTo(rect.right - 2, rect.bottom);
	dc.SelectPen(penHighLight);
	dc.MoveTo(rect.left, rect.top);
	dc.LineTo(rect.left, rect.bottom);
	//dc.MoveTo(rect.left + 1, rect.top);
	//dc.LineTo(rect.left + 1, rect.bottom);
	dc.MoveTo(rect.right - 1, rect.top);
	dc.LineTo(rect.right - 1, rect.bottom);
}

//
// class CHistoryEdit
//
CHistoryEdit::CHistoryEdit()
{
}

CHistoryEdit::~CHistoryEdit()
{
#if defined _DEBUG && 0
	_asm nop;
#endif
}

void CHistoryEdit::OnMouseMove(UINT nFlags, CPoint point)
{
	if (nFlags & MK_LBUTTON)
	{
		CWindow wnd = GetParent();
		::SendMessage(wnd, UM_UPDATE, (WPARAM)m_hWnd, WM_SIZING);
	}
}

void CHistoryEdit::OnLButtonUp(UINT nFlags, CPoint point)
{
	CWindow wnd = GetParent();
	::SendMessage(wnd, UM_UPDATE, (WPARAM)m_hWnd, WM_LBUTTONUP);
}

//
// CDlgHistory dialog
//
CDlgHistory::CDlgHistory()
{
	m_pHistory = NULL;
	m_initialized = FALSE;
	m_treeCtrlCtrl.modified = FALSE;
	m_treeCtrlCtrl.width = 0;
	SetNoData();
	SetNoFilters();
	Set(MODE_NORMAL);
}

CDlgHistory::~CDlgHistory()
{
#if 0
	this->DestroyWindow();
#endif
}

void CDlgHistory::Attach(History * history)
{
	m_pHistory = history;
}

LPCTSTR CDlgHistory::GetTreeText(int index)
{
	LPCTSTR names[] = {
		TEXT("Журнал"),
		TEXT("Сегодня"),
		TEXT("Вчера"),
		TEXT("Вчера и сегодня"),
		TEXT("За 7 дней"),
		TEXT("За 31 день"),
		TEXT("С начала месяца"),
		TEXT("С начала года"),
		TEXT("В прошлом году"),
		TEXT("Ещё раньше"),
		TEXT("За всё время"),
		TEXT("---"),
		TEXT("Нет данных"),
	};
	if (index >= 0 && index < SIZEOF_ARRAY(names))
		return names[index];
	else
		return NULL;
}

void CDlgHistory::UpdateTreeCtrl()
{
	int index;

	HTREEITEM hSel = m_treeCtrl.GetSelectedItem();
	DWORD_PTR sel = I_TREE_NODATA;
	if (hSel)
		sel = m_treeCtrl.GetItemData(hSel);

	m_treeCtrl.DeleteAllItems();
	SetNoFilters();

	HTREEITEM hRoot, hItem, hItemDay;
	TVINSERTSTRUCT tvInsert;
	tvInsert.hParent = NULL;
	tvInsert.hInsertAfter = NULL;
	tvInsert.item.mask = TVIF_TEXT;//|TVIF_IMAGE|TVIF_SELECTEDIMAGE;
	tvInsert.item.pszText = (LPTSTR)GetTreeText(I_TREE_ROOT);
	//tvInsert.item.iImage = -1;
	//tvInsert.item.iSelectedImage = -1;
	hRoot = m_treeCtrl.InsertItem(&tvInsert);
	History * history = GetHistory();
	if (history && !history->IsEmpty())
	{
		Transactions::Iterator it;
		Transaction * pTa;
		int found;
		
		SYSTEMTIME stime;
		GetLocalTime(&stime);
		CTime time(stime);

		struct _Times {
			SYSTEMTIME stime1;
			SYSTEMTIME stime2;
		};
		_Times times[I_TREE_LAST];
		_Times * pTime;
		int founds[SIZEOF_ARRAY(times)];
		
		// Сегодня:
		pTime = &times[I_TREE_TODAY];
		pTime->stime1 = stime;
		pTime->stime2 = stime;
		// Вчера:	
		SYSTEMTIME stime2;
		GetTimeYesterday(time, stime2);
		pTime = &times[I_TREE_YESTERDAY];
		pTime->stime1 = stime2;
		pTime->stime2 = stime2;
		// Вчера и сегодня:
		pTime = &times[I_TREE_2DAYS];
		pTime->stime1 = stime;
		pTime->stime2 = stime2;
		// За 7 дней:		
		pTime = &times[I_TREE_7DAYS];
		pTime->stime1 = stime;
		GetTime7Days(time, pTime->stime2);
		// За 31 день:
		pTime = &times[I_TREE_31DAYS];
		GetTime31Days(time, pTime->stime2);
		pTime->stime1 = stime;
		// С начала месяца:
		pTime = &times[I_TREE_MONTH];
		pTime->stime1 = stime;
		GetTimeMonth(time, pTime->stime2);
		// С начала года:
		pTime = &times[I_TREE_YEAR];
		pTime->stime1 = stime;
		GetTimeYear(stime, pTime->stime2);
		// В прошлом году:
		pTime = &times[I_TREE_LASTYEAR];
		GetTimeLastYear(stime, pTime->stime1, pTime->stime2);
		// Самые старые записи:
		pTime = &times[I_TREE_OLDEST];
		GetTimeOldest(stime, pTime->stime1, pTime->stime2);

		TSTRING_STD(text);
		for (int i = I_TREE_ROOT + 1; i < I_TREE_FULL; i++)
		{
			index = i;
			founds[i] = 0;
			history->FindFirstTransaction(it);
			found = 0;
			for (;;)
			{
				pTime = &times[i];
				pTa = history->FindTransactionByDate(pTime->stime2, pTime->stime1, it);
				if (pTa)
				{
					++found;
				}
				else
					break;
			} // for (;;)
			founds[i] = found;
			if (found)
			{
				_stprintf_s(text, SIZEOF_ARRAY(text), TEXT("%s (%d)"), GetTreeText(index), found);
				if (index == I_TREE_2DAYS)
				{
					if (!founds[I_TREE_TODAY] || !founds[I_TREE_YESTERDAY])
						found = 0;
				}				
			}
			else
				_stprintf_s(text, SIZEOF_ARRAY(text), TEXT("%s"), GetTreeText(index));

			if (found)
			{
				hItem = m_treeCtrl.InsertItem(TVIF_TEXT, text, 0, 0, 0, 0, 0, hRoot, NULL);
				m_treeCtrl.SetItemData(hItem, (DWORD_PTR)index);
				SetFilter(index);
			}
		} // for (i)

		// По месяцам:
		int iYear, iMonth, iDay;		
		memset(&calendar, 0, sizeof(calendar));
		history->FindFirstTransaction(it);
		for (;;)
		{
			pTa = history->FindNextTransaction(it);
			if (pTa != NULL)
			{				
				iYear = YEAR2IYEAR(pTa->time.wYear);
				iMonth = MONTH2IMONTH(pTa->time.wMonth);
				iDay = pTa->time.wDay;
				calendar[iYear][iMonth][0] += 1;
				calendar[iYear][iMonth][iDay] += 1;
			}
			else
				break;
		} // for (;;)

		int year;
		for (iYear = 199; iYear >= 0; iYear--)
		{
			for (iMonth = 11; iMonth >= 0; iMonth--)
			{
				found = calendar[iYear][iMonth][0];
				if (found)
				{
					LPCTSTR name = GetMonthName(iMonth);
					year = IYEAR2YEAR(iYear);
					if (stime.wYear == year)
						_stprintf_s(text, SIZEOF_ARRAY(text), TEXT("%s (%d)"), name, found);
					else
						_stprintf_s(text, SIZEOF_ARRAY(text), TEXT("%s %d (%d)"), name, year, found);
					hItem = m_treeCtrl.InsertItem(TVIF_TEXT, text, 0, 0, 0, 0, 0, hRoot, NULL);
					m_treeCtrl.SetItemData(hItem, MAKE_TREECTRL_ITEMDATA(iYear, iMonth, 0));
					// Добавляем дни:
					for (iDay = 1; iDay <= 31; iDay++)
					{
						int n = calendar[iYear][iMonth][iDay];
						if (n > 0)
						{
							LPCTSTR name2 = GetMonthName2(iMonth);
							_stprintf_s(text, SIZEOF_ARRAY(text), TEXT("%d %s (%d)"), iDay, name2, n);
							hItemDay = m_treeCtrl.InsertItem(TVIF_TEXT, text, 0, 0, 0, 0, 0, hItem, NULL);
							m_treeCtrl.SetItemData(hItemDay, MAKE_TREECTRL_ITEMDATA(iYear, iMonth, iDay));
						}
					} // for (iDay)
#if 0
					m_treeCtrl.Expand(hItem, TVE_EXPAND);
#endif
				}
			} // for (iMonth)
		} // for (iYear)

		// Вся история:
		index = I_TREE_FULL;
		found = history->GetNrTransactions();
		if (found)
		{
			_stprintf_s(text, SIZEOF_ARRAY(text), TEXT("%s (%d)"), GetTreeText(index), found);
			hItem = m_treeCtrl.InsertItem(TVIF_TEXT, text, 0, 0, 0, 0, 0, hRoot, NULL);
			m_treeCtrl.SetItemData(hItem, (DWORD_PTR)index);
		}
#if 0
		m_treeCtrl.EnableWindow();
#endif
		SetNoData(FALSE);
	}
	else
	{
		index = I_TREE_NODATA;
		hItem = m_treeCtrl.InsertItem(TVIF_TEXT, GetTreeText(index), 0, 0, 0, 0, 0, hRoot, NULL);
		m_treeCtrl.SetItemData(hItem, (DWORD_PTR)index);
#if 0
		m_treeCtrl.EnableWindow(FALSE);
#endif		
		SetNoData();
	}
	m_treeCtrl.Expand(hRoot, TVE_EXPAND);

	if (m_initialized)
	{
#if 0
		int found = 0;
		hItem = m_treeCtrl.GetFirstVisibleItem();
		if (hItem)
		{
			do 
			{
				index = (int)m_treeCtrl.GetItemData(hItem);
				if (index == m_treeCtrlCtrl.index)
				{
					found = 1;
					break;
				}
				hItem = m_treeCtrl.GetNextVisibleItem(hItem);
			}
			while (hItem);
		}
		if (found)
			m_treeCtrl.SelectItem(hItem);
		else
#else
		//if (sel != I_TREE_NODATA)
#endif
		{

		//	return;
		}
		m_treeCtrl.SelectItem(hRoot);
	}
}

int CDlgHistory::Update(const Transaction * pTa, int flags)
{
	HTREEITEM hItem, hRoot, hParent, hStart, hStop, hPrev, hInsertAfter;
	HTREEITEM hSel;
	DWORD_PTR data;
	
	TSTRING_STD(text);
	
	int iYear, iMonth, iDay;
	int year, month, day;

	hParent = hStart = hStop = hPrev = hInsertAfter = NULL;
	hSel = NULL;

	hRoot = m_treeCtrl.GetRootItem();

	if (! HaveData())
	{
		// Удаляем элемент "Нет данных":
		if (hRoot)
		{		
			while (hItem = m_treeCtrl.GetNextItem(hRoot, TVGN_CHILD))
				m_treeCtrl.DeleteItem(hItem);
		}
		// Текущим является самый первый элемент:
		m_treeCtrl.SelectItem(hRoot);
		m_listCtrl.EnableWindow();

		SetNoData(FALSE);
	}

	FilterItem filters[I_TREE_LAST];
	memset(filters, 0, sizeof(filters));

	SYSTEMTIME stime, stime1, stime2;
	GetLocalTime(&stime);
	CTime time(stime);

	DWORD d, d0, d1, d2;
	d = GetTimeStampDate(pTa);
	d2 = GetTimeStampDate(stime);	

	BOOL updateList = FALSE;
	BOOL found = TRUE;
	int _2days = 0;

	FilterItem * pFilter;
	// Проверяем какие фильтры нужно добавить:
	pFilter = &filters[I_TREE_TODAY];
	if (d == d2)
		pFilter->active++;		
	pFilter = &filters[I_TREE_YESTERDAY];
	GetTimeYesterday(time, stime2);
	d1 = GetTimeStampDate(stime2);
	if (d == d1)
		pFilter->active++;
	pFilter = &filters[I_TREE_2DAYS];
	if (d == d1 || d == d2)
	{		
		int checkToday = CheckFilter(I_TREE_TODAY);
		int checkYestrday = CheckFilter(I_TREE_YESTERDAY);
		if ((filters[I_TREE_YESTERDAY].active && checkToday) || (filters[I_TREE_TODAY].active && checkYestrday) ||
			(checkToday && checkYestrday) || ((d == d1) && checkToday) || ((d == d2) && checkYestrday))
		{
			pFilter->active++;
		}
	}
	pFilter = &filters[I_TREE_7DAYS];
	GetTime7Days(time, stime2);
	d1 = GetTimeStampDate(stime2);
	if (d >= d1 && d <= d2)
		pFilter->active++;
	pFilter = &filters[I_TREE_31DAYS];
	GetTime31Days(time, stime2);
	d1 = GetTimeStampDate(stime2);
	if (d >= d1 && d <= d2)
		pFilter->active++;
	pFilter = &filters[I_TREE_MONTH];
	GetTimeMonth(time, stime2);
	d1 = GetTimeStampDate(stime2);
	if (d >= d1 && d <= d2)
		pFilter->active++;
	pFilter = &filters[I_TREE_YEAR];
	GetTimeYear(stime, stime2);
	d1 = GetTimeStampDate(stime2);
	if (d >= d1 && d <= d2)
		pFilter->active++;
	pFilter = &filters[I_TREE_LASTYEAR];
	GetTimeLastYear(stime, stime1, stime2);
	d1 = GetTimeStampDate(stime1);
	d0 = GetTimeStampDate(stime2);
	if (d >= d0 && d <= d1)
		pFilter->active++;
	pFilter = &filters[I_TREE_OLDEST];
	GetTimeOldest(stime, stime1, stime2);
	d1 = GetTimeStampDate(stime1);
	d0 = GetTimeStampDate(stime2);
	if (d >= d0 && d <= d1)
		pFilter->active++;
	pFilter = &filters[I_TREE_FULL];
	pFilter->active++;

	pFilter = &filters[I_TREE_ROOT];
	pFilter->active++;

	d0 = MAKE_TIME_STAMP_DATE(pTa->time.wYear, pTa->time.wMonth, 0);

	if (flags & History::F_ADD)
	{
		hItem = m_treeCtrl.GetNextItem(hRoot, TVGN_CHILD);
		if (hItem)
		{
			// Проверяем какие фильтры нужно обновить:
			while (hItem)
			{
				data = m_treeCtrl.GetItemData(hItem);
				if (! TREECTRL_ITEMDATA_IS_DATE(data))
				{
					if (data != I_TREE_NODATA)
					{
						pFilter = &filters[data];					
						pFilter->hItem = hItem;
						pFilter->is = TRUE;

						if (data == I_TREE_FULL)
							hStop = hItem;
#if 0
						else if (data == I_TREE_YESTERDAY || data == I_TREE_TODAY)
							_2days++;
						else if (data == I_TREE_2DAYS)
							_2days--;
#endif
					}
				}
				hPrev = hItem;
				hItem = m_treeCtrl.GetNextItem(hItem, TVGN_NEXT);
			} // while (hItem)
		}
#if 0
		if (_2days == 2)
		{
			pFilter = &filters[I_TREE_2DAYS];
			if (! pFilter->active)
				pFilter->active++;
		}
#endif		
		// Добавляем нужные фильтры:
		for (int i = 1; i < SIZEOF_ARRAY(filters); i++)
		{
			pFilter = &filters[i];
			if (pFilter->active)
			{
				if (! pFilter->is)
				{
					// Вставляем фильтр в установленном порядке:
					hItem = m_treeCtrl.GetNextItem(hRoot, TVGN_CHILD);					
					hInsertAfter = NULL;
					hPrev = TVI_FIRST;
					while (hItem)
					{
						data = m_treeCtrl.GetItemData(hItem);
						if (! TREECTRL_ITEMDATA_IS_DATE(data))
						{
							if (i < (int)data)
							{
								hInsertAfter = hPrev;
								break;
							}
						}
						hPrev = hItem;
						hItem = m_treeCtrl.GetNextItem(hItem, TVGN_NEXT);
					} // while (hItem)
					if (hInsertAfter == NULL)
						hInsertAfter = hPrev;

					int n = 0;

					if (i == I_TREE_2DAYS)
						n = 1;
					_stprintf_s(text, SIZEOF_ARRAY(text), TEXT("%s (%d)"), GetTreeText(i), n);
					hItem = m_treeCtrl.InsertItem(TVIF_TEXT, text, 0, 0, 0, 0, 0, hRoot, hInsertAfter);
					m_treeCtrl.SetItemData(hItem, (DWORD_PTR)i);
					pFilter->is++;

					SetFilter(i);
				}
				else
					hItem = pFilter->hItem;
				if (hItem != NULL)
				{
					UpdateItemText(hItem, text, SIZEOF_ARRAY(text) - 1, +1);

					if (i == I_TREE_FULL)
						hStop = hItem;
				}
			}
		} // for (i)

		// Теперь добавляем/обновляем фильтры по месяцам и дням:
		LPCTSTR name;		
		int add = 1;
		DWORD date;
		hItem = m_treeCtrl.GetNextItem(hRoot, TVGN_CHILD);
		hPrev = hItem;
		hInsertAfter = NULL;
		while (hItem != hStop)
		{
			data = m_treeCtrl.GetItemData(hItem);
			if (TREECTRL_ITEMDATA_IS_DATE(data))
			{// Это месяц			
				iYear = TREECTRL_ITEMDATA_TO_YEAR(data);
				year = IYEAR2YEAR(iYear);
				iMonth = TREECTRL_ITEMDATA_TO_MONTH(data);
				month = IMONTH2MONTH(iMonth);
				day = TREECTRL_ITEMDATA_TO_DAY(data);

				date = MAKE_TIME_STAMP_DATE(year, month, 0);
				if (d0 == date)
				{// Уже добавлено, остаётся только обновить текст
					add = 0;
					break;
				}
				else if (d0 > date)
				{// Перебор
					add = 1;
					hInsertAfter = hPrev;
					break;
				}
			}
			hPrev = hItem;
			hItem = m_treeCtrl.GetNextItem(hItem, TVGN_NEXT);
		} // while (hItem)
	
		if (add)
		{
			if (hInsertAfter == NULL)
				hInsertAfter = m_treeCtrl.GetNextItem(hItem, TVGN_PREVIOUS);

			year = pTa->time.wYear;
			iYear = YEAR2IYEAR(year);
			iMonth = MONTH2IMONTH(pTa->time.wMonth);
			name = GetMonthName(iMonth);
			if (stime.wYear == year)
				_stprintf_s(text, SIZEOF_ARRAY(text), TEXT("%s (%d)"), name, 0);
			else
				_stprintf_s(text, SIZEOF_ARRAY(text), TEXT("%s %d (%d)"), name, year, 0);
			hItem = m_treeCtrl.InsertItem(TVIF_TEXT, text, 0, 0, 0, 0, 0, hRoot, hInsertAfter);
			m_treeCtrl.SetItemData(hItem, MAKE_TREECTRL_ITEMDATA(iYear, iMonth, 0));			
		} // if (add)

		// Обновляем текст:
		if (hItem != NULL)
			UpdateItemText(hItem, text, SIZEOF_ARRAY(text) - 1, +1);

		// Теперь по дням:
		add = 1;
		hParent = hItem;
		hItem = m_treeCtrl.GetNextItem(hParent, TVGN_CHILD);		
		hPrev = TVI_FIRST;
		hInsertAfter = NULL;		
		while (hItem)
		{
			data = m_treeCtrl.GetItemData(hItem);
			//if (TREECTRL_ITEMDATA_IS_DATE(data))
			{
				iYear = TREECTRL_ITEMDATA_TO_YEAR(data);
				year = IYEAR2YEAR(iYear);
				iMonth = TREECTRL_ITEMDATA_TO_MONTH(data);
				month = IMONTH2MONTH(iMonth);
				day = TREECTRL_ITEMDATA_TO_DAY(data);

				date = MAKE_TIME_STAMP_DATE(year, month, day);
				if (d == date)
				{// Уже добавлено, остаётся только обновить текст
					add = 0;
					break;
				}
				else if (d < date)
				{// Перебор
					add = 1;
					hInsertAfter = hPrev;
					break;
				}
			}
			hPrev = hItem;
			hItem = m_treeCtrl.GetNextItem(hItem, TVGN_NEXT);
		} // while (hItem)

		if (add)
		{
			if (hInsertAfter == NULL)
				hInsertAfter = hPrev;

			year = pTa->time.wYear;
			iYear = YEAR2IYEAR(year);
			iMonth = MONTH2IMONTH(pTa->time.wMonth);
			iDay = pTa->time.wDay;
			LPCTSTR name2 = GetMonthName2(iMonth);

			_stprintf_s(text, SIZEOF_ARRAY(text), TEXT("%d %s (%d)"), iDay, name2, 0);
			hItem = m_treeCtrl.InsertItem(TVIF_TEXT, text, 0, 0, 0, 0, 0, hParent, hInsertAfter);
			m_treeCtrl.SetItemData(hItem, MAKE_TREECTRL_ITEMDATA(iYear, iMonth, iDay));
		} // if (add)

		if (hItem != NULL)
			UpdateItemText(hItem, text, SIZEOF_ARRAY(text) - 1, +1);

	} // if (flags & History::F_ADD)

	// Теперь проверяем нужно ли обновлять список:
	hSel = m_treeCtrl.GetSelectedItem();
	data = m_treeCtrl.GetItemData(hSel);
	if (TREECTRL_ITEMDATA_IS_DATE(data))
	{
		iYear = TREECTRL_ITEMDATA_TO_YEAR(data);
		year = IYEAR2YEAR(iYear);
		iMonth = TREECTRL_ITEMDATA_TO_MONTH(data);
		month = IMONTH2MONTH(iMonth);
		day = TREECTRL_ITEMDATA_TO_DAY(data);
		DWORD date = MAKE_TIME_STAMP_DATE(year, month, day);
		DWORD date2 = (day) ? d : d0;
		if (date == date2)
			updateList = TRUE;
	}
	else
	{
		unsigned int index = (int)data;
		if (index < SIZEOF_ARRAY(filters))
		{
			pFilter = &filters[index];
			if (pFilter->active)
				updateList = TRUE;
		}
	}

	if (updateList)
	{// Добавляем элемент в список:
		if (flags & History::F_ADD)
		{
			int count = m_listCtrl.GetItemCount();
			m_listCtrl.AddTransaction(pTa, theApp.FindTableProperty(pTa), 0, 0);

			m_listCtrl.DoSort();
			m_listCtrl.DoScroll();					

			if (count == 0)
				m_listCtrl.EnableWindow();
		}
		else if (flags & History::F_UPDATE)
		{
			m_listCtrl.UpdateTransaction(pTa, theApp.FindTableProperty(pTa), 0, 0);
		}
	} // if (updateList)
	return S_OK;
}

int CDlgHistory::Remove(const Transaction * pTa, int iItem)
{	
	HTREEITEM hItem, hSel, hNext, hRoot, hDate, hChild, hFilter;
	DWORD_PTR data;
	TSTRING_STD(text);
	int iYear, iMonth;
	int year, month, day;
	int val;
	BOOL deleteFilter = FALSE;
	
	FilterItem filters[I_TREE_LAST];
	memset(filters, 0, sizeof(filters));

	SYSTEMTIME stime, stime1, stime2;
	GetLocalTime(&stime);
	CTime time(stime);

	DWORD d, d0, d1, d2;
	d = GetTimeStampDate(pTa);
	d2 = GetTimeStampDate(stime);	

	FilterItem * pFilter;
	// Проверяем какие фильтры нужно добавить:
	pFilter = &filters[I_TREE_TODAY];
	if (d == d2)
		pFilter->active++;		
	pFilter = &filters[I_TREE_YESTERDAY];
	GetTimeYesterday(time, stime2);
	d1 = GetTimeStampDate(stime2);
	if (d == d1)
		pFilter->active++;
	pFilter = &filters[I_TREE_2DAYS];
	if (d == d1 || d == d2)
		pFilter->active++;
	pFilter = &filters[I_TREE_7DAYS];
	GetTime7Days(time, stime2);
	d1 = GetTimeStampDate(stime2);
	if (d >= d1 && d <= d2)
		pFilter->active++;
	pFilter = &filters[I_TREE_31DAYS];
	GetTime31Days(time, stime2);
	d1 = GetTimeStampDate(stime2);
	if (d >= d1 && d <= d2)
		pFilter->active++;
	pFilter = &filters[I_TREE_MONTH];
	GetTimeMonth(time, stime2);
	d1 = GetTimeStampDate(stime2);
	if (d >= d1 && d <= d2)
		pFilter->active++;
	pFilter = &filters[I_TREE_YEAR];
	GetTimeYear(stime, stime2);
	d1 = GetTimeStampDate(stime2);
	if (d >= d1 && d <= d2)
		pFilter->active++;
	pFilter = &filters[I_TREE_LASTYEAR];
	GetTimeLastYear(stime, stime1, stime2);
	d1 = GetTimeStampDate(stime1);
	d0 = GetTimeStampDate(stime2);
	if (d >= d0 && d <= d1)
		pFilter->active++;
	pFilter = &filters[I_TREE_OLDEST];
	GetTimeOldest(stime, stime1, stime2);
	d1 = GetTimeStampDate(stime1);
	d0 = GetTimeStampDate(stime2);
	if (d >= d0 && d <= d1)
		pFilter->active++;
	pFilter = &filters[I_TREE_FULL];
	pFilter->active++;

	pFilter = &filters[I_TREE_ROOT];
	pFilter->active++;

	d0 = MAKE_TIME_STAMP_DATE(pTa->time.wYear, pTa->time.wMonth, 0);

	hRoot = m_treeCtrl.GetRootItem();
	hSel = m_treeCtrl.GetSelectedItem();

	// Удаляем элемент из списка:
	m_listCtrl.DeleteItem(iItem);
	int count = m_listCtrl.GetItemCount();

	hDate = NULL;
	hItem = m_treeCtrl.GetNextItem(hRoot, TVGN_CHILD);
	while (hItem)
	{
		data = m_treeCtrl.GetItemData(hItem);
		if (! TREECTRL_ITEMDATA_IS_DATE(data))
		{
			pFilter = &filters[(int)data];
			if (pFilter->active)
			{
				UpdateItemTextEx(hItem, text, SIZEOF_ARRAY(text) - 1, -1, 0, val);
				int n = 0;

				if (I_TREE_2DAYS == (int)data)
					n = 1;

				if (val <= n) // ?? (<)
				{// Нужно удалить фильтр
					deleteFilter = TRUE;
					hFilter = hItem;
				}
			}
		}
		else
		{
			if (hDate == NULL)
				hDate = hItem;
		}
		if (deleteFilter)
		{// Удаляем фильтр
			m_treeCtrl.DeleteItem(hFilter);
			hItem = m_treeCtrl.GetNextItem(hRoot, TVGN_CHILD);
		}
		else
			hItem = m_treeCtrl.GetNextItem(hItem, TVGN_NEXT);
		deleteFilter = FALSE;
	} // while (hItem)

	// Теперь даты:
	int found = 0;
	DWORD date;
	hItem = hDate;
	while (hItem)
	{
		data = m_treeCtrl.GetItemData(hItem);
		if (TREECTRL_ITEMDATA_IS_DATE(data))
		{
			iYear = TREECTRL_ITEMDATA_TO_YEAR(data);
			year = IYEAR2YEAR(iYear);
			iMonth = TREECTRL_ITEMDATA_TO_MONTH(data);
			month = IMONTH2MONTH(iMonth);
			day = TREECTRL_ITEMDATA_TO_DAY(data);

			date = MAKE_TIME_STAMP_DATE(year, month, 0);
			if (d0 == date)
			{
				found = 1;
				hChild = m_treeCtrl.GetNextItem(hItem, TVGN_CHILD);
				if (hChild)
				{
					hNext = hChild;
					while (hNext)
					{
						data = m_treeCtrl.GetItemData(hNext);
						day = TREECTRL_ITEMDATA_TO_DAY(data);
						date = MAKE_TIME_STAMP_DATE(year, month, day);
						if (d == date)
						{
							UpdateItemTextEx(hNext, text, SIZEOF_ARRAY(text) - 1, -1, 0, val);
							if (val <= 0) // ?? (<)
							{// Нужно удалить фильтр
								deleteFilter = TRUE;
								hFilter = hNext;								
							}
							break;
						}
						hNext = m_treeCtrl.GetNextItem(hNext, TVGN_NEXT);
					} // while (hNext)					
				}				
				UpdateItemTextEx(hItem, text, SIZEOF_ARRAY(text) - 1, -1, 0, val);
				if (val <= 0) // ?? (<)
				{// Нужно удалить фильтр (со всем содержимым)
					if (deleteFilter)
						m_treeCtrl.DeleteItem(hFilter);
					else
						deleteFilter = TRUE;
					hFilter = hItem;
				}
			}
		}
		else
			break;
		if (deleteFilter)
		{// Удаляем фильтр
			m_treeCtrl.DeleteItem(hFilter);
			hItem = m_treeCtrl.GetNextItem(hRoot, TVGN_CHILD);
			if (hItem == NULL)
			{// Нет данных
				int index = I_TREE_NODATA;
				hItem = m_treeCtrl.InsertItem(TVIF_TEXT, GetTreeText(index), 0, 0, 0, 0, 0, hRoot, NULL);
				m_treeCtrl.SetItemData(hItem, (DWORD_PTR)index);
				m_listCtrl.EnableWindow(FALSE);
				SetNoData();
				break;
			}
		}
		else
			hItem = m_treeCtrl.GetNextItem(hItem, TVGN_NEXT);
		if (found)
			break;
		deleteFilter = FALSE;
	} // while (hItem)

	return S_OK;
}

int CDlgHistory::SelectByDate(const SYSTEMTIME & st)
{
	int result = -1;	
	
	if (HaveData())
	{// Обходим дерево в поисках искомой даты:
		HTREEITEM hRoot = m_treeCtrl.GetRootItem();
		HTREEITEM hItem = NULL;
		int iYear, iMonth;
		int year, month, day;

		DWORD d = MAKE_TIME_STAMP_DATE(st.wYear, st.wMonth, st.wDay);
		DWORD d0 = MAKE_TIME_STAMP_DATE(st.wYear, st.wMonth, 0);

		hItem = m_treeCtrl.GetNextItem(hRoot, TVGN_CHILD);
		while (hItem)
		{			
			int found = 0;
			HTREEITEM hChild = NULL;
			DWORD_PTR data = m_treeCtrl.GetItemData(hItem);
			if (TREECTRL_ITEMDATA_IS_DATE(data))
			{
				iYear = TREECTRL_ITEMDATA_TO_YEAR(data);
				year = IYEAR2YEAR(iYear);
				iMonth = TREECTRL_ITEMDATA_TO_MONTH(data);
				month = IMONTH2MONTH(iMonth);
				day = TREECTRL_ITEMDATA_TO_DAY(data);

				DWORD date = MAKE_TIME_STAMP_DATE(year, month, 0);
				if (d0 == date)
				{			
					found++;
					hChild = m_treeCtrl.GetNextItem(hItem, TVGN_CHILD);
					while (hChild)
					{
						data = m_treeCtrl.GetItemData(hChild);
						day = TREECTRL_ITEMDATA_TO_DAY(data);
						date = MAKE_TIME_STAMP_DATE(year, month, day);
						if (d == date)
						{// Нашли искомое:							
							break;
						}
						hChild = m_treeCtrl.GetNextItem(hChild, TVGN_NEXT);
					} // while (hChild)
				} // if (d0 == date)
			}
			if (found)
			{
				m_treeCtrl.Expand(hItem, TVE_EXPAND);
				if (hChild)
					m_treeCtrl.SelectItem(hChild);
				else
					m_treeCtrl.SelectItem(hItem);
				break;
			}
			else
				hItem = m_treeCtrl.GetNextItem(hItem, TVGN_NEXT);
			result = S_OK;
		} // while (hItem)		
	} // if (HaveData())

	return result;
}

void CDlgHistory::ShowTreeMenu()
{
	//if (HaveData())
	{
		// Отображаем всплывающее меню:
		CMenu root;
		root.LoadMenu(IDR_MENU_HISTORY_TREE);
		CMenu menu = root.GetSubMenu(0);

		UpdateMenuTree(menu);

		CPoint point;
		GetCursorPos(&point);
		menu.TrackPopupMenu(TPM_LEFTALIGN |TPM_RIGHTBUTTON, point.x, point.y, *this);
	}
}

void CDlgHistory::ShowListMenu()
{
	if (m_listCtrl.IsWindowEnabled())	
	{
		// Отображаем всплывающее меню:
		CMenu root;
		root.LoadMenu(IDR_MENU_HISTORY);
		CMenu menu = root.GetSubMenu(0);
		UpdateMenuList(menu);

		CPoint point;
		GetCursorPos(&point);
		menu.TrackPopupMenu(TPM_LEFTALIGN |TPM_RIGHTBUTTON, point.x, point.y, *this);
	}
}

void CDlgHistory::DoScroll()
{
	m_listCtrl.DoScroll();
}

void CDlgHistory::DoSort()
{
	m_listCtrl.DoSort();
}

void CDlgHistory::SetNoSort(int set)
{
	m_listCtrl.SetNoSort(set);
}

void CDlgHistory::SetNoScroll(int set)
{
	m_listCtrl.SetNoScroll(set);
}

void CDlgHistory::InitTreeCtrl()
{
#if USE_TREECTRL_IMAGELIST
	m_imgList.Create(16, 16, ILC_COLOR24|ILC_MASK, 2, 0);
	m_imgList.Add(theApp.LoadIcon(IDI_ACCEPT));
	m_imgList.Add(theApp.LoadIcon(IDI_ACTIVE));
	m_treeCtrl.SetImageList(&m_imgList, TVSIL_NORMAL);
#endif

	UpdateTreeCtrl();

	//m_treeCtrl.Expand(hRoot, TVE_EXPAND);
	m_treeCtrl.SetExtendedStyle(0, TVS_EX_AUTOHSCROLL|TVS_EX_DOUBLEBUFFER);
}

void CDlgHistory::InitListCtrl()
{
	int width;
	int format;

	int iCol = 0;
	for (int i = 0; i < NR_COLS_MAX; i++)
	{
		const Settings::Presentation::ListOfDeals::Column & column = theApp.m_settings.presentation.list.columns.item[i];
		if (column.index < 0)
			break;
		else if (! column.visible && (column.index != COL_INDEX && column.index != COL_DATE))
			continue;

		LPCTSTR name = m_listCtrl.ColumnIndexToName(column.index);
		if (name != NULL)
		{
			width = m_listCtrl.GetColumnWidth(column.index, name);
			format = column.format;
			m_listCtrl.InsertColumn(iCol, name, format, width);

			CHeaderCtrl header = m_listCtrl.GetHeader();
			HDITEM hdi;
			hdi.mask = HDI_LPARAM;
			hdi.lParam = static_cast<LPARAM>(column.index);
			header.SetItem(iCol, &hdi);
			++iCol;
		}
	} // for (iCol)

	m_listCtrl.SetExtendedListViewStyle(m_listCtrl.GetExtendedListViewStyle()|
		LVS_EX_FULLROWSELECT|0|LVS_EX_DOUBLEBUFFER|LVS_EX_BORDERSELECT|
		0  ); // LVS_EX_BORDERSELECT LVS_EX_GRIDLINES LVS_EX_FULLROWSELECT

	m_listCtrl.SetImageList(theApp.GetIconList(), LVSIL_SMALL);

	m_listCtrl.UpdateBkColor();
}

void CDlgHistory::Resize(int cx, int cy, int width)
{
	int x0, x, y;

	CRect rect, treeRect, listRect, infoRect, frameRect, dlgRect, dateTimePickerRect;
	this->GetWindowRect(&dlgRect);
	this->GetClientRect(&rect);	

	// Дерево:
	m_treeCtrl.GetWindowRect(&treeRect);
	if (width > 0)
		treeRect.right = treeRect.left + width;

	x0 = x = rect.left - 1; y = rect.top - 1;
	
	CWindow wndDateTimePicker = GetDlgItem(IDC_DATETIMEPICKER);
	wndDateTimePicker.GetWindowRect(&dateTimePickerRect);
#if 0
	m_treeCtrl.MoveWindow(x, y, treeRect.Width(), rect.Height() + 2, FALSE);
#else	
	m_treeCtrl.MoveWindow(x, y, treeRect.Width(), rect.Height() + 2 - dateTimePickerRect.Height() - 2, FALSE);
#endif
	m_treeCtrl.GetWindowRect(&treeRect);	

	// Выбор даты и времени:
	wndDateTimePicker.MoveWindow(x + 2, y + treeRect.Height(), treeRect.Width() - 1 - 1, dateTimePickerRect.Height(), FALSE);
	treeRect.bottom += dateTimePickerRect.Height();

	CSize delimiter;
	delimiter.cx = 3;
	delimiter.cy = treeRect.Height();

	// Область информации:
	CWindow wndInfo = GetDlgItem(IDC_EDIT1);
	wndInfo.GetWindowRect(&infoRect);
	x0 += treeRect.Width() - 1;
	x = x0 + delimiter.cx; y = rect.bottom - infoRect.Height() - 1;
	
	CWindow wndFrame = GetDlgItem(IDC_STATIC);
#if 0
	wndFrame.MoveWindow(x, y, rect.Width() - treeRect.Width() + 3, infoRect.Height() + 1, FALSE);
	wndFrame.GetWindowRect(&frameRect);
#else
	wndFrame.ShowWindow(SW_HIDE);
#endif	

	// Список:
	y = rect.top - 1;

	CWindow wndListFrame = GetDlgItem(IDC_LIST_FRAME);
#if 0
	wndListFrame.MoveWindow(x, y, rect.Width() - treeRect.Width() + 3, rect.Height() - infoRect.Height() + 2, FALSE);
#else
	wndListFrame.ShowWindow(SW_HIDE);
#endif
	m_listCtrl.GetWindowRect(&listRect);
	m_listCtrl.MoveWindow(x + 1, y, rect.Width() - treeRect.Width() + 1 - delimiter.cx, rect.Height() - infoRect.Height() + 0, FALSE);
	m_listCtrl.GetWindowRect(&listRect);
#if SHOW_STATIC_LOADING
	CRect rectLoading;	
	CWindow wndLoading = GetDlgItem(IDC_STATIC_LOADING);
	wndLoading.GetWindowRect(&rectLoading);
	wndLoading.MoveWindow(x + (listRect.Width() - rectLoading.Width()) / 2, y + (listRect.Height() - rectLoading.Height()) / 2,
		rectLoading.Width(), rectLoading.Height(), FALSE); 
#endif
	wndInfo.MoveWindow(x + 1, y + listRect.Height() + 0, listRect.Width() - 1, infoRect.Height(), FALSE);	

	m_delimiter.MoveWindow(x0 + 1, y - 1, delimiter.cx - 0, delimiter.cy + 1, FALSE);

	Invalidate();
}

void CDlgHistory::UpdateMenuSort(CMenu & menu)
{
	CMyTradeListCtrl::Sort sort;
	m_listCtrl.GetSort(sort);

	UINT flags = MF_BYPOSITION;

	TSTRING_SMALL(str);

	int count;
	int nrMenuItems = COL_LAST - COL_FIRST;
	int n = 0;

	CHeaderCtrl header = m_listCtrl.GetHeader();
	count = header.GetItemCount();
	for (int iCol = COL_FIRST; iCol < count; iCol++)
	{
		int index = m_listCtrl.ColumnToIndex(iCol);
		int iItem = index - COL_FIRST;
		menu.GetMenuString(iItem, str, SIZEOF_ARRAY(str), flags);
		UINT id = menu.GetMenuItemID(iItem);
		int iMenuItem = nrMenuItems + n;
		menu.InsertMenu(iMenuItem, flags, id, str);
		if (index == COL_INDEX)
			menu.EnableMenuItem(iMenuItem, MF_DISABLED|MF_GRAYED|flags);
		n++;
	} // for (i)

	// Теперь удаляем лишние элементы:
	for (int i = 0; i < nrMenuItems; i++)
		menu.DeleteMenu(0, flags);

	menu.CheckMenuRadioItem(0, n - 1, sort.iItem - COL_FIRST, flags);

	count = static_cast<int>(menu.GetMenuItemCount());
	int first = count - 2;
	menu.CheckMenuRadioItem(first, first + 1, first + sort.descending, flags);
	UINT enabled = (sort.iItem >= 0) ? MF_ENABLED : MF_DISABLED|MF_GRAYED;
	menu.EnableMenuItem(first, enabled|flags);
	menu.EnableMenuItem(first + 1, enabled|flags);
}

void CDlgHistory::UpdateMenuList(CMenu & menu)
{
	CMenu subMenu;
	subMenu.Attach (menu.GetSubMenu(0));
	UpdateMenuSort(subMenu);

	// Удаляем только выбранные элементы списка:
	if (0 == m_listCtrl.GetSelectedCount() || (theApp.GetSettings().trading.history.modify == FALSE))
	{
		for (int i = 0; i < 2; i++) { menu.DeleteMenu(menu.GetMenuItemCount() - 1, MF_BYPOSITION); }
	}
	else
	{
		LPCTSTR msg2 = TEXT("Удалить записи");
		LPCTSTR msg1 = TEXT("Удалить запись");
		LPCTSTR msg;
		msg = (CheckListSingleSelection() >= 2) ? msg2 : msg1;
		UINT id = ID_HISTORY_LIST_REMOVE_ITEM;
		menu.ModifyMenu(id, MF_BYCOMMAND|MF_STRING, id, msg);
	}
	subMenu.Detach();
}

void CDlgHistory::UpdateMenuTree(CMenu & menu)
{
	const BOOL modify = theApp.GetSettings().trading.history.modify;
	BOOL deleteExpand = TRUE;
	BOOL deleteCollapse = TRUE;
	HTREEITEM hItem = m_treeCtrl.GetSelectedItem();
	if (hItem)
	{
		if (NULL != m_treeCtrl.GetNextItem(hItem, TVGN_CHILD))
		{
			UINT state = m_treeCtrl.GetItemState(hItem, TVIS_EXPANDED|TVIS_EXPANDEDONCE);
			if (state & TVIS_EXPANDED)
				deleteCollapse = FALSE;
			else
				deleteExpand = FALSE;
		}
	}
	if (deleteExpand)
		menu.DeleteMenu(ID_HISTORY_TREE_EXPAND, MF_BYCOMMAND);
	if (deleteCollapse)
		menu.DeleteMenu(ID_HISTORY_TREE_COLLAPSE, MF_BYCOMMAND);
	if (deleteExpand && deleteCollapse)
		menu.DeleteMenu(0, MF_BYPOSITION); // удаляем разделитель;

	if (hItem == m_treeCtrl.GetRootItem())
	{
		if (HaveData() && modify == TRUE)
			menu.ModifyMenu(ID_HISTORY_TREE_REMOVE, MF_BYCOMMAND|MF_STRING, ID_HISTORY_TREE_REMOVE, TEXT("Очистить"));
		else
		{
			menu.DeleteMenu(ID_HISTORY_TREE_REMOVE, MF_BYCOMMAND);
			if (!(deleteExpand && deleteCollapse) || modify == FALSE)
				menu.DeleteMenu(menu.GetMenuItemCount() - 1, MF_BYPOSITION);
		}
	}
	else
	{
		int type = (int)m_treeCtrl.GetItemData(hItem);
		if (type != I_TREE_NODATA && modify == TRUE)
		{
			TSTRING_STD2(name, size);
			TSTRING_STD(text);
			m_treeCtrl.GetItemText(hItem, name, size);
			_stprintf_s(text, SIZEOF_ARRAY(text) - 1, TEXT("Удалить записи \"%s\""), (LPCTSTR)name);
			menu.ModifyMenu(ID_HISTORY_TREE_REMOVE, MF_BYCOMMAND|MF_STRING, ID_HISTORY_TREE_REMOVE, text);
		}
		else
		{
			menu.DeleteMenu(ID_HISTORY_TREE_REMOVE, MF_BYCOMMAND);
			if (modify == FALSE)
				menu.DeleteMenu(menu.GetMenuItemCount() - 1, MF_BYPOSITION);
		}
	}
}

void CDlgHistory::SetNoFilters()
{
	m_filters.mask = 0x0;
}

void CDlgHistory::SetFilter(int index, BOOL set)
{
	DWORD mask = (1 << index) & 0xffffffff;
	if (set)
		m_filters.mask |= mask;
	else
		m_filters.mask &= ~mask;
}

int CDlgHistory::CheckFilter(int index) const
{
	DWORD mask = (1 << index) & 0xffffffff;
	return (m_filters.mask & mask);
}

void CDlgHistory::UpdateBkColor()
{
	m_listCtrl.UpdateBkColor();
}

void CDlgHistory::UpdateColumns(const Settings::Presentation::ListOfDeals & settings, const Settings::Presentation::ListOfDeals & prev)
{
	Settings::Presentation::ListOfDeals current, previos;
	current = settings; previos = prev;

	Settings::Presentation::ListOfDeals * pSettingss[] = {&current, &previos};

	for (int i = 0; i < SIZEOF_ARRAY(pSettingss); i++)
	{
		Settings::Presentation::ListOfDeals * pSettings = pSettingss[i];
		for (int j = 0; j < SIZEOF_ARRAY(pSettings->columns.item); j++)
		{
			Settings::Presentation::ListOfDeals::Column & column = pSettings->columns.item[j];
			if (column.index == COL_INDEX || column.index == COL_DATE)
				column.visible = TRUE;
		} // for (j)
	} // for (i)

	m_listCtrl.UpdateColumns(current, previos);
}

int CDlgHistory::UpdateColumnIndex()
{
	return m_listCtrl.UpdateColumnIndex();
}

// CDlgHistory message handlers

void CDlgHistory::OnSize(UINT nType, CSize size)
{
	if (::IsWindow(m_hWnd) && ::IsWindow(m_treeCtrl.m_hWnd) && ::IsWindow(m_listCtrl.m_hWnd))
		this->Resize(size.cx, size.cy);
}

BOOL CDlgHistory::OnInitDialog(CWindow wndFocus, LPARAM lInitParam)
{
	DoDataExchange(false);
	
	::SetWindowTheme(m_treeCtrl, TEXT("explorer"), NULL);

	my::DialogMessageHook::InstallHook(*this);

	HICON hIcon = theApp.LoadSmallIcon(IDI_TIME_LIST);
	SetIcon(hIcon, FALSE);

	m_listCtrl.SetParentName(this->GetName());	

	CRect rect;
	m_treeCtrl.GetClientRect(&rect);
	m_treeCtrlCtrl.resizing = FALSE;

	Resize(0, 0, m_treeCtrlCtrl.width);

	InitTreeCtrl();
	InitListCtrl();	

	HCURSOR hCursor = ::LoadCursor(NULL, IDC_SIZEWE);
	::SetClassLong(m_delimiter.m_hWnd, GCL_HCURSOR, (LONG) hCursor);   

	m_initialized = TRUE;

	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX Property Pages should return FALSE
}

void CDlgHistory::OnCancel(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	::PostMessage(GetParent(), UM_CLOSE_WINDOW, this->IDD, 0);
}

LRESULT CDlgHistory::OnClose(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	if (lParam == 0)
		OnCancel(0, 0, NULL);
	else
		bHandled = TRUE;
	return 0;
}

void CDlgHistory::OnDestroy()
{
	m_listCtrl.SaveListCtrlParams();
	my::DialogMessageHook::UninstallHook(*this);	
}

void CDlgHistory::OnMouseMove(UINT nFlags, CPoint point)
{
#if 0
	CRect rect;
	m_treeCtrl.GetClientRect(&rect);
	if (point.x == rect.right)
	{
		if (! m_treeCtrlCtrl.resizing)
		{
			HCURSOR hCursor = LoadCursor(theApp.GetModuleInstance(), IDC_SIZEWE);
			SetCursor(hCursor); 
			m_treeCtrlCtrl.resizing = 1;
		}
	}
	else
	{
		if (m_treeCtrlCtrl.resizing)
		{
			HCURSOR hCursor = LoadCursor(theApp.GetModuleInstance(), IDC_ARROW);
			SetCursor(hCursor);
			m_treeCtrlCtrl.resizing = 0;
		}
	}
#else
#endif
}

void CDlgHistory::OnLButtonUp(UINT nFlags, CPoint point)
{
	m_treeCtrlCtrl.resizing = FALSE;

}

void CDlgHistory::OnSelectFilter(int index)
{
#if SHOW_STATIC_LOADING
	m_listCtrl.ShowWindow(SW_HIDE);	
	::UpdateWindow(GetDlgItem(IDC_LIST_FRAME));
	CWindow wndLoading = GetDlgItem(IDC_STATIC_LOADING);
	wndLoading.EnableWindow(FALSE);
	wndLoading.ShowWindow(SW_SHOW);
	wndLoading.UpdateWindow();
#else
	Set(MODE_INIT);
	UpdateDescription(TEXT("Обработка данных..."));
#endif
	m_listCtrl.DeleteAllItems();
	
	History * history = GetHistory();
	if (history)
	{
		Transaction * pTa;
		Transactions::Iterator it;
		pTa = history->FindFirstTransaction(it);
		if (pTa)
		{
			int found = 0;

			SYSTEMTIME stime, stime1, stime2;
			GetLocalTime(&stime);
			CTime time(stime);

			for (;;)
			{
				if (TREECTRL_ITEMDATA_IS_DATE(index))
				{// Месяц
					int year, month, day;
					int iYear = TREECTRL_ITEMDATA_TO_YEAR(index);
					int iMonth = TREECTRL_ITEMDATA_TO_MONTH(index);
					int iDay = TREECTRL_ITEMDATA_TO_DAY(index);
					year = IYEAR2YEAR(iYear);
					month = iMonth + 1;
					day = iDay;
					if (day)
					{
						SYSTEMTIME stime;
						stime.wDay = day; stime.wMonth = month; stime.wYear = year;
						pTa = history->FindTransactionByDate(stime, it);
					}
					else
					{
						stime1.wDay = 1; stime1.wMonth = month; stime1.wYear = year;
						stime2.wDay = 31; stime2.wMonth = month; stime2.wYear = year;
						pTa = history->FindTransactionByDate(stime1, stime2, it);
					}
				}
				else if (index == I_TREE_TODAY)
				{
					pTa = history->FindTransactionByDate(stime, it);
				}
				else if (index == I_TREE_YESTERDAY)
				{			
					SYSTEMTIME stimeYesterday;
					GetTimeYesterday(time, stimeYesterday);
					pTa = history->FindTransactionByDate(stimeYesterday, it);
				}
				else if (index == I_TREE_2DAYS)
				{
					SYSTEMTIME stime22;
					GetTimeYesterday(time, stime22);
					pTa = history->FindTransactionByDate(stime22, stime, it);
				}
				else if (index == I_TREE_7DAYS)
				{
					SYSTEMTIME stime7;
					GetTime7Days(time, stime7);
					pTa = history->FindTransactionByDate(stime7, stime, it);
				}
				else if (index == I_TREE_31DAYS)
				{
					SYSTEMTIME stime31;
					GetTime31Days(time, stime31);
					pTa = history->FindTransactionByDate(stime31, stime, it);
				}
				else if (index == I_TREE_MONTH)
				{
					SYSTEMTIME stimeMonth;
					GetTimeMonth(time, stimeMonth);
					pTa = history->FindTransactionByDate(stimeMonth, stime, it);
				}
				else if (index == I_TREE_YEAR)
				{
					SYSTEMTIME stimeYear;
					GetTimeYear(stime, stimeYear);
					pTa = history->FindTransactionByDate(stimeYear, stime, it);
				}
				else if (index == I_TREE_LASTYEAR)
				{// В прошлом году:
					GetTimeLastYear(stime, stime1, stime2);
					pTa = history->FindTransactionByDate(stime2, stime1, it);
				}
				else if (index == I_TREE_OLDEST)
				{// Самые старые записи:
					GetTimeOldest(stime, stime1, stime2);
					pTa = history->FindTransactionByDate(stime2, stime1, it);
				}
				else if (index == I_TREE_ROOT || index == I_TREE_FULL)
				{
					pTa = history->FindNextTransaction(it);
				}
				else
					break;
				
				if (pTa)
				{
					++found;
					m_listCtrl.AddTransaction(pTa, theApp.FindTableProperty(pTa), 0, 0);					
				}
				else
					break;
			} // for (;;)
		} // if (pTa)
	} // if (history)
	int count = m_listCtrl.GetItemCount();
	if (count > 0)
	{
		m_listCtrl.DoSort();
		m_listCtrl.DoScroll();		
		m_listCtrl.EnableWindow();
	}
	else
		m_listCtrl.EnableWindow(FALSE);

	// При необходимости обновляем текст фильтров (из-за бага фильтра "Вчера и сегодня"):
	HTREEITEM hItem = m_treeCtrl.GetSelectedItem();
	TSTRING_STD(text);
	UpdateItemTextEx(hItem, text, SIZEOF_ARRAY(text) - 1, 0, 1, count);

#if SHOW_STATIC_LOADING
	wndLoading.ShowWindow(SW_HIDE);
	m_listCtrl.UpdateWindow();
	m_listCtrl.Invalidate();
	m_listCtrl.ShowWindow(SW_SHOW);
#endif
	UpdateDescription();
	Set(MODE_NORMAL);
}

LRESULT CDlgHistory::OnTvnSelchangedTree1(NMHDR *pNMHDR)
{
	LPNMTREEVIEW pNMTreeView = reinterpret_cast<LPNMTREEVIEW>(pNMHDR);	

	if (m_mode != MODE_REMOVE)
	{
		int index = (int)m_treeCtrl.GetItemData(pNMTreeView->itemNew.hItem);
		OnSelectFilter(index);
	}
#if defined _DEBUG && 0
	else
	{
		_asm nop;
	}
#endif
	return 0;
}

LRESULT CDlgHistory::OnHdnItemclickList1(NMHDR *pNMHDR)
{
	LPNMHEADER phdr = reinterpret_cast<LPNMHEADER>(pNMHDR);

	m_listCtrl.DoSort();

	return 0;
}

void CDlgHistory::OnEnSetfocusEdit1(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	m_edit.HideCaret();
}

void CDlgHistory::UpdateDescription(int iItem)
{	
	TCHAR text[1024];
	if (iItem == -1)
		iItem = my::ctrl::GetFocusedItem(m_listCtrl);
	if (iItem >= 0)
	{
		Transaction * pTa = (Transaction*)m_listCtrl.GetItemData(iItem);
		if (pTa != NULL)
		{
			const QuoteTable::Properties * pProperties = theApp.FindTableProperty(pTa);
			int nd = (pProperties) ? pProperties->price.nd : 0;
			int n = MakeDescription(pTa, text, SIZEOF_ARRAY(text) - 1, nd);
		}
	}
	else
	{
		HTREEITEM hItem = m_treeCtrl.GetSelectedItem();
		if (hItem != NULL)
		{
#if 0
			int index = (int)m_treeCtrl.GetItemData(hItem);
			_stprintf_s(text, SIZEOF_ARRAY(text), TEXT("%s"), GetTreeText(index));
#else
			TSTRING_STD2(itemText, size);
			m_treeCtrl.GetItemText(hItem, itemText, size);
			_stprintf_s(text, SIZEOF_ARRAY(text), TEXT("%s"), itemText);
#endif
		}
	}
	m_edit.SetWindowText(text);
}

void CDlgHistory::UpdateDescription(LPCTSTR msg)
{
	m_edit.SetWindowText(msg);
}

LPCTSTR CDlgHistory::UpdateItemText(HTREEITEM hItem, LPTSTR text, int size, int inc)
{
	TVITEM tv;
	tv.hItem = hItem;
	tv.mask = TVIF_HANDLE|TVIF_TEXT;
	tv.pszText = text;
	tv.cchTextMax = size;
	if (TRUE == m_treeCtrl.GetItem(&tv))
	{
		LPTSTR pStr = StrRChr(text, NULL, TEXT('('));
		if (pStr != NULL)
		{
			++pStr;
			int offset = (int)(pStr - text);
			int n = StrToInt(pStr);
			n += inc;
			_stprintf_s(text + offset, size - offset, TEXT("%d)"), n);
			m_treeCtrl.SetItem(&tv);
		}
	}
	return text;
}

LPCTSTR CDlgHistory::UpdateItemTextEx(HTREEITEM hItem, LPTSTR text, int size, int inc, int sync, int & val)
{
	int n = -1;
	TVITEM tv;
	tv.hItem = hItem;
	tv.mask = TVIF_HANDLE|TVIF_TEXT;
	tv.pszText = text;
	tv.cchTextMax = size;
	if (TRUE == m_treeCtrl.GetItem(&tv))
	{
		LPTSTR pStr = StrRChr(text, NULL, TEXT('('));
		if (pStr != NULL)
		{
			++pStr;
			int offset = (int)(pStr - text);
			int v = StrToInt(pStr);
			if (sync)
				n = val;
			else
				n = v + inc;
			if (n != v)
			{
				_stprintf_s(text + offset, size - offset, TEXT("%d)"), n);
				m_treeCtrl.SetItem(&tv);
			}
		}
	}

	if (! sync)
		val = n;

	return text;
}

LRESULT CDlgHistory::OnLvnItemchangedList1(NMHDR *pNMHDR)
{
	LPNMLISTVIEW pNMLV = reinterpret_cast<LPNMLISTVIEW>(pNMHDR);
	if (m_mode == MODE_NORMAL)
	{
		if (pNMLV->uOldState == 0)
			UpdateDescription(pNMLV->iItem);
	}
	return 0;
}

int CDlgHistory::RemoveSelectedItems(int flags)
{
	int iItem;
	int deleted = 0;
	
	int iSelected = -1;
	int nrSelectedItems = 0;

	BOOL quickDelete = FALSE;
	BOOL deleteAll = FALSE;
	const int nrItemsToQuickDelete = 20;

	CWindow wndParent = GetParent();

	History * history = GetHistory();

	Set(MODE_REMOVE);

#if defined _DEBUG && 1
	TRACE("Remove selected items: begin (%d)\r\n", SetTicks1());
#endif
	if (flags & History::F_REMOVE_ALL)
	{// Удаляем всю историю
		deleteAll = TRUE;
		deleted = m_listCtrl.GetItemCount();
		quickDelete = FALSE;

		::SendMessage(wndParent, UM_REMOVE_TRANSACTION, (WPARAM)NULL, History::F_CLEAR);
	}
	else
	{
		// Ищем последний выделенный элемент:
		iItem = -1;
		for (;;)
		{
			iItem = m_listCtrl.GetNextItem(iItem, LVIS_SELECTED);
			if (iItem >= 0)
			{
				iSelected = iItem;
				++nrSelectedItems;
			}
			else
				break;
		} // for (;;)

		if (nrSelectedItems == 0)
			goto end;
		else
		{
			if (nrSelectedItems == m_listCtrl.GetItemCount())
				deleteAll = TRUE;
	#if 0
			if (nrSelectedItems <= nrItemsToQuickDelete)
	#endif
			{// Быстрое удаление:
	#if 0
				if (! (flags & History::F_REMOVE_BY_FILTER))
	#endif
				quickDelete = TRUE;
			}
		}
		
		// Удаление элементов истории:
		for (;;)
		{
			iItem = m_listCtrl.GetNextItem(-1, LVIS_SELECTED);
			if (iItem >= 0)
			{
				Transaction * pTa = (Transaction*)m_listCtrl.GetItemData(iItem);
				if (pTa != NULL)
				{				
					if (quickDelete)
						Remove(pTa, iItem);
					else
						m_listCtrl.DeleteItem(iItem);

					// Уведомляем главное окно об удалении транзакции:
					::SendMessage(wndParent, UM_REMOVE_TRANSACTION, (WPARAM)pTa, History::F_REMOVE);

					++deleted;
				}
			} // if (iItem >= 0)
			else
				break;
		} // for (;;)		
	}

	if (deleted)
	{
		m_listCtrl.Invalidate();
		// Теперь обновляем фильтры:
		if (! quickDelete)
		{			
			UpdateTreeCtrl();
		} // if (! quickDelete)

		::SendMessage(wndParent, UM_REMOVE_TRANSACTION, (WPARAM)NULL, History::F_UPDATE);

		if (deleteAll)
		{
			HTREEITEM hSel = m_treeCtrl.GetSelectedItem();
			if (hSel)
			{
				int index = (int)m_treeCtrl.GetItemData(hSel);
				OnSelectFilter(index);
			}
		}
		else
		{
			m_listCtrl.UpdateColumnIndex();

			int count = m_listCtrl.GetItemCount();
			if (count > 0)
			{
				if (iSelected >= 0)
				{				
					if (iSelected >= count)
						iSelected = count - 1;
				}
			}			
			m_listCtrl.SetItemState(iSelected, LVIS_SELECTED, LVIS_SELECTED);
			m_listCtrl.SetFocus();
		}
	} // if (deleted)

end:
#if defined _DEBUG && 1
	DWORD ticks2 = SetTicks2();
	TRACE("Remove selected items: end (%d, dt=%d)\r\n", ticks2, GetTicksDelta());
#endif
	Set(MODE_NORMAL);
	return deleted;
}

void CDlgHistory::SelectAll()
{
	int count = m_listCtrl.GetItemCount();
	if (count > 0)
	{
		for (int i = 0; i < count; i++)
		{
			m_listCtrl.SetItemState(i, LVIS_SELECTED, LVIS_SELECTED);
		}
	}
}

int CDlgHistory::CheckListSingleSelection() const
{	
	return static_cast<int>(m_listCtrl.GetSelectedCount());
}

void CDlgHistory::OnHistoryListRemoveItem(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	BOOL remove = TRUE;
	HWND hWndFocus = ::GetFocus();
	if (theApp.GetSettings().trading.history.confirmRemove)
	{		
		LPCTSTR msg;
		LPCTSTR msg2 = TEXT("Вы уверены, что хотите удалить выбранные записи?");
#if 1
		LPCTSTR msg1 = TEXT("Вы уверены, что хотите удалить выбранную запись?");
		msg = (CheckListSingleSelection() >= 2) ? msg2 : msg1;
#else
		CString str;
		if (1 == CheckListSingleSelection())
		{
			double N = -1;
			POSITION pos = m_listCtrl.GetFirstSelectedItemPosition();
			int iItem = m_listCtrl.GetNextSelectedItem(pos);
			if (iItem >= 0)
			{
				Transaction * pTa = (Transaction*)m_listCtrl.GetItemData(iItem);
				if (pTa)
					N = pTa->orderKey;
			}
			str.Format(TEXT("Вы уверены, что хотите удалить запись № %.0f?"), N);
			msg = (LPCTSTR)str;
		}
		else
			msg = msg2;
#endif
		if (IDYES != my::MessageBox(*this, msg, MB_ICONQUESTION|MB_YESNOCANCEL))
			remove = FALSE;
	}
	::SetFocus(hWndFocus);
	if (remove)
		RemoveSelectedItems();
}

LRESULT CDlgHistory::OnNMRClickList1(NMHDR *pNMHDR)
{
	LPNMITEMACTIVATE pNMItemActivate = reinterpret_cast<LPNMITEMACTIVATE>(pNMHDR);
	
	ShowListMenu();

	return 0;
}

LRESULT CDlgHistory::OnLvnKeydownList1(NMHDR *pNMHDR)
{
	LPNMLVKEYDOWN pLVKeyDow = reinterpret_cast<LPNMLVKEYDOWN>(pNMHDR);

	if (theApp.GetSettings().trading.history.modify == FALSE)
		return 0;

	if (pLVKeyDow->wVKey == VK_DELETE)
		OnHistoryListRemoveItem(0, 0, NULL);

	return 0;
}

LRESULT CDlgHistory::OnNMRClickTree1(NMHDR *pNMHDR)
{
	CPoint point(::GetMessagePos());
	ScreenToClient(&point);

	UINT flags;
	HTREEITEM hItem = m_treeCtrl.HitTest(point, &flags);
	if (hItem == NULL)
		return 0;
	m_treeCtrl.SelectItem(hItem);

	int index = (int)m_treeCtrl.GetItemData(m_treeCtrl.GetSelectedItem());
#if 0
	OnSelectFilter(index);
#else
	// Итак вызывается в соответствующем обработчике!
#endif
	
	ShowTreeMenu();

	return 0;
}

LRESULT CDlgHistory::OnNMClickTree1(NMHDR *pNMHDR)
{
	return 0;
}

void CDlgHistory::OnHistoryTreeRemove(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	BOOL remove = TRUE;
	int flags = History::F_REMOVE_BY_FILTER;
	if (theApp.GetSettings().trading.history.confirmRemove)
	{
		TSTRING_SMALL(text);
		LPCTSTR msg = TEXT("Вы уверены, что хотитете удалить записи?");
		HTREEITEM hRoot = m_treeCtrl.GetRootItem();
		HTREEITEM hItem = m_treeCtrl.GetSelectedItem();
		if (hItem)
		{
			if (hItem == hRoot)
			{
				msg = TEXT("Вы уверены, что хотитете очистить историю транзакций?");
				flags = History::F_REMOVE_ALL;
			}
			else
			{
				TSTRING_SMALL2(name, size);
				m_treeCtrl.GetItemText(hItem, name, size);
				_stprintf_s(text, SIZEOF_ARRAY(text) - 1, TEXT("Вы уверены, что хотитете удалить записи \"%s\"?"), (LPCTSTR)name);
				msg = text;
			}
		}
		if (IDYES != my::MessageBox(*this, msg, MB_ICONQUESTION|MB_YESNOCANCEL))
			remove = FALSE;
	}
	if (remove)
	{
		SelectAll();
		RemoveSelectedItems(flags);
	}
}

LRESULT CDlgHistory::OnTvnKeydownTree1(NMHDR *pNMHDR)
{
	LRESULT result = 0;
	LPNMTVKEYDOWN pTVKeyDown = reinterpret_cast<LPNMTVKEYDOWN>(pNMHDR);

	if (theApp.GetSettings().trading.history.modify == FALSE)
		return result;

	int vkey = pTVKeyDown->wVKey;
	if (vkey == VK_DELETE)
		OnHistoryTreeRemove(0, 0, NULL);

	return result;
}

LRESULT CDlgHistory::OnCmdMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = FALSE;
	int nCode = HIWORD(wParam);
	if (nCode == 0)
	{
		UINT nID = LOWORD(wParam);
		if (nID > ID_SORT_INDEX && nID <= ID_SORT_ACCOUNT)
		{
			int iItem = nID - ID_SORT_INDEX + COL_FIRST;
			m_listCtrl.DoSort(iItem, -1);
			bHandled = TRUE;
		}
		else if (nID == ID_SORT_ASCENDING || nID == ID_SORT_DESCENDING)
		{
			int descending = nID - ID_SORT_ASCENDING;
			m_listCtrl.DoSort(-1, descending);
			bHandled = TRUE;
		}
	}
	return 0;
}

void CDlgHistory::OnHistoryTreeExpand(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	HTREEITEM hItem = m_treeCtrl.GetSelectedItem();
	if (hItem)
		m_treeCtrl.Expand(hItem, TVE_EXPAND);
}

void CDlgHistory::OnHistoryTreeCollapse(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	HTREEITEM hItem = m_treeCtrl.GetSelectedItem();
	if (hItem)
		m_treeCtrl.Expand(hItem, TVE_COLLAPSE);
}

LRESULT CDlgHistory::OnWndMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = TRUE;
	switch (uMsg)
	{
	case UM_UPDATE:
		{
			HWND hWnd = reinterpret_cast<HWND>(wParam);
			int code = (int)lParam;
			
			{
#if 0
				if (code == WM_SIZING)
				{
					int width = m_delimiter.GetWidth();
					Resize(0, 0, width);
				}
#else
				switch (code)
				{
				case WM_SIZING:
					{
						if (m_treeCtrlCtrl.resizing)
						{
							int width;
							CPoint point;
							GetCursorPos(&point);
							ScreenToClient(&point);
							width = point.x;
							Resize(0, 0, width);
						}
					}
					break;
				case WM_LBUTTONDOWN:
					{
						if (hWnd == m_delimiter.m_hWnd)
							SetSizing();
					}
					break;
				case WM_LBUTTONUP:
						SetSizing(FALSE);
					break;
				}
#endif
			}
		}
		break;
	case UM_MENU:
		{
			int what = (int)wParam;
			if (what == F_SHOW_MENU)
			{
				CWindow focus = GetFocus();
				if (focus == m_treeCtrl)
					ShowTreeMenu();
				else if (focus == m_listCtrl)
					ShowListMenu();
			}
		}
		break;
	default:
		bHandled = FALSE;
	} // switch (uMsg)

	return 0;
}

void CDlgHistory::SetSizing(BOOL set)
{
	if (set)
	{
	}
	else
	{
		// Нужно сохранить ширину окна:	
		CRect rect;
		m_treeCtrl.GetWindowRect(&rect);
		m_treeCtrlCtrl.width = rect.Width();
		m_treeCtrlCtrl.modified = TRUE;
	}
	m_treeCtrlCtrl.resizing = set;
}

void CDlgHistory::SetWidthTreeCtrl(int width)
{
	m_treeCtrlCtrl.width = width;
}

int CDlgHistory::GetModifiedWidthTreeCtrl() const
{
	if (m_treeCtrlCtrl.modified)
		return m_treeCtrlCtrl.width;
	else
		return 0;
}

//DTN_DATETIMECHANGE
LRESULT CDlgHistory::OnDtnDatetimechangeDatetimepicker(NMHDR *pNMHDR)
{
	LPNMDATETIMECHANGE pDTChange = reinterpret_cast<LPNMDATETIMECHANGE>(pNMHDR);	
	// Поиск по дате:
	if (S_OK == SelectByDate(pDTChange->st))
	{
	}
	return 0;
}

void CDlgHistory::OnActivate(UINT nState, BOOL bMinimized, CWindow wndOther)
{
	::SendMessage(GetParent(), UM_ACTIVE, (WPARAM)this->m_hWnd, (LPARAM)nState);
}
