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
 *  DlgGlass.cpp : implementation file
 */

#include "stdafx.h"
#include "MyQuik.h"
#include "DlgGlass.h"
#include <color.h>
#include "strings.h"
#include "dlg.h"
#include "mcapture.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

#define USE_USER_COLORSNTEXT 0

//
// CDlgGlass dialog
//
CDlgGlass::CDlgGlass()
{
	m_hIcon = theApp.LoadIcon(IDR_MAINFRAME);

	m_bitmap = NULL;
	m_draw.flags = F_DRAW_FULL;

	HideMarker();
	ClearActiveItems();

	m_pTable = NULL;
}

CDlgGlass::~CDlgGlass()
{
	SAFE_DELETE(m_bitmap);
}

void CDlgGlass::HideMarker(BOOL hide, BOOL redraw)
{
	if (hide)
	{
		DrawCtrl::Item & marker = m_draw.items[I_ACTIVE_MARKER];
		marker.rect.SetRectEmpty();
		marker.point.x = -1;
		marker.point.y = -1;
	}
	if (redraw)
		Invalidate(FALSE);
}

void CDlgGlass::HideMenu()
{
//	HWND hWnd;
//#if 0
//	hWnd = *this;
//	SendMessage(hWnd, WM_CANCELMODE, 0, 0);
//	SendMessage(hWnd, WM_EXITMENULOOP, 1, 0);
//#else
//	hWnd = FindWindow(TEXT("MNU"), NULL);
//	if (hWnd)
//	{
//		SendMessage(hWnd, WM_DESTROY, 0x00, 0x00);
//		SendMessage(hWnd, WM_CANCELMODE, 0x00, 0x00);
//	}
//#endif

}

void CDlgGlass::ClearActiveItems()
{
	m_active.ClearItem(I_ACTIVE_MARKER);
	m_active.ClearItem(I_ACTIVE_CURSOR);
}

void CDlgGlass::SetTable(QuoteTable * pTable)
{
	m_pTable = pTable;
}

void CDlgGlass::SetDlgIcon(HINSTANCE hInst, BOOL freeze)
{
	UINT ids[] = {IDI_BUY_SELL_SMALL, IDI_BUY_SELL_SMALL_FRAME};
	HICON hIcon = CMyQuikApp::LoadIconEx(hInst, ids[freeze], 16);
	SetIcon(hIcon, FALSE);
	HICON hIconBig = theApp.LoadIcon(IDR_MAINFRAME);
	SetIcon(hIconBig, TRUE);
}

void CDlgGlass::SetFreeze(BOOL freeze)
{
	SetDlgIcon(theApp.GetModuleInstance(), freeze);
}

void CDlgGlass::SetMaster(BOOL master)
{
	m_master = master;
}

BOOL CDlgGlass::IsMaster() const 
{
	return m_master; 
}

BOOL CDlgGlass::IsMarkerVisible() const
{
	const DrawCtrl::Item & marker = m_draw.items[I_ACTIVE_MARKER];
	return (marker.rect.IsRectEmpty() == FALSE);
}

QuoteTable * CDlgGlass::GetTable()
{
	return (m_user.pTable != NULL) ? m_user.pTable : this->GetCurrentTable();
}

QuoteTable * CDlgGlass::GetCurrentTable()
{
	return m_pTable;
}

void CDlgGlass::SetQuantity(double quantity) 
{
	if (quantity == 0)
	{
		const QuoteTable * pTable = this->GetCurrentTable();
		if (pTable != NULL)
			quantity = pTable->GetProperties()->trading.quantity;
	}
	this->quantity = quantity;
}

void CDlgGlass::UpdateName(LPCTSTR name)
{	
	if (name != NULL)
		m_name = name;

	TSTRING_SMALL2(str, size);

	if (m_name.GetLength() > 0)
	{
		int n = _stprintf_s(str, size, TEXT("%s"), m_name);
		if ((m_user.pTable))
			_stprintf_s(str + n, size - n, TEXT(" - активные заявки"));
		else
		{
			const QuoteTable * pTable = this->GetCurrentTable();
			if (pTable)
				_stprintf_s(str + n, size - n, TEXT(" - %.0f"), this->quantity); // pTable->GetProperties()->trading.quantity
		}
	}
	else
		_stprintf_s(str, size, TEXT("%s"), TEXT("Стакан заявок"));

	this->SetWindowText(str);
}

void CDlgGlass::Update(LPCTSTR name, double price)
{
	UpdateName(name);
	int flags = F_DRAW_FULL;
	if (price)
	{
		TRACE("Update by price %f\r\n", price);
		flags |= F_DRAW_MARKER_BY_PRICE;
		m_draw.items[I_ACTIVE_MARKER].price = price;
	}
	Redraw(flags);
}

void CDlgGlass::Redraw(int flags)
{
#if _DEBUG && 0
	TRACE(__FUNCTION__" (0x%x)\r\n", flags);
#endif
	m_draw.flags = flags;
	if (flags & F_DRAW_NOW)
	{		
		this->Paint();
		m_draw.flags = F_DRAW_NULL;
	}
#if 0
	else
#endif
		Invalidate(FALSE);
}

void CDlgGlass::Empty()
{
	m_name.Empty();
	UpdateName(NULL);
	SetUserTrade(0, NULL, FALSE);
	Redraw();
}

void CDlgGlass::SetUserTrade(int set, QuoteTable * pTable, BOOL updateName)
{
	if (set == -1)
	{
		Update(NULL);		
	}
	else
	{
		m_user.pTable = pTable;
		m_user.trade = set;
#if 0
		if (set)
			HideMarker();
#endif
		if (updateName)
			UpdateName(m_name);
	}	
}

BOOL CDlgGlass::IsUserTrade() const
{
	return (m_user.trade != 0 && m_user.pTable != NULL);
}

void CDlgGlass::ModifyMenuViewGlass(CMenu & menu)
{
	UINT flags = MF_BYCOMMAND;
	UINT mfEnable;
	UINT id;

	CMenu subMenu;
	subMenu.Attach (menu.GetSubMenu(0));
	subMenu.CheckMenuRadioItem(GLASS_STYLE_1, GLASS_STYLE_3, m_settings.view.style, MF_BYPOSITION);
	subMenu.Detach();

	menu.CheckMenuItem(ID_GLASS_BUY_ABOVE, ((m_settings.view.flags & F_GLASS_VIEW_BUY_UP) ? MF_CHECKED : MF_UNCHECKED)|flags);
	menu.CheckMenuItem(ID_GLASS_FLIP_HORIZONTAL, (m_settings.view.flipHorizontal ? MF_CHECKED : MF_UNCHECKED)|flags);
	menu.CheckMenuItem(ID_GLASS_RAREFY, ((m_settings.view.flags & F_GLASS_VIEW_RAREFIED) ? MF_CHECKED : MF_UNCHECKED)|flags);
	menu.CheckMenuItem(ID_GLASS_NEUTRAL_ZONE, ((m_settings.view.flags & F_GLASS_VIEW_NEUTRAL_ZONE) ? MF_CHECKED : MF_UNCHECKED)|flags);

	// Линии сетки:
	const Settings::Presentation::Glass::Grid & grid = m_settings.grid;
	subMenu.Attach (menu.GetSubMenu(menu.GetMenuItemCount() - 1 - 1 - 3));
	mfEnable = grid.show ? MF_ENABLED : MF_DISABLED|MF_GRAYED;
	id = ID_GLASS_GRID_LINES_VERTICAL;
	subMenu.CheckMenuItem(id, ((grid.lines & GRID_LINES_VERTICAL) ? MF_CHECKED : MF_UNCHECKED)|flags);
	subMenu.EnableMenuItem(id, mfEnable);
	id = ID_GLASS_GRID_LINES_HORIZONTAL;
	subMenu.CheckMenuItem(id, ((grid.lines & GRID_LINES_HORIZONTAL) ? MF_CHECKED : MF_UNCHECKED)|flags);
	subMenu.EnableMenuItem(id, mfEnable);
	subMenu.Detach();

	// Граница заявок:
	const Settings::Presentation::Glass::Border & border = m_settings.border;
	subMenu.Attach (menu.GetSubMenu(menu.GetMenuItemCount() - 1 - 1 - 2));
	subMenu.CheckMenuItem(ID_GLASS_BORDER, (border.show ? MF_CHECKED : MF_UNCHECKED)|flags);
	mfEnable = border.show ? MF_ENABLED : MF_DISABLED|MF_GRAYED;
	id = ID_GLASS_BORDER_LINES_BORDERS;
	subMenu.CheckMenuItem(id, ((border.lines & BORDER_LINES_BORDERS) ? MF_CHECKED : MF_UNCHECKED)|flags|mfEnable);
	subMenu.EnableMenuItem(id, mfEnable);
	id = ID_GLASS_BORDER_LINES_MEDIAN;
	subMenu.CheckMenuItem(id, ((border.lines & BORDER_LINES_MEDIAN) ? MF_CHECKED : MF_UNCHECKED)|flags|mfEnable);	
	subMenu.EnableMenuItem(id, mfEnable);
	subMenu.Detach();

	// Маркер:
	const Settings::Presentation::Glass::Marker & marker = m_settings.marker;
	subMenu.Attach (menu.GetSubMenu(menu.GetMenuItemCount() - 1 - 1 - 1));
	subMenu.CheckMenuItem(ID_GLASS_MARKER, (marker.show ? MF_CHECKED : MF_UNCHECKED)|flags);
	mfEnable = marker.show ? MF_ENABLED : MF_DISABLED|MF_GRAYED;
	id = ID_GLASS_MARKER_FOLLOW_PRICE;
	subMenu.CheckMenuItem(id, ((marker.follow & MARKER_FOLLOW_PRICE) ? MF_CHECKED : MF_UNCHECKED)|flags|mfEnable);
	subMenu.EnableMenuItem(id, mfEnable);
	id = ID_GLASS_MARKER_FOLLOW_CURSOR;
	subMenu.CheckMenuItem(id, ((marker.follow & MARKER_FOLLOW_CURSOR) ? MF_CHECKED : MF_UNCHECKED)|flags|mfEnable);	
	subMenu.EnableMenuItem(id, mfEnable);
	id = ID_GLASS_MARKER_FOLLOW_MENU;
	subMenu.CheckMenuItem(id, ((marker.follow & MARKER_FOLLOW_MENU) ? MF_CHECKED : MF_UNCHECKED)|flags|mfEnable);	
	subMenu.EnableMenuItem(id, mfEnable);
	subMenu.Detach();

	// Свои заявки:
	subMenu.Attach (menu.GetSubMenu(menu.GetMenuItemCount() - 1));
	subMenu.CheckMenuItem(ID_USER_SHOW_ACTIVE_BIDS, ((m_settings.user.flags & F_USER_SHOW_ACTIVE_BIDS) ? MF_CHECKED : MF_UNCHECKED)|flags);
	subMenu.CheckMenuItem(ID_USER_SHOW_ACTIVE_STOPS, ((m_settings.user.flags & F_USER_SHOW_ACTIVE_STOPS) ? MF_CHECKED : MF_UNCHECKED)|flags);
	subMenu.CheckMenuItem(ID_USER_SHOW_ACTIVE_DEALS, ((m_settings.user.flags & F_USER_SHOW_ACTIVE_DEALS) ? MF_CHECKED : MF_UNCHECKED)|flags);
	subMenu.CheckMenuItem(ID_USER_SHOW_ICONS, ((m_settings.user.flags & F_USER_SHOW_ICONS) ? MF_CHECKED : MF_UNCHECKED)|flags);
	subMenu.CheckMenuItem(ID_GLASS_USER_SEPARATE_COLUMN, ((m_settings.user.flags & F_USER_SHOW_USER_COLUMN) ? MF_CHECKED : MF_UNCHECKED)|flags);
	subMenu.CheckMenuItem(ID_GLASS_USER_GROUP_PRICE, ((m_settings.user.flags & F_USER_GROUP_BY_PRICE) ? MF_CHECKED : MF_UNCHECKED)|flags);
	subMenu.Detach();
}

void CDlgGlass::SetFocus(BOOL focus)
{
	if (focus == TRUE)
	{
		if (*this != ::GetFocus())
			::SetFocus(*this);
	}
}

void CDlgGlass::SelectItem(CPoint * pPoint, int flags)
{
	HideMenu();

	CPoint point;
	if (pPoint == NULL)
	{
		pPoint = &point;
		::GetCursorPos(pPoint);
		ScreenToClient(pPoint);
	}
	m_draw.items[I_ACTIVE_MARKER].point = *pPoint;
	m_active.items[I_ACTIVE_MARKER].flags = 0;
	Redraw(F_DRAW_MARKER|F_DRAW_MARKER_BY_POINT|flags);
}

void CDlgGlass::CheckActiveItem(CPoint * pPoint, int flags)
{
	CPoint point;
	if (pPoint == NULL)
	{
		pPoint = &point;
		::GetCursorPos(pPoint);
		ScreenToClient(pPoint);
	}
	m_draw.items[I_ACTIVE_CURSOR].point = *pPoint;
	m_active.items[I_ACTIVE_CURSOR].flags = 0;
	Redraw(F_GET_ACTIVE_ITEM|flags);
}

// CDlgGlass message handlers

BOOL CDlgGlass::OnInitDialog(CWindow wndFocus, LPARAM lInitParam)
{
#if 0
	my::DialogMessageHook::InstallHook(*this);
#endif
	HINSTANCE hInst = theApp.GetModuleInstance();
	int icons[] = {
		IDI_ACTIVE_BUY_SMALL,
		IDI_ACTIVE_SELL_SMALL,
		IDI_STOP_BUY_SMALL,
		IDI_STOP_SELL_SMALL,
		IDI_BUY_SMALL,
		IDI_SELL_SMALL,
	};
	for (int i = 0; i < SIZEOF_ARRAY(m_icons.hIcon); i++)
		m_icons.hIcon[i] = CMyQuikApp::LoadIconEx(hInst, icons[i], 16);

	SetDlgIcon(hInst, FALSE);

	Resize();
	//UpdateWindow();
#if USE_TRACK_MOUSE_EVENT
	TRACKMOUSEEVENT tme;
	tme.cbSize = sizeof(tme);
	tme.dwFlags = TME_HOVER|TME_LEAVE|TME_NONCLIENT;
	tme.hwndTrack = *this;
	tme.dwHoverTime = HOVER_DEFAULT;
	::TrackMouseEvent(&tme);
#endif
	return TRUE;  // return TRUE  unless you set the focus to a control
}

void CDlgGlass::Paint()
{
	CPaintDC dc(*this);

	CRect rect;
	this->GetClientRect(rect);

	BOOL drawNull = (m_draw.flags == F_DRAW_NULL) ? TRUE : FALSE;

	if (IsIconic())
	{
		SendMessage(WM_ICONERASEBKGND, reinterpret_cast<WPARAM>(dc.m_hDC), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{			
#if USE_GDIPLUS
		CDialog::OnPaint();

		Gdiplus::Graphics graphics(this->m_hWnd);

		Gdiplus::Bitmap backBuffer(rect.right, rect.bottom, &graphics);
		Gdiplus::Graphics _graphics(&backBuffer);
		_graphics.SetSmoothingMode(Gdiplus::SmoothingModeHighQuality);

		// Прорисовка содержимого в буфере:
		//m_rScp.Draw(_graphics, theApp.GetOptions().view, al::SystemControlPanel::Draw::F_REDRAW_ALL);

		Gdiplus::Color color(0, 100, 0);
		Gdiplus::SolidBrush solidBrush(color);
		_graphics.FillRectangle(&solidBrush, rect.left, rect.top, rect.Width(), rect.Height());		

		// Вывод на экран содержимого временного буфера:
		graphics.DrawImage(&backBuffer, 0, 0, 0, 0, rect.right, rect.bottom, Gdiplus::UnitPixel);
#else
		CDC _dc;
		_dc.CreateCompatibleDC(dc);
		_dc.SelectBitmap(*m_bitmap);

		const Settings::Presentation::Glass & settings = m_settings;
		int marker = FALSE;
	
		Draw(&_dc, rect, settings, marker);

		dc.SetStretchBltMode(COLORONCOLOR);
		dc.BitBlt(rect.left, rect.top, rect.Width(), rect.Height(), _dc, rect.left, rect.top, SRCCOPY);

		DrawCtrl::Item & drawItemMarker = m_draw.items[I_ACTIVE_MARKER];
		DrawCtrl::Item & drawItemCursor = m_draw.items[I_ACTIVE_CURSOR];

		if (!marker && (m_draw.flags & F_DRAW_MARKER))
		{
			HideMarker(TRUE, FALSE);
			if (m_draw.flags & F_DRAW_MARKER_BY_POINT)
				drawItemMarker.price = 0;
		}
#if 1
		double price = drawItemMarker.price;
		QuoteTable * pTable = theApp.GetCurrentTable();
		if (pTable)
		{
			if (pTable->priceUser != price)
				pTable->SetPriceUser(price);
		}
#endif
		if (! drawItemMarker.rect.IsRectEmpty())
		{			
			if (settings.marker.show && settings.marker.width)
			{
				int width = settings.marker.width;
				CRect rect = drawItemMarker.rect;
#if 0
				CRect _rect(0, 0, rect.Width(), rect.Height());

				COLORREF color = m_draw.marker.color;
				color = my::lib::MakeColorLighter(color, 20);
				CBrush brush(color);			

				CDC _dc;
				_dc.CreateCompatibleDC(pDC);

				CBitmap bitmap;
				bitmap.CreateCompatibleBitmap(pDC, _rect.Width(), _rect.Height());
				_dc.SelectObject(&bitmap);

				_dc.FillRect(&_rect, &brush);

				BLENDFUNCTION blend = {AC_SRC_OVER, 0, 100, 0};
				dc.AlphaBlend(rect.left, rect.top, rect.Width(), rect.Height(), &_dc, _rect.left, _rect.top, _rect.Width(), _rect.Height(), blend);
#else
				CPen pen;
				COLORREF markerColor = settings.marker.color;
				if (*this != GetFocus())
					markerColor = my::lib::MakeColorLighterEx(markerColor, 50, 240, 1);
				pen.CreatePen(PS_SOLID, width, markerColor);
				dc.SelectPen(pen);
				dc.SelectStockBrush(NULL_BRUSH);

				if (width % 2 == 0)
				{
					rect.right += 1;
					rect.bottom += 1;
				}
				else
				{
					rect.top -= 1;
					rect.bottom += 1;
				}
				dc.Rectangle(&rect);

				//dc.SetStretchBltMode(COLORONCOLOR);
				//dc.BitBlt(rect.left, rect.top, rect.Width(), rect.Height(), &_dc, _rect.left, _rect.top, SRCCOPY);
#endif
				if (m_draw.flags & F_DRAW_MARKER)
					m_draw.flags &= ~(F_DRAW_MARKER);
			}
		}
#endif
	}
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CDlgGlass::OnPaint(CDCHandle )
{
	this->Paint();
}

void CDlgGlass::Draw(CDC * pDC, CRect & rect, const Settings::Presentation::Glass & settings, int & drawMarker)
{
#if defined _DEBUG && 1
	TRACE("Draw glass: begin (%d)\r\n", SetTicks1());
#endif
	const Settings::GlassItem & common = settings.items[GLASS_ITEM_COMMON];
	const Settings::GlassItem & giBuy = settings.items[GLASS_ITEM_BUY];
	const Settings::GlassItem & giSell = settings.items[GLASS_ITEM_SELL];
	const Settings::GlassItem & giBuyPrice = settings.items[GLASS_ITEM_BUY_PRICE];
	const Settings::GlassItem & giBuyVolume = settings.items[GLASS_ITEM_BUY_VOLUME];
	const Settings::GlassItem & giBuyUser = settings.items[GLASS_ITEM_BUY_USER];
	const Settings::GlassItem & giSellPrice = settings.items[GLASS_ITEM_SELL_PRICE];
	const Settings::GlassItem & giSellVolume = settings.items[GLASS_ITEM_SELL_VOLUME];
	const Settings::GlassItem & giSellUser = settings.items[GLASS_ITEM_SELL_USER];
	const Settings::Presentation::Glass::Columns & columns = settings.columns;
	const Settings::Presentation::Glass::Grid & grid = settings.grid;
	const Settings::Presentation::Glass::Margins & margins = settings.margins;
	const Settings::Presentation::Glass::Border & border = settings.border;
	const Settings::Presentation::Glass::Marker & marker = settings.marker;

	COLORREF colorBackground = settings.backgrnd.color;

	const QuoteTable * pTable = this->GetTable();
	if (pTable == NULL)
	{
		// Рисуем подложку и выходим:
		CBrush b;
		b.CreateSolidBrush(colorBackground);
		pDC->FillRect(rect, b);
		return;
	}	

	const int style = settings.view.style;	
	const BOOL buyUp = settings.view.flags & F_GLASS_VIEW_BUY_UP;
	const BOOL flipHorizontal = settings.view.flipHorizontal;
	const BOOL neutralZone = settings.view.flags & F_GLASS_VIEW_NEUTRAL_ZONE;

	int style3 = (style == GLASS_STYLE_3);

	BOOL showGrid = (grid.show && grid.width && grid.lines);
	BOOL showGridRows = showGrid && ((grid.lines & GRID_LINES_HORIZONTAL) ? TRUE : FALSE);
	BOOL showGridCols = showGrid && ((grid.lines & GRID_LINES_VERTICAL) ? TRUE : FALSE);
	BOOL showBorder = border.show && border.width;

	const BOOL useUserColumn = settings.user.flags & F_USER_SHOW_USER_COLUMN;

	int marginLeft, marginRight, marginTop, marginBottom;
	marginLeft = marginRight = marginTop = marginBottom = 0;
	if (margins.enable)
		Settings::Presentation::Glass::GetMargins(margins.value, marginTop, marginBottom, marginLeft, marginRight);

	int cxUserPerc, cxPricePerc, cxVolumePerc;
	Settings::Presentation::Glass::GetColumnWidth(columns.width, cxPricePerc, cxVolumePerc, cxUserPerc);

	COLORREF color = common.text.cf.crTextColor;
	COLORREF colorBuy = giBuy.backgrnd.color;
	COLORREF colorSell = giSell.backgrnd.color;
	COLORREF colorFill = common.backgrnd.color;
	COLORREF colorTop, colorBottom;

	if (! giBuy.backgrnd.enable)
		colorBuy = colorFill;
	if (! giSell.backgrnd.enable)
		colorSell = colorFill;
	
	const QuoteTable::Array * pArTop, * pArBottom;
	const QuoteTable::Array * pArrayBuy = pTable->GetArray(QuoteTable::I_BUY);
	const QuoteTable::Array * pArraySell = pTable->GetArray(QuoteTable::I_SELL);
	const QuoteTable::Array * pArrayGen = pTable->GetArray(QuoteTable::I_NEUTRAL);

	const Settings::GlassItem * pgiTop, * pgiBottom;
	const Settings::GlassItem * pgiPriceTop, * pgiPriceBottom;
	const Settings::GlassItem * pgiVolumeTop, * pgiVolumeBottom;
	const Settings::GlassItem * pgiUserTop, * pgiUserBottom;

	DWORD alignmentTop, alignmentBottom;

	int typeTop, typeBottom;

	if (buyUp)
	{// Покупки сверху, продажи снизу
		typeTop = DrawInfo::Area::TYPE_BUY; typeBottom = DrawInfo::Area::TYPE_SELL;

		pgiTop = &giBuy; pgiBottom = &giSell;
		pgiPriceTop = &giBuyPrice; pgiPriceBottom = &giSellPrice; 
		pgiVolumeTop = &giBuyVolume; pgiVolumeBottom = &giSellVolume; 
		pgiUserTop = &giBuyUser; pgiUserBottom = &giSellUser; 

		colorTop = colorBuy; colorBottom = colorSell;

		pArTop = pArrayBuy; pArBottom = pArraySell;

		alignmentTop = giBuy.u.alignment.enable ? giBuy.u.alignment.value : 0; 
		alignmentBottom = giSell.u.alignment.enable ? giSell.u.alignment.value : 0;
	}
	else
	{// Покупки внизу, продажи вверху
		typeTop = DrawInfo::Area::TYPE_SELL; typeBottom = DrawInfo::Area::TYPE_BUY;

		pgiTop = &giSell; pgiBottom = &giBuy;
		pgiPriceTop = &giSellPrice; pgiPriceBottom = &giBuyPrice; 
		pgiVolumeTop = &giSellVolume; pgiVolumeBottom = &giBuyVolume; 
		pgiUserTop = &giSellUser; pgiUserBottom = &giBuyUser; 

		colorTop = colorSell; colorBottom = colorBuy;

		pArTop = pArraySell; pArBottom = pArrayBuy;

		alignmentTop = giSell.u.alignment.enable ? giSell.u.alignment.value : 0; 
		alignmentBottom = giBuy.u.alignment.enable ? giBuy.u.alignment.value : 0;
	}

	BOOL showNeutralZone = neutralZone && pArrayGen->count;
	BOOL showBorderMedian = showBorder && (border.lines & BORDER_LINES_MEDIAN);
	BOOL showNeutralZoneBorder = showBorder && showNeutralZone && (border.lines & BORDER_LINES_BORDERS);

	int n0, n1;
	n0 = n1 = 0;

	if (showNeutralZone)
	{
		int n = pArrayGen->count;		
		if ((pArTop->count == 0 && pArBottom->count == 0) || (pArTop->count && pArBottom->count))
		{
			n0 = n / 2;
		}
		else
		{
			if (pArTop->count == 0)
				n1 = n;
			else
				n0 = n;
		}
		n1 = n - n0;

		showBorderMedian = showBorder && (border.lines & BORDER_LINES_MEDIAN) && ((style3 && (n0 || n1)) || (!style3 && (n0 && n1)));
	}
	else
	{
		showBorderMedian = showBorder && (pArTop->count || pArBottom->count);
	}

	// Определяем середину:
	int width, height;
	int width_2= rect.Width() / 2; 
	int height_2 = rect.Height() / 2;
	int x1Middle, y1Middle, x2Middle, y2Middle;

	int y, y0, y1;
	int d0, d1;
	int dy0;

	int gridWidth = grid.width;
#if 1
	gridWidth = 1;
#endif
	int borderWidth = 0;
	if (showBorder)
		borderWidth = border.width;
	d0 = d1 = 0;
	if (showBorder)
	{
		d0 = d1 = border.width / 2;
		if (border.width && ((border.width % 2) == 0))
			d1 -= 1;			
	}

	CRect rectRow;
	CRect rectTop, rectBottom;

	if (style3)
	{
		width = width_2; height = rect.Height();
		rectTop.SetRect(rect.left, rect.top, rect.left + width_2, rect.bottom);
		if (showBorderMedian)
			rectTop.right -= d0;
		rectBottom.SetRect(rect.right - width_2, rect.top, rect.right, rect.bottom);
		if (showBorderMedian)
			rectBottom.left += d1 + 1;
		else
			rectBottom.left = rectTop.right;
		x1Middle = rect.left + width_2; y1Middle = rect.top;
		x2Middle = x1Middle; y2Middle = rect.bottom;
	}
	else
	{
		width = rect.Width(); height = height_2;
		rectTop.SetRect(rect.left, rect.top, rect.right, rect.top + height_2);
		if (showBorderMedian)
			rectTop.bottom -= d0;
		rectBottom.SetRect(rect.left, rect.top + height_2, rect.right, rect.bottom);
		if (showBorderMedian)
			rectBottom.top += d1;
		else
			rectBottom.top = rectTop.bottom;
		x1Middle = rect.left; y1Middle = rect.top + height_2;
		x2Middle = rect.right; y2Middle = y1Middle;
	}

	DrawInfo glass;
	DrawInfo::Area & area0 = glass.area[0];
	DrawInfo::Area & area1 = glass.area[1];

	area0.i = 0; area1.i = 1;
	area0.type = typeTop; area1.type = typeBottom;
	area0.rect = area0.rectW = rectTop; area1.rect = area1.rectW = rectBottom;

	area0.color = colorTop; area1.color = colorBottom;
	area0.alignment = alignmentTop; area1.alignment = alignmentBottom;

	area0.pgi = pgiTop; area1.pgi = pgiBottom;

	//
	// Определяем размеры и положение столбцов:
	//
	double pricePerc = (double)cxPricePerc; 
	double volumePerc = (double)cxVolumePerc; 
	double userPerc = (double)cxUserPerc;
	int cxPrice, cxVolume, cxUser;
	int xPrice, xVolume, xUser;
	int xNull, cxNull;
	int dx;

	if (! useUserColumn)
	{
		pricePerc += userPerc / 2;
		volumePerc += userPerc / 2;	
		userPerc = 0;
	}
	if (style == GLASS_STYLE_2)
	{
		double percents = pricePerc + 2*volumePerc;
		if (useUserColumn)
			percents += 2*userPerc;

		double k = 100 / percents;
		pricePerc *= k; volumePerc *= k; userPerc *= k;
	}
	cxPrice = (int)((double)width * pricePerc / 100);
	cxVolume = (int)((double)width * volumePerc / 100);
#if 0
	cxUser = useUserColumn ? area0.rect.Width() - cxPrice - cxVolume : 0;
#else
	cxUser = useUserColumn ? (int)((double)width * userPerc / 100) : 0;
#endif

	int w0 = area0.rect.Width();	
	int w = cxPrice + 2*(cxVolume + cxUser);
	if (style == GLASS_STYLE_2)
	{		
		if (w != w0)
		{
			dx = (int)(0.0 + (double)(w0 - w) / 2);
			if (useUserColumn)
				cxUser += dx;
			else
				cxVolume += dx;
		}
	}
	else
	{
		if (cxPrice + cxVolume + cxUser != w0)
		{
			if (useUserColumn)
				cxUser = w0 - (cxPrice + cxVolume);
			else
				cxVolume = w0 - cxPrice;
		}
	}

	DrawInfo::Item * pDrawItem;
	if (style == GLASS_STYLE_1)
	{
		if (flipHorizontal)
		{
			xUser = rect.left; xVolume = xUser + cxUser; xPrice = xVolume + cxVolume;
		}
		else
		{
			xPrice = rect.left; xVolume = xPrice + cxPrice; xUser = xVolume + cxVolume;
		}
		pDrawItem = &area0.items[I_COLUMN_NULL];
		pDrawItem->rect.SetRectEmpty();
		pDrawItem = &area0.items[I_COLUMN_PRICE];
		pDrawItem->rect = rectRow; pDrawItem->rect.left = xPrice; pDrawItem->rect.right = pDrawItem->rect.left + cxPrice;
		pDrawItem = &area0.items[I_COLUMN_VOLUME];
		pDrawItem->rect = rectRow; pDrawItem->rect.left = xVolume; pDrawItem->rect.right = pDrawItem->rect.left + cxVolume;
		pDrawItem = &area0.items[I_COLUMN_USER];
		pDrawItem->rect = rectRow; pDrawItem->rect.left = xUser; pDrawItem->rect.right = pDrawItem->rect.left + cxUser;

		for (int i = 0; i < SIZEOF_ARRAY(area1.items); i++)
			area1.items[i] = area0.items[i]; 
	}
	else if (style == GLASS_STYLE_2)
	{		
		dx = cxVolume;
		if (useUserColumn)
			dx += cxUser;
		for (int i = 0; i < SIZEOF_ARRAY(glass.area); i++)
		{
			DrawInfo::Area & area = glass.area[i];
			int cxVol = cxVolume;
			int cxUsr = cxUser;
			int flip = flipHorizontal ^ i;
			if (flip)
			{
				xUser = rect.left; xVolume = xUser + cxUsr; xPrice = xVolume + cxVol; 
				xNull = xPrice + cxPrice; cxNull = area.rect.right - xNull;
				area.rect.left = xUser;
			}
			else
			{
				if (w != w0)
				{
					if (useUserColumn)
						cxUsr += 1;
					else
						cxVol += 1;
				}				
				xPrice = rect.left + dx; xVolume = xPrice + cxPrice; xUser = xVolume + cxVol;
				xNull = area.rectW.left; cxNull = xPrice - xNull;
				area.rect.left = xPrice;
			}
			area.rect.right = area.rect.left + rect.Width() - dx;
			pDrawItem = &area.items[I_COLUMN_NULL];
			pDrawItem->rect = rectRow; pDrawItem->rect.left = xNull; pDrawItem->rect.right = pDrawItem->rect.left + cxNull;
			pDrawItem = &area.items[I_COLUMN_PRICE];
			pDrawItem->rect = rectRow; pDrawItem->rect.left = xPrice; pDrawItem->rect.right = pDrawItem->rect.left + cxPrice;
			pDrawItem = &area.items[I_COLUMN_VOLUME];
			pDrawItem->rect = rectRow; pDrawItem->rect.left = xVolume; pDrawItem->rect.right = pDrawItem->rect.left + cxVol;
			pDrawItem = &area.items[I_COLUMN_USER];
			pDrawItem->rect = rectRow; pDrawItem->rect.left = xUser; pDrawItem->rect.right = pDrawItem->rect.left + cxUsr;		
		} // for (i)
	}
	else if (style3)
	{		
		w = cxPrice + cxVolume + cxUser;
		for (int i = 0; i < SIZEOF_ARRAY(glass.area); i++)
		{
			DrawInfo::Area & area = glass.area[i];
			area.rect.OffsetRect(0, -1);
			int cxVol = cxVolume;
			int cxUsr = cxUser;
			int flip = flipHorizontal ^ i;
			if (flip)
			{
				w0 = area.rect.Width();
				if (w != w0)
				{
					if (useUserColumn)
						cxUsr += w0 - w;
					else
						cxVol += w0 - w;
				}
				xPrice = area.rect.left; xVolume = xPrice + cxPrice; xUser = xVolume + cxVol;
			}
			else
			{
				xUser = area.rect.left; xVolume = xUser + cxUsr; xPrice = xVolume + cxVol;
			}
			pDrawItem = &area.items[I_COLUMN_NULL];
			pDrawItem->rect.SetRectEmpty();
			pDrawItem = &area.items[I_COLUMN_PRICE];
			pDrawItem->rect = rectRow; pDrawItem->rect.left = xPrice; pDrawItem->rect.right = pDrawItem->rect.left + cxPrice;
			pDrawItem = &area.items[I_COLUMN_VOLUME];
			pDrawItem->rect = rectRow; pDrawItem->rect.left = xVolume; pDrawItem->rect.right = pDrawItem->rect.left + cxVol;
			pDrawItem = &area.items[I_COLUMN_USER];
			pDrawItem->rect = rectRow; pDrawItem->rect.left = xUser; pDrawItem->rect.right = pDrawItem->rect.left + cxUsr;
		} // for (i)
	}

	CPen penGrid;
	penGrid.CreatePen(PS_SOLID, gridWidth, grid.color);

	LOGBRUSH lb;
	lb.lbStyle = BS_SOLID;
	lb.lbHatch = 0;
	lb.lbColor = border.color;

	CPen penBorder;
	penBorder.CreatePen(PS_SOLID|PS_GEOMETRIC|PS_ENDCAP_FLAT, border.width, &lb);

#if 0
	else
	{// Загружаем нарисованную поверхность (?)
	}
#endif
	pDC->SetBkMode(TRANSPARENT);
	pDC->SetTextColor(color);

	int & drawFlags = m_draw.flags;
	if (drawFlags & F_DRAW_FULL)
		drawFlags |= F_DRAW_ALL;
#if 1
	if (drawFlags != F_DRAW_NULL)
		if (!(drawFlags & F_DRAW_MARKER_BY_POINT) && (m_settings.marker.follow & MARKER_FOLLOW_PRICE) && m_draw.items[I_ACTIVE_MARKER].price)
			drawFlags |= F_DRAW_MARKER_BY_PRICE;
#endif

	CBrush brushFill;
	if (drawFlags & F_DRAW_BACKGROUND)
	{		
		brushFill.CreateSolidBrush(colorFill);
		pDC->FillRect(&rect, brushFill);
	}

	CFont font, fontTop, fontBottom;
	this->CreateFont(pDC, &font, &common);
	this->CreateFont(pDC, &fontTop, pgiTop); this->CreateFont(pDC, &fontBottom, pgiBottom);

	CBrush brushTop, brushBottom;

	int borderWidth_1 = border.width - 1;
	int dyBorder;
	int y0Border, y1Border;

	if (style3)
	{
		y0 = rectTop.top;
		dy0 = +1;
		dyBorder = d0;
	}
	else
	{
		y0 = rectTop.bottom;
		dy0 = -1;
		dyBorder = -d1;
		borderWidth_1 = -borderWidth_1;
	}
	y1 = rectBottom.top;

#if 1
	if (drawFlags & F_DRAW_BACKGROUND)
	{
		if (pgiTop->backgrnd.enable)
			brushTop.CreateSolidBrush (colorTop);
		if (pgiBottom->backgrnd.enable)
			brushBottom.CreateSolidBrush (colorBottom);
	}
#endif
	BOOL showHorizontals = showGridRows | (showBorderMedian && !style3) | showNeutralZoneBorder;

	if (showNeutralZone)
	{
		const Settings::GlassItem & giNeutral = settings.items[GLASS_ITEM_NEUTRAL_ZONE];
		const Settings::GlassItem & giNeutralPrice = settings.items[GLASS_ITEM_NEUTRAL_PRICE];
		const Settings::GlassItem & giNeutralVolume = settings.items[GLASS_ITEM_NEUTRAL_VOLUME];
		const Settings::GlassItem & giNeutralUser = settings.items[GLASS_ITEM_NEUTRAL_USER];

		CFont * pFontTop, * pFontBottom;
		CBrush brush0, brush1;
		COLORREF color0, color1;
		COLORREF txtColor0, txtColor1;

		if (pgiTop->text.enable)
		{
			pFontTop = &fontTop;
			txtColor0 = pgiTop->text.cf.crTextColor;
		}
		else
		{
			pFontTop = &font;
			txtColor0 = color;
		}		

		if (pgiBottom->text.enable)
		{
			pFontBottom = &fontBottom;
			txtColor1 = pgiBottom->text.cf.crTextColor;
		}
		else
		{
			pFontBottom = &font;
			txtColor1 = color;
		}
		color0 = (pgiTop->backgrnd.enable) ? pgiTop->backgrnd.color : colorFill;
		color1 = (pgiBottom->backgrnd.enable) ? pgiBottom->backgrnd.color : colorFill;

		if (drawFlags & F_DRAW_BACKGROUND)
		{
			CRect rect0(rectTop);
			if (style3)
				rect0.top = y0;
			else
				rect0.bottom = y0;
			CRect rect1(rectBottom);
			rect1.top = y1;

			if (pgiTop->backgrnd.enable)
				pDC->FillRect (&rect0, brushTop);
			if (pgiBottom->backgrnd.enable)
				pDC->FillRect (&rect1, brushBottom);

			if (giNeutral.backgrnd.enable)
			{
				const double opacity = (double)giNeutral.backgrnd.opacity / 100;
				brush0.CreateSolidBrush(my::lib::SumColorsAlpha(color0, giNeutral.backgrnd.color, opacity));
				brush1.CreateSolidBrush(my::lib::SumColorsAlpha(color1, giNeutral.backgrnd.color, opacity));
				pDC->FillRect (&rect0, (giNeutral.backgrnd.val1) ? brush0 : brushFill);
				pDC->FillRect (&rect1, (giNeutral.backgrnd.val1) ? brush1 : brushFill);
			}
		}
#if 1
		if (style3)
		{
			y1 -= 1;
			y0 = y1;
		}
		else
		{
			if (!showHorizontals || (!showBorderMedian && !showGridRows))
			{
				if (n0 + n1 > 1)
					y1 -= 1;
			}
		}
#endif
		if (buyUp)
		{
			if (n0) 
			{
				pDC->SetTextColor(txtColor0);
				DrawArea(pDC, rect, settings, &giNeutral, &giNeutralPrice, &giNeutralVolume, &giNeutralUser,  
					area0, area0.type|DrawInfo::Area::TYPE_NEUTRAL, 
					*pFontTop, brush0, y0, dy0, pTable, pArrayGen, n0 - 1, -1, n0, drawMarker);
			}
			if (n1)
			{
				pDC->SetTextColor(txtColor1);
				DrawArea(pDC, rect, settings, &giNeutral, &giNeutralPrice, &giNeutralVolume, &giNeutralUser, 
					area1, area1.type|DrawInfo::Area::TYPE_NEUTRAL, 
					*pFontBottom, brush1, y1, 1, pTable, pArrayGen, n0, 1, n1, drawMarker);
			}
		}
		else
		{
			if (n1)
			{
				pDC->SetTextColor(txtColor0);
				DrawArea(pDC, rect, settings, &giNeutral, &giNeutralPrice, &giNeutralVolume, &giNeutralUser, 
					area0, area0.type|DrawInfo::Area::TYPE_NEUTRAL,
					*pFontTop, brush0, y0, dy0, pTable, pArrayGen, n0, 1, n1, drawMarker);
			}
			if (n0)
			{
				pDC->SetTextColor(txtColor1);
				DrawArea(pDC, rect, settings, &giNeutral, &giNeutralPrice, &giNeutralVolume, &giNeutralUser, 
					area1, area1.type|DrawInfo::Area::TYPE_NEUTRAL, 
					*pFontBottom, brush1, y1, 1, pTable, pArrayGen, n0 - 1, -1, n0, drawMarker);
			}
		}
#if 1
		if (!showHorizontals || (!showNeutralZoneBorder && showNeutralZone && !showGridRows))
		{
			if (! style3)
				y0 += 1;
		}
#endif
	}

	pDC->SetTextColor(color);
	
	// Отображаем верхнюю часть:
	if (pArTop->count)
	{
		y = y0;
		if (showNeutralZoneBorder && pArrayGen->count)
		{
			if (border.width)
				y += borderWidth_1;
		}
		if (drawFlags & F_DRAW_BACKGROUND)
		{
			if (pgiTop->backgrnd.enable)
			{
				CRect rect(rectTop);
				if (style3)
					rect.top = y;
				else
					rect.bottom = y;
				pDC->FillRect (&rect, (pgiTop->backgrnd.val1) ? brushTop : brushFill);
			}
		}
#if 1
		if (!showHorizontals || (!showNeutralZoneBorder && !showGridRows))
		{
			if (style3)
				y -= 1;
		}
#endif
		DrawArea(pDC, rect, settings, pgiTop, pgiPriceTop, pgiVolumeTop, pgiUserTop, 
			area0, area0.type, font, brushTop, y, dy0, pTable, pArTop, 0, +1, pArTop->count, drawMarker);

		y0Border = y0 + dyBorder;
		y0 = y;
	}
	
	// Отображаем нижнюю часть:
	if (pArBottom->count)
	{
		y = y1;
		if (showNeutralZoneBorder && pArrayGen->count)
		{
			if (border.width)
				y += border.width - 1;
		}
		if (drawFlags & F_DRAW_BACKGROUND)
		{
			if (pgiBottom->backgrnd.enable)
			{
				CRect rect(rectBottom);
				rect.top = y;
				pDC->FillRect (&rect, (pgiBottom->backgrnd.val1) ? brushBottom : brushFill);
			}
		}
#if 1
		if (!showHorizontals || (!showNeutralZoneBorder && showNeutralZone && !showGridRows))
		{
			if (!style3 || (pArrayGen->count > 1 && n1 != n0))
				y -= 1;
		}
#endif
		DrawArea(pDC, rect, settings, pgiBottom, pgiPriceBottom, pgiVolumeBottom, pgiUserBottom, 
			area1, area1.type, font, brushBottom, y, 1, pTable, pArBottom, 0, +1, pArBottom->count, drawMarker);

		y1Border = y1 + d0;
		y1 = y;
	}

	// Сетка: вертикальные линии
	int dy = (showGridRows) ? gridWidth : 0;
	int ys[SIZEOF_ARRAY(glass.area)][3] = {
		{area0.rect.bottom, area0.rect.top - dy, y0 - dy}, 
		{area1.rect.top, area1.rect.bottom + dy, y1 + dy}, 
	};
	if (style == GLASS_STYLE_3)
		for (int i = 0; i < 3; i++)
			ys[0][i] = ys[1][i];

	if ((drawFlags & F_DRAW_GRID) && showGridCols)
	{
		for (int iArea = 0; iArea < SIZEOF_ARRAY(glass.area); iArea++)
		{
			DrawInfo::Area & area = glass.area[iArea];
			
			int xs[] = {
				area.items[I_COLUMN_PRICE].rect.left, 
				area.items[I_COLUMN_PRICE].rect.right, 
				area.items[I_COLUMN_VOLUME].rect.right, 
				area.items[I_COLUMN_USER].rect.right,				
			};
			int count = 2;
			int iFirst = 1;
			if (useUserColumn)
				count += 1;
			if (style == GLASS_STYLE_2)
			{
				iFirst = 0;
				count += 1;
			}
			else
			{
				count += 1;
			}
						
			int yBegin = ys[iArea][0];
			int yEnd = ys[iArea][1];
#if 0
			if (area.pgi->backgrnd.val1 == 0)
#endif
				yEnd = ys[iArea][2];

			pDC->SelectPen(penGrid);
			for (int i = iFirst; i < count; i++)
			{
				int x = xs[i];
				pDC->MoveTo(x, yBegin);
				pDC->LineTo(x, yEnd);
			} // for (i)
		} // for (iArea)
	}
		
	// Границы областей:
	if ((drawFlags & F_DRAW_BORDER) && showBorder)
	{
		if (showBorderMedian)
		{
			pDC->SelectPen(penBorder);			
			pDC->MoveTo(x1Middle, y1Middle);
			pDC->LineTo(x2Middle, y2Middle);
		}
		if (showNeutralZoneBorder && pArrayGen->count)
		{
			pDC->SelectPen(penBorder);
			pDC->MoveTo(area0.rectW.left, y0Border);
			pDC->LineTo(area0.rectW.right, y0Border);
			pDC->MoveTo(area1.rectW.left, y1Border);
			pDC->LineTo(area1.rectW.right, y1Border);
		}
	}
#if 1
	Active::Item & active = m_active.items[I_ACTIVE_CURSOR];
	if ((drawFlags & F_GET_ACTIVE_ITEM) && !(active.flags & F_ACTIVE_ITEM_IS_VALID))
	{
		CPoint point = m_draw.items[I_ACTIVE_CURSOR].point;
		if (rectTop.PtInRect(point))
		{
			if (buyUp)
				active.flags |= area0.type;
			else
				active.flags |= area1.type;
			active.flags |= F_ACTIVE_ITEM_IS_VALID;
		}
		else if (rectBottom.PtInRect(point))
		{
			if (buyUp)
				active.flags |= area1.type;
			else
				active.flags |= area0.type;
			active.flags |= F_ACTIVE_ITEM_IS_VALID;
		}
	}
#endif
#if defined _DEBUG && 1
	DWORD ticks2 = SetTicks2();
	TRACE("Draw glass: end (%d, dt=%d)\r\n", ticks2, GetTicksDelta());
#endif
}

void CDlgGlass::DrawArea(CDC * pDC, 
	CRect & rect, 
	const Settings::Presentation::Glass & settings, 
	const Settings::GlassItem * pgi, 
	const Settings::GlassItem * pgiPrice, const Settings::GlassItem * pgiVolume, const Settings::GlassItem * pgiUser, 
	DrawInfo::Area & area, 
	int type, 
	CFont & font0, CBrush & brush, 
	int & y, int dy, 
	const QuoteTable * pTable, 
	const QuoteTable::Array * pArray,
	int iFirst, int inc, int count, 
	int & drawMarker)
{
	const Settings::GlassItem & common = settings.items[GLASS_ITEM_COMMON];
	const Settings::Presentation::Glass::Columns & columns = settings.columns;
	const Settings::Presentation::Glass::Grid & grid = settings.grid;
	const Settings::Presentation::Glass::Margins & margins = settings.margins;
	const Settings::Presentation::Glass::Marker & marker = settings.marker;

	const QuoteTable::Properties * pProperties = pTable->GetProperties();

	TCHAR format[] = TEXT("%0.xf");
	TSTRING_STD(text);
	int n, nd;

	nd = pProperties->price.nd;
	format[3] = (nd % 10) + 0x30;

	const int style = settings.view.style;
	const BOOL rarefied = settings.view.flags & F_GLASS_VIEW_RAREFIED;

	const BOOL showGrid = (grid.show && grid.width && grid.lines);
	const BOOL showGridRows = showGrid && ((grid.lines & GRID_LINES_HORIZONTAL) ? TRUE : FALSE);
	const BOOL showGridCols = showGrid && ((grid.lines & GRID_LINES_VERTICAL) ? TRUE : FALSE);
	const BOOL showIcons = settings.user.flags & F_USER_SHOW_ICONS;

	const BOOL useUserColumn = settings.user.flags & F_USER_SHOW_USER_COLUMN;

	const BOOL showVolumeIndicators = pgiVolume->other.enable;

	double volumeMin, volumeMax;
	volumeMin = volumeMax = 0;
	CBrush brushInd;
	if (showVolumeIndicators)
	{		
		pTable->GetVolumeMinMax(volumeMin, volumeMax);
		brushInd.CreateSolidBrush((COLORREF)pgiVolume->other.val1);
	}

	int marginLeft, marginRight, marginTop, marginBottom;
	marginLeft = marginRight = marginTop = marginBottom = 0;
	if (margins.enable)
		Settings::Presentation::Glass::GetMargins(margins.value, marginTop, marginBottom, marginLeft, marginRight);

	int alignments[3];
	Settings::Presentation::Glass::GetColumnAlignment(area.alignment, alignments[0], alignments[1], alignments[2]);

	COLORREF color = common.text.cf.crTextColor;
	COLORREF colorFill = common.backgrnd.color;

	const QuoteTable::Array::Item * pItem;

	const Settings::GlassItem * pgiCol;

	double prices[] = {
		-1, -1, 
		-1, -1, 
		-1, -1,
		pTable->priceMax, pTable->priceMin, 
		pTable->priceBid, pTable->priceOffer, 
		pTable->price, 
	};

	const Settings::GlassItem * pgis[] = {
		&settings.items[GLASS_ITEM_USER_BIDS_BUY], &settings.items[GLASS_ITEM_USER_BIDS_SELL], 
		&settings.items[GLASS_ITEM_USER_STOPS_BUY], &settings.items[GLASS_ITEM_USER_STOPS_SELL], 
		&settings.items[GLASS_ITEM_USER_DEALS_BUY], &settings.items[GLASS_ITEM_USER_DEALS_SELL], 
		&settings.items[GLASS_ITEM_PRICE_MAX], &settings.items[GLASS_ITEM_PRICE_MIN],
		&settings.items[GLASS_ITEM_PRICE_BID], &settings.items[GLASS_ITEM_PRICE_OFFER],
		&settings.items[GLASS_ITEM_PRICE_LAST], 		
	};

	int cx, cy;
	int yTop, yBottom = 0;
	int yText;
	CPoint point;
	int xIcon, yIcon;
	const int cxIcon = 9; 
	const int cyIcon = 9;

	// Шрифт:
	CFont font, fontPrice, fontVolume, fontUser;
	CFont * pFont, * pFont2;

	CBrush brushPrice, brushVolume, brushUser;
	CBrush * pBrush2;

	CFont fontPrev;
	COLORREF colorPrev;

	COLORREF colorText = pDC->GetTextColor();

	pFont = &font0;

	if (pgi->text.enable)
	{
		pFont = &font;
		this->CreateFont(pDC, pFont, pgi);
		pDC->SetTextColor(pgi->text.cf.crTextColor);
	}

	if (pgiPrice->text.enable)
		this->CreateFont(pDC, &fontPrice, pgiPrice);
	if (pgiVolume->text.enable)
		this->CreateFont(pDC, &fontVolume, pgiVolume);
	if (pgiUser->text.enable)
		this->CreateFont(pDC, &fontUser, pgiUser);

	if (pgiPrice->backgrnd.enable)
		brushPrice.CreateSolidBrush(pgiPrice->backgrnd.color);
	if (pgiVolume->backgrnd.enable)
		brushVolume.CreateSolidBrush(pgiVolume->backgrnd.color);
	if (pgiUser->backgrnd.enable)
		brushUser.CreateSolidBrush(pgiUser->backgrnd.color);

	pDC->SelectFont(*pFont);

	CSize size;
	text[0] = TEXT('t');
	pDC->GetTextExtent(text, 1, &size);
	cx = size.cx; cy = size.cy;

	int gridWidth = grid.width;
#if 1
	gridWidth = 1;
#endif
	int cyRow = marginTop + size.cy + marginBottom + 0;
#if 1
	if (! showGridRows)
		cyRow -= gridWidth;
#endif
	CPen penGrid;
	penGrid.CreatePen(PS_SOLID, gridWidth, grid.color);

	int & drawFlags = m_draw.flags;

	int iRow, iCol;
	int iItem = iFirst;

	const int iPrice = QuoteTable::ITEM_PRICE;
	const int iVolume = QuoteTable::ITEM_VOLUME;

	DrawCtrl::Item & cursor = m_draw.items[I_ACTIVE_CURSOR];

	if (style == GLASS_STYLE_3)
		dy = +1;

	if (dy < 0)
	{
		y -= (cyRow);
	}
#if 1
	else
	{
		if (! showGridRows)
			y += gridWidth;
	}
#endif
	int iArea = area.i;

	BOOL fullArea = !pgi->backgrnd.enable || pgi->backgrnd.val1;

	CRect * pRect = &area.rect;
#if 1
	if (fullArea)
		pRect = &area.rectW;
#endif

	yTop = y;	

	for (iRow = 0; iRow < count; iRow++, iItem += inc)
	{			
		pItem = pArray->GetItem(iItem);

		double userVolumes[] = {
			pItem->values[QuoteTable::ITEM_USER_BID_BUY], pItem->values[QuoteTable::ITEM_USER_BID_SELL],
			pItem->values[QuoteTable::ITEM_USER_STOP_BUY], pItem->values[QuoteTable::ITEM_USER_STOP_SELL],
			pItem->values[QuoteTable::ITEM_USER_DEAL_BUY], pItem->values[QuoteTable::ITEM_USER_DEAL_SELL],
		};

		if (! (settings.user.flags & F_USER_SHOW_ACTIVE_BIDS))
			userVolumes[0] = userVolumes[0 + 1] = 0;
		if (! (settings.user.flags & F_USER_SHOW_ACTIVE_STOPS))
			userVolumes[2] = userVolumes[2 + 1] = 0;
		if (! (settings.user.flags & F_USER_SHOW_ACTIVE_DEALS))
			userVolumes[4] = userVolumes[4 + 1] = 0;

		int nrUser = 0;
		int iCellUser = -1;

		double price = pItem->values[iPrice];
		double volume = pItem->values[iVolume];
			
		if (volume == 0)
		{
			if (!rarefied && !(type & DrawInfo::Area::TYPE_NEUTRAL))
				continue;
		}

		if (drawFlags & F_DRAW_BACKGROUND)
		{
			if (! fullArea)
			{
				CRect rect(area.rect.left, y, area.rect.right, y + cyRow);
				pDC->FillRect (&rect, brush);
			}
		}

		point.y = y;
		yText = point.y + marginTop;
#if 1
		if (! showGridRows)
			yText -= gridWidth;
#endif
		for (iCol = 0; iCol < SIZEOF_ARRAY(area.items); iCol++)
		{
			CRect cell = area.items[iCol].rect;
#if 1
			if (cell.Width() == 0)
				continue;
#endif
			cell.top = y;
			cell.bottom = cell.top + cyRow;

			n = 0;
			int val;

			BOOL changeFont = FALSE;
				
			pgiCol = NULL;

			colorPrev = pDC->GetTextColor();

			if (iCol == I_COLUMN_PRICE)
			{
				n = _stprintf_s(text, SIZEOF_ARRAY(text), format, price);
				val = (int)price;

				pgiCol = pgiPrice; 
				pFont2 = &fontPrice; pBrush2 = &brushPrice;
			}
			else if (iCol == I_COLUMN_VOLUME)
			{
#if 1
				if (volume == 0)
					text[0] = TEXT('\0');
				else
#endif
					n = _stprintf_s(text, SIZEOF_ARRAY(text), TEXT("%d"), (int)volume);
				val = (int)volume;

				pgiCol = pgiVolume; 
				pFont2 = &fontVolume; pBrush2 = &brushVolume;
			}
			else if (iCol == I_COLUMN_USER)
			{
				pgiCol = pgiUser; 
				pFont2 = &fontUser; pBrush2 = (pgiUser->backgrnd.enable) ? &brushUser : &brush;
			}

			if (n && common.text.val1 && val >= 1000)
				n = my::str::GroupDigits(text, n, text, SIZEOF_ARRAY(text));

			if (drawFlags & F_DRAW_BACKGROUND)
			{					
				if (pgiCol)
				{
					if (pgiCol->text.enable)
					{
						pDC->SelectFont(*pFont2);
						colorPrev = pDC->SetTextColor(pgiCol->text.cf.crTextColor);
						changeFont = TRUE;
					}
					if (pgiCol->backgrnd.enable)
					{						
						pDC->FillRect (&cell, *pBrush2);
					}
				}
			}
					
			COLORREF colorCell = pDC->GetPixel(cell.left, cell.top + 1);
			const Settings::GlassItem * pgiCell = NULL;
			for (int i = 0; i < SIZEOF_ARRAY(prices); i++)
			{			
				CFont fontCell;
					
				BOOL setFont = FALSE;
				BOOL fillCell = FALSE;
				int customize = 0;
						
				pgiCell = pgis[i];
						
				if (prices[i] != -1)
				{
					if ((float)price == (float)prices[i])
						customize = 1;
				}
				else
				{// Свои заявки:
					if (pItem->user)
					{
						if (userVolumes[i])
						{
							if (iCol == I_COLUMN_USER)
								++nrUser;
							customize = 2;
						}
					}
				}
				if (customize)
				{							
					if (pgiCell->text.enable)
					{
						if (customize == 1)
						{
							if (pgiCell->text.val1 || iCol == I_COLUMN_PRICE)
								setFont = TRUE;
						}
						else
						{
							if (pgiCell->text.val1 && iCol != I_COLUMN_USER)
								setFont = TRUE;
						}
					}
					if (pgiCell->backgrnd.enable)
					{								
						if (customize == 1)
						{
							if (pgiCell->backgrnd.val1 || iCol == I_COLUMN_PRICE)
								fillCell = TRUE;
						}
						else
						{
							if (pgiCell->backgrnd.val1 && iCol != I_COLUMN_USER)
								fillCell = TRUE;
						}
						if (fillCell)
						{
							if (pgiCell->backgrnd.opacity < 100)
								colorCell = my::lib::SumColorsAlpha(colorCell, pgiCell->backgrnd.color, (double)pgiCell->backgrnd.opacity / 100);
							else
								colorCell = pgiCell->backgrnd.color;
						}
					}
				}						

				if (drawFlags & F_DRAW_BACKGROUND)
				{
					if (! (iCol == I_COLUMN_NULL && !fullArea))
					{
						if (setFont)
						{
							this->CreateFont(pDC, &fontCell, pgiCell);
							pDC->SelectFont(fontCell);
							pDC->SetTextColor(pgiCell->text.cf.crTextColor);
							changeFont = TRUE;
						}
						if (fillCell)
						{
							CBrush brush;
							brush.CreateSolidBrush(colorCell);
							pDC->FillRect (&cell, brush);
						}
					}
				}

			} // for (i)

			if (iCol == I_COLUMN_VOLUME)
			{
				if (drawFlags & F_DRAW_VOLUME)
				{
					if (showVolumeIndicators && volume && volumeMax)
					{// Рисуем индикатор объёма:
						DWORD align = pgiVolume->u.values.value1;
						DWORD margins = pgiVolume->u.values.value2;
						int l, r, t, b;
						Settings::Presentation::Glass::GetMargins(margins, t, b, l, r);
						int cxInd = (int)((double)(cell.Width() - l - r) * volume / volumeMax + 0.5);
						CRect rectInd(cell);
						if (align == ALIGNMENT_LEFT)
						{
							rectInd.left += l;
							rectInd.right = rectInd.left + cxInd;
						}
						else if (align == ALIGNMENT_RIGHT)
						{
							rectInd.right -= r;
							rectInd.left = rectInd.right - cxInd;								
						}
						else //if (align == ALIGNMENT_CENTER)
						{
							rectInd.left += (cell.Width() - cxInd) / 2;
							rectInd.right = rectInd.left + cxInd;
						}
						rectInd.top += t; rectInd.bottom -= b;
						pDC->FillRect (&rectInd, brushInd);
					} // if (showVolumeIndicators)
				}
			} // if (iCol == I_COLUMN_VOLUME)

			pDC->GetTextExtent(text, n, &size);

			int aligment = alignments[iCol];
			if (aligment == ALIGNMENT_LEFT)
				point.x = cell.left + marginLeft;
			else if (aligment == ALIGNMENT_RIGHT)
				point.x = cell.right - size.cx - marginRight;
			else
				point.x = cell.left + (cell.Width() - size.cx) / 2;

			if (iCol == I_COLUMN_PRICE)
			{
				if (drawFlags & F_DRAW_PRICE)
					pDC->ExtTextOut(point.x, yText, ETO_CLIPPED, &rect, text, lstrlen(text), NULL);
			}
			else if (iCol == I_COLUMN_VOLUME)
			{
				if (drawFlags & F_DRAW_VOLUME)
					pDC->ExtTextOut(point.x, yText, ETO_CLIPPED, &rect, text, lstrlen(text), NULL);
			}
			else if (iCol == I_COLUMN_USER && useUserColumn && nrUser)
			{
#if 0
				// Из-за выравнивания в столбцах приходится городить огород:
				// рисуем в одном месте, вычисляем ширину области, затем копируем её в нужное месте.
				CDC dc;
				CBitmap bitmap;

				if (drawFlags & F_DRAW_USER)
				{
					dc.CreateCompatibleDC(*pDC);
					dc.SelectFont(pDC->GetCurrentFont());
					dc.SetTextColor(pDC->GetTextColor());
					dc.SetBkMode(pDC->GetBkMode());
					bitmap.CreateCompatibleBitmap(*pDC, cell.Width(), cell.Height());
					dc.SelectBitmap(bitmap);

					dc.FillRect(userRect, *pBrush2);
				}
				CPoint pt0(0, 0);									
				CPoint pt(pt0);
#if 1
				if (! showGridRows)
					pt.y -= gridWidth;
#endif
				int draw = 0;
#else
				int xUser, cxUser = cell.Width();
				if (nrUser > 1)
				{
					cxUser /= nrUser;
				}
				CRect rectUser(cell);
				int iUser = 0;
#endif				
				COLORREF colorUser = pDC->GetPixel(cell.left, cell.top + 1);

				for (int j = 0; j < SIZEOF_ARRAY(userVolumes); j++)
				{
					double value = userVolumes[j];
					if (value > 0)
					{
#if 0
						if (showIcons)
						{// Значок:
							int iIcon = (j - QuoteTable::ITEM_USER_FIRST);
							xIcon = pt.x;
							yIcon = pt0.y + (int)((double)(cyRow - cyIcon)/2 + 0.5);
							if (drawFlags & F_DRAW_USER)
								::DrawIconEx(dc, xIcon, yIcon, m_icons.hIcon[iIcon], m_icons.size.cx, m_icons.size.cy, 0, NULL, DI_NORMAL);
							pt.x += (cxIcon);
						}
						// Текст:
						n = _stprintf_s(text, SIZEOF_ARRAY(text), TEXT("%d"), (int)value);
						pDC->GetTextExtent(text, n, &size);								
						if (drawFlags & F_DRAW_USER)
							dc.ExtTextOut(pt.x, pt.y + marginTop, ETO_CLIPPED, &rect, text, lstrlen(text), NULL);
						pt.x += size.cx;
						pt.x += 2;
						draw++;
#else						
						CFont fontUser;
						BOOL setFont = FALSE;
						BOOL setFill = FALSE;
						const Settings::GlassItem * pgiUser = pgis[j];

						++iUser;						
						rectUser.right = (iUser < nrUser) ? rectUser.left + cxUser : cell.right;
#if 1
						if (rectUser.PtInRect(m_draw.items[I_ACTIVE_MARKER].point))
							iCellUser = j + QuoteTable::ITEM_USER_FIRST;
#endif
						if (drawFlags & F_DRAW_USER)
						{
							if (pgiUser->text.enable)
							{
								setFont = TRUE;
								this->CreateFont(pDC, &fontUser, pgiUser);
								pDC->SelectFont(fontUser);
								colorPrev = pDC->SetTextColor(pgiUser->text.cf.crTextColor);
								//changeFont = TRUE;
							}
							if (pgiUser->backgrnd.enable)
							{
								COLORREF color;
								if (pgiUser->backgrnd.opacity < 100)
									color = my::lib::SumColorsAlpha(colorUser, pgiUser->backgrnd.color, (double)pgiUser->backgrnd.opacity / 100);
								else
									color = pgiUser->backgrnd.color;

								CBrush brush;
								brush.CreateSolidBrush(color);
								pDC->FillRect (&rectUser, brush);
							}
						}

						int cx = 0;
						if (showIcons)
							cx += cxIcon;
						n = _stprintf_s(text, SIZEOF_ARRAY(text), TEXT("%d"), (int)value);
						pDC->GetTextExtent(text, n, &size);	
						cx += size.cx;						

						if (aligment == ALIGNMENT_LEFT)
							xUser = rectUser.left + marginLeft;
						else if (aligment == ALIGNMENT_RIGHT)
							xUser = rectUser.right - cx - marginRight;
						else
							xUser = rectUser.left + (rectUser.Width() - cx) / 2;

						if (showIcons)
						{// Значок:
							int iIcon = j;
							xIcon = xUser;
							yIcon = point.y + (int)((double)(cyRow - cyIcon)/2 + 0.5);
							if (drawFlags & F_DRAW_USER)
								::DrawIconEx(*pDC, xIcon, yIcon, m_icons.hIcon[iIcon], m_icons.size.cx, m_icons.size.cy, 0, NULL, DI_NORMAL);
							xUser += (cxIcon);
						}
						// Текст:
						if (drawFlags & F_DRAW_USER)
						{
							pDC->ExtTextOut(xUser, yText, ETO_CLIPPED, &rect, text, n, NULL);
							if (setFont)
							{
								pDC->SetTextColor(colorPrev);
								pDC->SelectFont(*pFont);
							}
						}

						rectUser.OffsetRect(cxUser, 0);
#endif
					} // if (value > 0)
				} // for (j)		
#if 0
				if (drawFlags & F_DRAW_USER)
				{
					if (draw)
					{
						int cx = pt.x - pt0.x;
						if (alignments[iCol] == ALIGNMENT_LEFT)
							pt.x = cell.left + marginLeft;
						else if (alignments[iCol] == ALIGNMENT_RIGHT)
							pt.x = cell.right - cx - marginRight;
						else
							pt.x = cell.left + (cell.Width() - cx) / 2;

						// Теперь копируем:
						pDC->BitBlt(pt.x, point.y, cx, cell.Height(), dc, 0, 0, SRCCOPY);
					}
				}
#endif
			} // if (iCol == I_COLUMN_USER)

			if (changeFont)
			{
				pDC->SetTextColor(colorPrev);
				pDC->SelectFont(*pFont);
			}

		} // for (iCol)

		if ((drawFlags & F_DRAW_GRID) && showGridRows)
		{
			pDC->SelectPen(penGrid);
			pDC->MoveTo(pRect->left, point.y);
			pDC->LineTo(pRect->right, point.y);
		}
		if (drawFlags & (F_DRAW_MARKER|F_GET_ACTIVE_ITEM))
		{
			DrawCtrl::Item & marker = m_draw.items[I_ACTIVE_MARKER];
			CRect _rect(pRect->left, point.y + 1, pRect->right, point.y + cyRow);
#if 1
			if (! showGridRows)
				_rect.top -= gridWidth;
#endif
			if (drawFlags & F_DRAW_MARKER)
			{
				if (drawFlags & F_DRAW_MARKER_BY_PRICE)
				{
					if ((float)price == (float)marker.price)
						drawMarker = TRUE;
				}
				else if (_rect.PtInRect(marker.point))
				{
					drawMarker = TRUE;
					marker.price = price;
				}
				if (drawMarker)
				{
					marker.rect = _rect;

					marker.iArea = iArea;
					marker.iItem = iItem;
					marker.type = type;
					marker.dy = dy;
					marker.inc = inc;				
				
					drawFlags &= ~(F_DRAW_MARKER);

					Active::Item & active = m_active.items[I_ACTIVE_MARKER];
					active.item = *pItem;
					active.iCell = iCol;
					active.iUser = iCellUser;
					active.flags = F_ACTIVE_ITEM_IS_VALID|type;
				}
			}
			if (drawFlags & F_GET_ACTIVE_ITEM)
			{				
				if (_rect.PtInRect(cursor.point))
				{
					Active::Item & active = m_active.items[I_ACTIVE_CURSOR];
					active.item = *pItem;
					active.iCell = iCol;
					active.iUser = iCellUser;
					active.flags = F_ACTIVE_ITEM_IS_VALID|type;

					drawFlags &= ~(F_GET_ACTIVE_ITEM);
				}
			} // if (drawFlags & F_GET_ACTIVE_ITEM)
		}
		if (dy > 0)
		{
			y += (cyRow);			
			if (y > rect.bottom)
				break;
		}
		else
		{
			y -= (cyRow);			
			if (y < rect.top - cyRow)
				break;
		}
	} // for (iRow)
	//if (y > yBottom)
	//	yBottom = y;
	// Последняя строка:
	if ((drawFlags & F_DRAW_GRID) && showGridRows)
	{
		if (pArray->count > 0)
		{
			pDC->SelectPen(penGrid);
			pDC->MoveTo(pRect->left, point.y + cyRow);
			pDC->LineTo(pRect->right, point.y + cyRow);
		}
	}

	if (dy < 0)
	{
		y += (cyRow);
		if (! showGridRows)
			y -= gridWidth;
	}

	// Восстанавливаем исходный цвет текста:
	pDC->SetTextColor(colorText);
}

HFONT CDlgGlass::CreateFont (CDC * pDC, CFont * pFont, const Settings::GlassItem * pgi)
{
	BYTE bold, italic;
	int pts;
	int lfHeight;

	bold = (pgi->text.cf.dwEffects & CFE_BOLD) ? TRUE : FALSE;
	italic = (pgi->text.cf.dwEffects & CFE_ITALIC) ? TRUE : FALSE;
	pts = pgi->text.cf.yHeight / 20;
	lfHeight = -MulDiv(pts, pDC->GetDeviceCaps(LOGPIXELSY), 72);

	return pFont->CreateFont(lfHeight, 0, 0, 0, bold ? FW_BOLD : FW_NORMAL, italic, FALSE, 0, DEFAULT_CHARSET, 
		OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH|FF_SWISS,
		pgi->text.cf.szFaceName);
}

LRESULT CDlgGlass::OnWndMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = TRUE;
	switch (uMsg)
	{
	case UM_MENU:
		{
			int what = static_cast<int>(wParam);
			if (what == F_SHOW_MENU)
				this->TrackPopupMenu();
		}
		break;
	default:
		bHandled = FALSE;
	} // switch (uMsg)
	return 0;
}

LRESULT CDlgGlass::OnFocus(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	if (uMsg == WM_SETFOCUS)
	{
		Redraw(F_DRAW_MARKER);
	}
	else if (uMsg == WM_KILLFOCUS)
	{
		Redraw(F_DRAW_MARKER);
	}
	return 0;
}

void CDlgGlass::Resize(int cx, int cy)
{
	CRect rect;
	this->GetClientRect(&rect);

	if (m_bitmap != NULL)
		delete m_bitmap;
	m_bitmap = new CBitmap();
	if (m_bitmap != NULL)
	{
		CPaintDC dc(*this);
		m_bitmap->CreateCompatibleBitmap(dc, rect.Width(), rect.Height());
	}

	int flags = F_DRAW_ALL;
#if 0
	if (! m_draw.marker.rect.IsRectEmpty())
		HideMarker();
#else
	flags |= F_DRAW_MARKER_BY_PRICE;
#endif
	Redraw(flags);
}

void CDlgGlass::OnSize(UINT nType, CSize size)
{
	Resize(size.cx, size.cy);
}

void CDlgGlass::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags)
{
#if defined _DEBUG && 0
	TRACE(__FUNCTION__"(0x%x)\r\n", nChar);
#endif
	if (nChar == VK_ESCAPE)
		::PostMessage(GetParent(), UM_CLOSE_WINDOW, this->IDD, 0);
	else
	{
		if (nChar == VK_UP || nChar == VK_DOWN)
		{
			QuoteTable * pTable = this->GetTable();
			if (pTable)
			{
				const QuoteTable::Properties * pProperties = pTable->GetProperties();
				double price, price0;
				double nearest = 0;
				const Settings::Presentation::Glass::View & view = m_settings.view;
				BOOL buyUp = view.flags & F_GLASS_VIEW_BUY_UP;

				const QuoteTable::Array * pArray;
				const QuoteTable::Array::Item * pItem;

				DrawCtrl::Item & marker = m_draw.items[I_ACTIVE_MARKER];
				price0 = marker.price;

				BOOL passed;
#if 0
				if (nChar == VK_UP)
					nearest = buyUp ? 0 : DBL_MAX;
				else
					nearest = buyUp ? DBL_MAX : 0;

				int count = (view.neutralZone) ? QuoteTable::I_LAST : QuoteTable::I_NEUTRAL;				
				for (int iArray = 0; iArray < count; iArray++)
				{
					pArray = pTable->GetArray(iArray);
					for (int iItem = 0; iItem < pArray->count; iItem++)
					{
						pItem = pArray->GetItem(iItem);
						price = pItem->values[QuoteTable::ITEM_PRICE];
						BOOL passed1 = (price > price0) && (price < nearest);
						BOOL passed2 = (price < price0) && (price > nearest);						
						if (nChar == VK_UP)
							passed = buyUp ? passed2 : passed1;
						else
							passed = buyUp ? passed1 : passed2;
						if (passed)
						{
							if ((pItem->values[QuoteTable::ITEM_VOLUME]) || 
								(iArray == QuoteTable::I_NEUTRAL) || (view.rarefied))								
								nearest = price;
						}
					}
				} // for (iArray)
				if (nearest > 0 && nearest < DBL_MAX)
#else
				int iArray;
				int type = marker.type;
				if (type & DrawInfo::Area::TYPE_NEUTRAL)
					iArray = QuoteTable::I_NEUTRAL;
				else if (type & DrawInfo::Area::TYPE_BUY)
					iArray = QuoteTable::I_BUY;
				else
					iArray = QuoteTable::I_SELL;
				pArray = pTable->GetArray(iArray);

				int iItem = marker.iItem;
				int inc = marker.inc;
				int dy = marker.dy;

				int style = m_settings.view.style;

				int iNextArrayUps[3][2] = {{QuoteTable::I_NEUTRAL, -1}, {-1, QuoteTable::I_NEUTRAL}, {QuoteTable::I_SELL, QuoteTable::I_BUY}};
				int iNextArrayDowns[3][2] = {{-1, QuoteTable::I_NEUTRAL}, {QuoteTable::I_NEUTRAL, -1}, {QuoteTable::I_BUY, QuoteTable::I_SELL}};

				BOOL withoutNeutralZone = FALSE;
				if (! ((view.flags & F_GLASS_VIEW_NEUTRAL_ZONE) && pTable->GetArray(QuoteTable::I_NEUTRAL)->count))
				{
					withoutNeutralZone = TRUE;
					iNextArrayUps[QuoteTable::I_BUY][0] = iNextArrayUps[QuoteTable::I_NEUTRAL][0];
					iNextArrayUps[QuoteTable::I_SELL][1] = iNextArrayUps[QuoteTable::I_NEUTRAL][1];
					iNextArrayDowns[QuoteTable::I_BUY][1] = iNextArrayDowns[QuoteTable::I_NEUTRAL][1];
					iNextArrayDowns[QuoteTable::I_SELL][0] = iNextArrayDowns[QuoteTable::I_NEUTRAL][0];
				}				

				if ((nChar == VK_UP && dy > 0) || (nChar == VK_DOWN && dy < 0))
					inc = -inc;

				for (;;)
				{
					iItem += inc;
				
					if ((inc > 0 && iItem < pArray->count) || (inc < 0 && iItem >= 0))
						passed = TRUE;
					else
						passed = FALSE;
					if (! passed)
					{						
						if (style == GLASS_STYLE_3)
						{
							if (! withoutNeutralZone)
							{
								if (iArray == QuoteTable::I_NEUTRAL && nChar == VK_DOWN)
								{
									iArray = (iItem == pArray->count) ? QuoteTable::I_SELL : QuoteTable::I_BUY;
									iItem = 0;
								}
								else if (iItem < 0 && nChar == VK_UP)
								{
									int iNextArray = QuoteTable::I_NEUTRAL;
									iItem = (iArray == QuoteTable::I_BUY) ? 0 : pTable->GetArray(iNextArray)->count - 1;
									iArray = iNextArray;
								}
								else
									iItem = -1;
							}
							else
							{
								iArray = (iArray == QuoteTable::I_BUY) ? QuoteTable::I_SELL : QuoteTable::I_BUY;
								iItem = 0;
							}
						}
						else
						{
							iArray = (nChar == VK_UP) ? iNextArrayUps[iArray][buyUp] : iNextArrayDowns[iArray][buyUp];
							iItem = -1;
						}
						if (iArray >= 0)
						{
							pArray = pTable->GetArray(iArray);
							if (iItem < 0)
							{
								iItem = 0;
								if (iArray == QuoteTable::I_NEUTRAL)
								{
									if ((buyUp && nChar == VK_UP) || (!buyUp && nChar == VK_DOWN))
										iItem = pArray->count - 1;
								}
							}
							passed = TRUE;
						}
						else
							break;
					}
					if (passed)
					{						
						pItem = pArray->GetItem(iItem);
						price = pItem->values[QuoteTable::ITEM_PRICE];
						if ((pItem->values[QuoteTable::ITEM_VOLUME]) || 
							(iArray == QuoteTable::I_NEUTRAL) || (view.flags & F_GLASS_VIEW_RAREFIED))
						{
							nearest = price;
							break;
						}						
					}
				} // for (;;)
				if (nearest)
#endif
				
				{
					pTable->SetPriceUser(nearest);
					marker.price = nearest;
					Redraw(F_DRAW_MARKER|F_DRAW_MARKER_BY_PRICE);
				}
			}
		}
	}	
}

void CDlgGlass::OnCancel(UINT uNotifyCode, int nID, CWindow wndCtl)
{
	HWND hParent = GetParent();
	if (this->IsMaster())
		::PostMessage(hParent, WM_CLOSE, 0, 0);
	else
		::PostMessage(hParent, UM_CLOSE_WINDOW, this->IDD, 0);		
}

LRESULT CDlgGlass::OnClose(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	if (lParam == 0)
		OnCancel(0, 0, NULL);
	else
		bHandled = TRUE;
	return 0;
}

void CDlgGlass::OnDestroy()
{
#if 0
	my::DialogMessageHook::UninstallHook(*this);
#endif
}

void CDlgGlass::OnProperties()
{
	::PostMessage(GetParent(), UM_SHOW_PROPERTIES, 0, (LPARAM)this->m_hWnd);
}

void CDlgGlass::OnUserTrade()
{
	const QuoteTable * pTable = GetCurrentTable();
	if (pTable != NULL)
	{
		if (theApp.IsActiveTrade(pTable->GetName()))
		{
			int userTrade;
			if (m_user.trade)
				userTrade = 0;
			else
				userTrade = 1;
	
			::PostMessage(GetParent(), UM_USER_TRADE, userTrade, 0);
		}
	}
}

BOOL CDlgGlass::OnEraseBkgnd(CDCHandle dc)
{
	return TRUE;
}

#if 0
void CDlgGlass::OnLButtonDown(UINT nFlags, CPoint point)
{
	my::MouseCapturer::Capture(*this, nFlags, point);
}

void CDlgGlass::OnLButtonUp(UINT nFlags, CPoint point)
{
	my::MouseCapturer::Release();
}
#endif
void CDlgGlass::OnMouseMove(UINT nFlags, CPoint point)
{
	if (m_settings.marker.follow & MARKER_FOLLOW_CURSOR)
	{
		m_draw.items[I_ACTIVE_MARKER].point = point;
		Redraw(F_DRAW_MARKER|F_DRAW_MARKER_BY_POINT);
	}
#if 0
	my::MouseCapturer::OnMouseMove(nFlags, point);
#endif
}

void CDlgGlass::TrackPopupMenu(CPoint * pPoint)
{
	//HideMenu();

	CMenu menu;
	CMenu * pMenu = NULL;

	CMenu mainMenu, subMenu;
	CMenu menu0, menu1;

	menu.LoadMenu(IDR_MENU_GLASS);
	menu0 = menu.GetSubMenu(0);
	menu1 = menu.GetSubMenu(1);

	const QuoteTable * pTable = GetCurrentTable();
	UINT id = ID_USER_TRADE;

	CWindow mainWnd = theApp.GetMainWnd();

	int pos;
	int iMenus[] = {-1, -1, -1};
	int iMenu;

	if (this->IsMaster())
	{		
		mainWnd.SendMessage(UM_MENU, F_GET_MENU_MAIN, (LPARAM)&mainMenu);		
		if (mainMenu.IsMenu())
		{
			subMenu = (mainMenu.GetSubMenu(MMENU_SHOW));
			if (pTable != NULL && theApp.IsActiveTrade(pTable->GetName()))
				subMenu.ModifyMenu(ID_TRANSACTION_INFO, MF_BYCOMMAND|MF_STRING, id, STRING_ACTIVE_BIDSNDEALS);
			else
				for (int i = 0; i < 2; i++) subMenu.DeleteMenu(0, MF_BYPOSITION);			

			ModifyMenuViewGlass(menu1);
			subMenu.InsertMenu(subMenu.GetMenuItemCount() - 4, MF_SEPARATOR|MF_BYPOSITION);
			subMenu.InsertMenu(subMenu.GetMenuItemCount() - 4, MF_POPUP|MF_BYPOSITION|MF_STRING, 
				(UINT_PTR)menu1.m_hMenu, STRING_GLASS_VIEW);

			subMenu.Detach();
			pMenu = &mainMenu;
		}
	}		
	else
	{
		pMenu = &menu0;

		int count;
		pos = count = 0;
		if (pTable == NULL)
			count = 4;
		else if (! theApp.IsActiveTrade(pTable->GetName()))
		{
			count = 4; pos = 0;
		}
		else 
		{
			count = 2; pos = 2;
		}

		for (int i = 0; i < count; i++)
			pMenu->DeleteMenu(pos, MF_BYPOSITION);

		pos = pMenu->GetMenuItemCount();
#if 0
		if (pos > 1)
			pos -= 2;
#endif
		mainWnd.SendMessage(UM_MENU, F_GET_MENU_TRADE|F_GET_MENU_INSTRUMENTS, (LPARAM)&mainMenu);
		if (mainMenu.IsMenu())
		{
			int count;
#if 1
			iMenu = MMENU_TRADING;
			subMenu = (mainMenu.GetSubMenu(iMenu));
			count = subMenu.GetMenuItemCount();
			if (count)
			{
				iMenus[iMenu] = pos;
				pMenu->InsertMenu(pos, MF_POPUP|MF_BYPOSITION|MF_STRING, (UINT_PTR)subMenu.m_hMenu, TEXT("Торговля"));
				pos += 1;
			}
			subMenu.Detach();
#endif
			iMenu = MMENU_INSTRUMENTS;
			subMenu = (mainMenu.GetSubMenu(iMenu));
			count = subMenu.GetMenuItemCount();
			if (count > 1)
			{
#if 0
				TSTRING_STD2(str, size);
				MENUITEMINFO info;
				info.cbSize = sizeof (MENUITEMINFO);
				info.fMask = MIIM_STATE|MIIM_ID|MIIM_STRING|MIIM_CHECKMARKS|MIIM_FTYPE;
				info.dwTypeData = str;				
				int i;
				for (i = 0; i < count; i++)
				{
					info.cch = size;
					if (TRUE == subMenu.GetMenuItemInfo(i, TRUE, &info))
					{
						if (info.fType & MFT_SEPARATOR)
							break;
						else
						{
							int iItem = pos + i;
							pMenu->InsertMenu(iItem, MF_BYPOSITION, info.wID, info.dwTypeData);
							if (info.fState & MFS_CHECKED)
								pMenu->CheckMenuRadioItem(iItem, iItem, iItem, MF_BYPOSITION);
						}
					}
				} // for (i)
				if (i)
					pMenu->InsertMenu(pos + i, MF_BYPOSITION|MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));
#else
				iMenus[iMenu] = pos;
				pMenu->InsertMenu(pos, MF_POPUP|MF_BYPOSITION|MF_STRING, (UINT_PTR)subMenu.m_hMenu, STRING_INSTRUMENT);
				pMenu->InsertMenu(pos + 1, MF_BYPOSITION|MF_SEPARATOR, (UINT)ID_SEPARATOR, TEXT(""));
#endif
			}
			subMenu.Detach();
		}

		pMenu = &menu1;
		if (pMenu)
		{
			ModifyMenuViewGlass(*pMenu);
			menu0.InsertMenu(menu0.GetMenuItemCount(), MF_POPUP|MF_BYPOSITION|MF_STRING, 
				(UINT_PTR)menu1.m_hMenu, STRING_GLASS_VIEW);
		}
		pMenu = &menu0;
		ModifyMenuItemsText(*pMenu);
	}

	// Отображаем всплывающее меню:
	if (pMenu != NULL)
	{
		if (m_user.trade)
			pMenu->CheckMenuItem(id, MF_CHECKED|MF_BYCOMMAND);

		CPoint point;
		::GetCursorPos(&point);
		pMenu->TrackPopupMenu(TPM_LEFTALIGN|TPM_RIGHTBUTTON, point.x, point.y, *this);

		int nrSubMenus = 0;
		for (int i = 0; i < SIZEOF_ARRAY(iMenus); i++)
		{
			iMenu = iMenus[i];
			if (iMenu >= 0)
			{
				pos = iMenu - nrSubMenus;
				subMenu = pMenu->GetSubMenu(pos);
				if (subMenu)
				{
					pMenu->RemoveMenu(pos, MF_BYPOSITION);
					++nrSubMenus;
				}
				subMenu.Detach();			
			}
		} // for (i)
	}
	mainMenu.Detach();
}

LRESULT CDlgGlass::OnCmdMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = FALSE;
	int nCode = HIWORD(wParam);
	if (nCode == 0)
	{// Сообщение от меню
		UINT nID = LOWORD(wParam);
		CWindow wnd = GetParent();
		if (this->IsMaster())
		{			
			::SendMessage(wnd, uMsg, wParam, lParam);
			bHandled = TRUE;
		}
		else
		{
			switch (nID)
			{
			case ID_PROPERTIES: OnProperties(); break;
			case ID_USER_TRADE: OnUserTrade(); break;
			default:			
				::SendMessage(wnd, uMsg, wParam, lParam);
			}
			bHandled = TRUE;
		}
	}
	else
	{
		if (HIWORD(wParam) == BN_CLICKED && LOWORD(wParam) == IDCANCEL)
		{
			if (lParam)
				bHandled = TRUE;
		}
	}
	return 0;
}

void CDlgGlass::OnActivate(UINT nState, BOOL bMinimized, CWindow wndOther)
{
#if 1
	Redraw();
#endif
	::PostMessage(GetParent(), UM_ACTIVE, (WPARAM)this->m_hWnd, (LPARAM)nState);
}

#if USE_TRACK_MOUSE_EVENT
void CDlgGlass::OnMouseHover(WPARAM wParam, CPoint ptPos)
{
	this->SetFocus();
}

void CDlgGlass::OnMouseLeave()
{
#ifdef _DEBUG
	_asm nop;
#endif
}
#endif // USE_TRACK_MOUSE_EVENT


