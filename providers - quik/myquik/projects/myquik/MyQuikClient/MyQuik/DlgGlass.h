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
 *  DlgGlass.h : header file
 */

#pragma once

#include "myclient.h"
#include "tables.h"

#define USE_TRACK_MOUSE_EVENT 0

enum {
	I_COLUMN_PRICE,
	I_COLUMN_VOLUME,
	I_COLUMN_USER,
	I_COLUMN_NULL,
};

// CDlgGlass dialog
class CDlgGlass : public CDialogImpl<CDlgGlass>
{
	friend class CTradeDlg;
	friend class CTradeDlg2;

public:
	enum {
		F_DRAW_NULL = 0x0,
		F_DRAW_FULL = 0x01,
		F_DRAW_COPY = 0x02,
		F_DRAW_MARKER = 0x04,
		F_DRAW_BACKGROUND = 0x08,
		F_DRAW_PRICE = 0x10,
		F_DRAW_VOLUME = 0x20,
		F_DRAW_USER = 0x40,
		F_DRAW_GRID = 0x80,	
		F_DRAW_BORDER = 0x100,
		F_DRAW_MARKER_BY_POINT = 0x1000,
		F_DRAW_MARKER_BY_PRICE = 0x2000,
		F_DRAW_ALL = (F_DRAW_BACKGROUND|F_DRAW_PRICE|F_DRAW_VOLUME|F_DRAW_USER|F_DRAW_GRID|F_DRAW_BORDER|F_DRAW_MARKER),
		F_DRAW_NOW = 0x8000,
	};
	enum {
		F_GET_ACTIVE_ITEM = 0x10000,
		F_ACTIVE_ITEM_IS_VALID = 0x20000,
		F_CHECK_MARKER = 0x400000,
	};

// Construction
public:
	CDlgGlass();
	~CDlgGlass();

// Dialog Data
	enum { IDD = IDD_GLASS };

	LPCTSTR GetName() const { return TEXT("DlgGlass"); }

	BOOL IsUserTrade() const;
	BOOL IsMaster() const;

	BOOL IsMarkerVisible() const;

protected:
	QuoteTable * GetTable();
	QuoteTable * GetCurrentTable();

	void Update(LPCTSTR name, double price = 0);
	void UpdateName(LPCTSTR name = NULL);
	void Empty();

	void SetUserTrade(int set, QuoteTable * pTable, BOOL updateName = TRUE);
	void SetMaster(BOOL master);

	void SetSettings(const Settings::Presentation::Glass & settings) { m_settings = settings; }

	void SetQuantity(double quantity);

	void ModifyMenuViewGlass(CMenu & menu);

protected:
	void Redraw(int flags = F_DRAW_FULL);

	void HideMarker(BOOL hide = TRUE, BOOL redraw = FALSE);
	void HideMenu();

	void SetDlgIcon(HINSTANCE hInst, BOOL freeze);
	void SetFreeze(BOOL freeze = TRUE);

	void SetTable(QuoteTable * pTable);

	void ClearActiveItems();

protected:
	void SetFocus(BOOL focus = TRUE);
	void SelectItem(CPoint * pPoint = NULL, int flags = 0);
	void CheckActiveItem(CPoint * pPoint = NULL, int flags = 0);

	void TrackPopupMenu(CPoint * pPoint = NULL);

protected:
	Settings::Presentation::Glass m_settings;
	QuoteTable * m_pTable;

// Implementation
protected:
	HICON m_hIcon;

	CString m_name;

protected:
	CBitmap * m_bitmap;
	struct DrawCtrl {
		int flags;
		struct Item {
			CRect rect;
			CPoint point;
			double price;
			int iArea;
			int iItem;
			int type;
			int dy;
			int inc;
		};
		Item items[2];
	} m_draw;

	struct Icons {
		Icons() : size(16, 16)
		{}
		HICON hIcon[6];
		CSize size;
	} m_icons;

	struct User {
		User() : trade(0), pTable(NULL)
		{}
		int trade;
		QuoteTable * pTable;
	} m_user;

	BOOL m_master;

	enum {
		I_ACTIVE_MARKER,
		I_ACTIVE_CURSOR,
	};
	struct Active {
		struct Item {
			QuoteTable::Array::Item item;
			int iCell;
			int iUser;
			int flags;
		};
		Item items[2];

		void ClearItem(int i) {items[i].item.Clear();}
	} m_active;

	double quantity;

protected:
	const Active::Item & GetActiveItem (int i) const {return m_active.items[i];}

protected:
	void Paint();

	void Resize(int cx = 0, int cy = 0);

	void Draw();
	void Draw(CDC * pDC, CRect & rect, const Settings::Presentation::Glass & settings, int & drawMarker);

	class DrawInfo {
	public:
		struct Item {
			CRect rect;
		};
		struct Area {
			enum Type {
				TYPE_BUY = 0x1,
				TYPE_SELL = 0x2,
				TYPE_NEUTRAL = 0x10,
			};

			int i;
			int type;

			Item items[4];
			CRect rect;
			CRect rectW;
			COLORREF color;
			struct User {
				CBrush brush;
			} user;
			DWORD alignment;

			const Settings::GlassItem * pgi;
		};

		Area area[2];
	};

	void DrawArea(CDC * pDC, 
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
		int & drawMarker);

	HFONT CreateFont (CDC * pDC, CFont * pFont, const Settings::GlassItem * pgi);

	void OnProperties();
	void OnUserTrade();

protected:
	CWindow GetParent() { return theApp.GetMainWnd(); }

protected:
	BEGIN_MSG_MAP_EX(CDlgGlass)
		MSG_WM_INITDIALOG(OnInitDialog)
		COMMAND_ID_HANDLER_EX(IDCANCEL, OnCancel)
		MSG_WM_PAINT(OnPaint)
		MSG_WM_SIZE(OnSize)
		MSG_WM_KEYDOWN(OnKeyDown)
		MESSAGE_HANDLER(WM_CLOSE, OnClose)
		MSG_WM_ERASEBKGND(OnEraseBkgnd)
#if 0
		MSG_WM_LBUTTONDOWN(OnLButtonDown)
		MSG_WM_LBUTTONUP(OnLButtonUp)
		MSG_WM_RBUTTONDOWN(OnRButtonDown)
		MSG_WM_RBUTTONUP(OnRButtonUp)
#endif
		MSG_WM_MOUSEMOVE(OnMouseMove)
		MESSAGE_RANGE_HANDLER(WM_SETFOCUS, WM_KILLFOCUS, OnFocus)
		MESSAGE_RANGE_HANDLER(UM_BASE + 1, UM_LAST - 1, OnWndMsg)
		MESSAGE_HANDLER(WM_COMMAND, OnCmdMsg)
		MSG_WM_DESTROY(OnDestroy)
		MSG_WM_ACTIVATE(OnActivate)
#if USE_TRACK_MOUSE_EVENT
		MSG_WM_MOUSEHOVER(OnMouseHover)
		MSG_WM_MOUSELEAVE(OnMouseLeave)
#endif
	END_MSG_MAP()

	BOOL OnInitDialog(CWindow wndFocus, LPARAM lInitParam);

	void OnPaint(CDCHandle );
	void OnSize(UINT nType, CSize size);
	void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
	void OnCancel(UINT uNotifyCode, int nID, CWindow wndCtl);
	void OnDestroy();
	LRESULT OnClose(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);

	BOOL OnEraseBkgnd(CDCHandle dc);
#if 0
	void OnLButtonDown(UINT nFlags, CPoint point);
	void OnLButtonUp(UINT nFlags, CPoint point);
#endif
	void OnMouseMove(UINT nFlags, CPoint point);

	LRESULT OnFocus(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);

	LRESULT OnWndMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
	LRESULT OnCmdMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);

	void OnActivate(UINT nState, BOOL bMinimized, CWindow wndOther);
#if USE_TRACK_MOUSE_EVENT
	void OnMouseHover(WPARAM wParam, CPoint ptPos);
	void OnMouseLeave();
#endif
};
