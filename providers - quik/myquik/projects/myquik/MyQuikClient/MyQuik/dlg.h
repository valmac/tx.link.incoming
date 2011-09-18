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
 *  dlg.h
 */

#pragma once

#include <set>

namespace my {
#if USE_MODELESS_DIALOG_IMPL
	template <class T, class TBase = CWindow>
	class ATL_NO_VTABLE ModelessDialogImpl : public CDialogImpl <T>, public CMessageFilter
	{
	protected:
		virtual BOOL PreTranslateMessage(MSG* pMsg) 
		{
			return IsDialogMessage(pMsg);
		}

		void OnFinalMessage(HWND hWnd)
		{
			CMessageLoop* pLoop = theApp.GetMessageLoop();
			ATLASSERT(pLoop != NULL);
			pLoop->RemoveMessageFilter(this);
		}

		BOOL AddMessageFilter(CMessageFilter* pMessageFilter)
		{
			CMessageLoop* pLoop = theApp.GetMessageLoop();
			ATLASSERT(pLoop != NULL);
			return pLoop->AddMessageFilter(pMessageFilter);
		}
	};
#endif // #if USE_MODELESS_DIALOG_IMPL

	typedef ::std::set<HWND> THWNDCollection;

	class DialogMessageHook  
	{
	public:
		// set a dialog message hook for the specified modeless dialog

		static HRESULT InstallHook(HWND hWnd);
		static HRESULT UninstallHook(HWND hWnd);

	private:
		// the hook function
		static LRESULT CALLBACK GetMessageProc(int nCode, WPARAM wParam, LPARAM lParam);

		// the hook handle
		static HHOOK m_hHook;

		// the set of HWNDs we are hooking
		static THWNDCollection m_aWindows;
	};

} // namespace my

