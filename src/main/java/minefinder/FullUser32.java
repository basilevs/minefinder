package minefinder;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.win32.W32APIOptions;
import com.sun.jna.platform.win32.WinDef.HWND;
import com.sun.jna.platform.win32.WinDef.WPARAM;
import com.sun.jna.platform.win32.WinDef.LPARAM;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinDef.LONG;
import com.sun.jna.platform.win32.WinUser.INPUT;
import com.sun.jna.platform.win32.WinUser.POINT;
import com.sun.jna.platform.win32.WinUser.MOUSEINPUT;

public interface FullUser32 extends com.sun.jna.platform.win32.User32 {
	static final FullUser32 INSTANCE = (FullUser32) Native.loadLibrary("user32", FullUser32.class,
			W32APIOptions.DEFAULT_OPTIONS);

	HWND GetParent(HWND hWnd);
	
	final int GA_PARENT = 1;
	final int GA_ROOT = 2;
	final int GA_ROOTOWNER = 3;
	int SWP_ASYNCWINDOWPOS = 0x4000;
	int SWP_NOACTIVATE = 0x0010;
	int SWP_NOMOVE = 0x0002;
	int SWP_NOSIZE = 0x0001;
	static int WM_LBUTTONDOWN = 0x0201;
	static int WM_LBUTTONUP = 0x0202;
	static int WM_RBUTTONDOWN = 0x0204;
	static int WM_RBUTTONUP = 0x0205;
//	WPARAM MK_LBUTTON = new WPARAM(0x0001L);
//	WPARAM MK_RBUTTON = new WPARAM(0x0002L);
	HWND HWND_TOPMOST = new HWND(Pointer.createConstant(-1L));
	HWND HWND_NOTOPMOST = new HWND(Pointer.createConstant(-2L));
	static final DWORD MOUSEEVENTF_LEFTDOWN = new DWORD(0x0002L|0x8000L);
	static final DWORD MOUSEEVENTF_LEFTUP = new DWORD(0x0004L|0x8000L);
	static final DWORD MOUSEEVENTF_RIGHTDOWN = new DWORD(0x0008L|0x8000L);
	static final DWORD MOUSEEVENTF_RIGHTUP = new DWORD(0x0010L|0x8000L);
	static final DWORD MOUSEEVENTF_MOVE = new DWORD(0x0001L|0x8000L);
	static final DWORD MOUSEEVENTF_ABSOLUTE = new DWORD(0x8000L);

	boolean BringWindowToTop(HWND hWnd);
	boolean IsIconic(HWND hWnd);
	HWND GetAncestor(HWND hWnd, int gaFlags);
	HWND WindowFromPoint(PointByValue point);
	class MouseInput extends INPUT {
		public MouseInput (DWORD dwFlags, double x, double y) {
			type = new DWORD(Long.valueOf(INPUT_MOUSE));
			MOUSEINPUT i = new MOUSEINPUT();
			i.mouseData = new DWORD(0);
			i.dwFlags = dwFlags;
			i.dx = new LONG((long)(65535*x));
			i.dy = new LONG((long)(65535*y));
			input.mi = i;
			input.setType("mi");
		}
	}
	static class PointByValue extends POINT implements Structure.ByValue {
		public PointByValue(int x, int y) {
			super(x, y);
		}
	}
	class Convert {
		public static DWORD toDWORD(long i) {
			return new DWORD(i);
		}
	}
}