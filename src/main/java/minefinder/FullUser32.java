package minefinder;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.win32.W32APIOptions;

interface FullUser32 extends com.sun.jna.platform.win32.User32 {
	FullUser32 INSTANCE = (FullUser32) Native.loadLibrary("user32", FullUser32.class,
			W32APIOptions.DEFAULT_OPTIONS);

	HWND GetParent(HWND hWnd);
	
	final int GA_PARENT = 1;
	final int GA_ROOT = 2;
	final int GA_ROOTOWNER = 3;
	int SWP_ASYNCWINDOWPOS = 0x4000;
	int SWP_NOACTIVATE = 0x0010;
	int SWP_NOMOVE = 0x0002;
	int SWP_NOSIZE = 0x0001;
	HWND HWND_TOPMOST = new HWND(Pointer.createConstant(-1L));
	HWND HWND_NOTOPMOST = new HWND(Pointer.createConstant(-2L));

	boolean BringWindowToTop(HWND hWnd);
	boolean IsIconic(HWND hWnd);
	HWND GetAncestor(HWND hWnd, int gaFlags);
}