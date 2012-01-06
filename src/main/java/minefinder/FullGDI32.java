package minefinder;

import com.sun.jna.Native;
import com.sun.jna.platform.win32.GDI32;
import com.sun.jna.platform.win32.WinDef.HDC;
import com.sun.jna.platform.win32.WinDef.HRGN;
import com.sun.jna.platform.win32.WinDef.RECT;
import com.sun.jna.win32.W32APIOptions;

public interface FullGDI32 extends GDI32 {
	static final FullGDI32 INSTANCE = (FullGDI32) Native.loadLibrary("gdi32", FullGDI32.class,
			W32APIOptions.DEFAULT_OPTIONS);
	int GetRandomRgn(HDC hdc, HRGN hrgn, int num);
	boolean EqualRgn(HRGN hSrcRgn1, HRGN hSrcRgn2);
	int GetRgnBox(HRGN hrgn, RECT lprc);
}
