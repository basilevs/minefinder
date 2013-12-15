import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinDef.HWND;
import com.sun.jna.platform.win32.WinDef.LONG;
import com.sun.jna.platform.win32.WinDef.RECT;
import com.sun.jna.platform.win32.WinUser.INPUT;
import com.sun.jna.platform.win32.WinUser.MOUSEINPUT;

import java.awt.Dimension;
import java.awt.Toolkit;

public class SendInput {
	//We won't use relative positioning, so we are adding MOUSEEVENTF_ABSOLUTE mask everywhere
	static final DWORD MOUSEEVENTF_LEFTDOWN = new DWORD(0x0002L|0x8000L);
	static final DWORD MOUSEEVENTF_LEFTUP = new DWORD(0x0004L|0x8000L);
	static final DWORD MOUSEEVENTF_MOVE = new DWORD(0x0001L|0x8000L);
	static final DWORD MOUSEEVENTF_ABSOLUTE = new DWORD(0x8000L);
	public SendInput(){}
	public static INPUT createMouseEvent(DWORD dwFlags, Float x, Float y) {
		MOUSEINPUT i = new MOUSEINPUT();
		i.mouseData = new DWORD(0);
		i.dwFlags = dwFlags;
		i.dx = new LONG((long)(65535*x));
		i.dy = new LONG((long)(65535*y));
		INPUT rv = new INPUT();
		rv.type = new DWORD(Long.valueOf(INPUT.INPUT_MOUSE));
		rv.input.mi = i;
		rv.input.setType("mi");
		return rv;
	}
	public static INPUT[] createMouseClickDirect(int x, int y) {
		DWORD[] eventCodes = new DWORD[]{MOUSEEVENTF_MOVE, MOUSEEVENTF_LEFTDOWN, MOUSEEVENTF_LEFTUP};
		System.out.println(String.format("Clicking in %dx%d", x, y));
//		DWORD[] eventCodes = new DWORD[]{MOUSEEVENTF_LEFTDOWN};
//		DWORD[] eventCodes = new DWORD[]{MOUSEEVENTF_MOVE};
		INPUT[] rv = new INPUT[eventCodes.length];
		Dimension res = Toolkit.getDefaultToolkit().getScreenSize();
		for (int idx = 0; idx < eventCodes.length; ++idx) {
			rv[idx] = createMouseEvent(eventCodes[idx], Float.valueOf(x)/res.width, Float.valueOf(y)/res.height);
		}
		return rv;
	}
	public static int main() {
		new SendInput().testClick();
		return 0;
	}
	public void testClick() {
		//Find a notepad window to click in
		HWND notepad = User32.INSTANCE.FindWindow(null, "Безымянный — Блокнот");
		if (notepad == null) {
			notepad = User32.INSTANCE.FindWindow(null, "Unnamed — Notepad");
		}
		assert(notepad!=null);
		RECT rect = new RECT();
		boolean r = User32.INSTANCE.GetWindowRect(notepad, rect);
		assert(r);
		//Produce event chain for SendInput
		INPUT[] inputs = createMouseClickDirect(rect.left + 100, rect.top +100);
		
		assert(28==inputs[0].size()); // size in bytes, it is always 28 bytes 
		for(INPUT input: inputs) {
			//Fails with IllegalArgumentException: Structure array elements must use contiguous memory (bad backing address at Structure array index 1)
			if (User32.INSTANCE.SendInput(new DWORD(1), new INPUT[]{input}, input.size()).longValue() != 1) {
				System.err.println(Kernel32.INSTANCE.GetLastError());
				assert(false);
			}
		}
		
	}
}

