package minefinder;

import com.sun.jna.platform.win32.User32.{INSTANCE => User32}
import com.sun.jna.platform.win32.Kernel32.{INSTANCE => Kernel32}
import com.sun.jna.platform.win32.{WinBase, WinDef, WinUser}
import com.sun.jna.platform.win32.GDI32
import com.sun.jna.platform.win32.WinGDI._
import com.sun.jna.platform.win32.WinUser._
import com.sun.jna.platform.win32.WinDef._
import com.sun.jna.ptr.IntByReference
import com.sun.jna.Pointer

import java.awt.image.{BufferedImage}
import java.awt.{Robot, MouseInfo, Point}
import java.awt.Rectangle
import java.awt.event.InputEvent
import java.awt.Toolkit
import java.awt.Dimension

import OnceCloseable._

class WindowsException(code:Int) extends  RuntimeException {
	def this() = this(Kernel32.GetLastError)
	override def getLocalizedMessage() = {
		format
	}
	def format = {
		val buffer = java.nio.CharBuffer.allocate(500)
		val length = Kernel32.FormatMessage(WinBase.FORMAT_MESSAGE_FROM_SYSTEM, Pointer.NULL, code, 0, buffer, buffer.length, Pointer.NULL) 
		val message = new String(buffer.array, 0, length)
		""+code+": "+message
	}
}

class DeviceContext(private val handle:HDC, private val hwnd:HWND) extends OnceCloseable {
	def this(hwnd:HWND) = this(User32.GetDC(hwnd), hwnd)
	def closeOnce {
		User32.ReleaseDC(hwnd, handle)
	}
	def getSystemRgn = {
		val rv = new Region()
		rv.getSystemRgn(handle)
		rv
	}
}

class Region(private val handle:HRGN) extends OnceCloseable {
	def this() = this(GDI32.INSTANCE.CreateRectRgn(0, 0, 0, 0))
	private var closed = false
	def closeOnce {
		GDI32.INSTANCE.DeleteObject(handle)
	}
	def getSystemRgn(hdc:HDC) = {
		FullGDI32.INSTANCE.GetRandomRgn(hdc, handle, 4)
	}
	def diff(that:Region) = {
		val rv = new Region()
		val code = GDI32.INSTANCE.CombineRgn(rv.handle, handle, that.handle, RGN_DIFF) 
		if (code == ERROR) {
			throw new Region.RegionException()
		}
		if (code == NULLREGION) {
			null
		} else {
			rv
		}
	}
	override def equals(that:Any) = that match {
		case rgn:Region => {
			val code = FullGDI32.INSTANCE.EqualRgn(handle, rgn.handle)
			if (!code) {
				false
			} else {
				true
			}
		}
		case _ => false
	}
	def getBox = {
		val rv = new RECT()
		if (FullGDI32.INSTANCE.GetRgnBox(handle, rv) == 0) {
			throw new Region.RegionException();
		}
		rv
	}
	
}


object Region {
	class RegionException(code:Int) extends WindowsException(code) {
		def this() = this(Kernel32.GetLastError)
	}
	def createRectRegion(left:Int, top:Int, right:Int, bottom:Int) = {
		new Region(GDI32.INSTANCE.CreateRectRgn(left, top, right, bottom))
	}
}

object Mouse {
	class Location(x:Int, y:Int) extends Point(x, y) {
		def restore = {
			robot.mouseMove(x,y)
		}
	}

	private val robot = new Robot()
	private def getCurrentMousePositionPair = {
		val a = MouseInfo.getPointerInfo().getLocation()
		(a.getX.toInt, a.getY.toInt)
	}
	def currentLocation = {
		val (x, y) = getCurrentMousePositionPair
		new Location(x, y)
	}
}

class InputState(foregroundWindow:Window, mouse:Mouse.Location) {
	def restore = {
		foregroundWindow.bringForeground
		mouse.restore
	}
}

object InputState {
	def get = {
		new InputState(Window.foregroundWindow, Mouse.currentLocation)
	}
}


object Window {
	private val robot = new Robot()
	class WindowCaptureException(message:String) extends RuntimeException(message)
	class WindowRaiseException(message:String) extends RuntimeException(message)
	class WindowRectException(message:String) extends RuntimeException(message)
	class MouseClickException(message:String) extends RuntimeException(message) {
		def this() = this(Window.FormatLastError)
	}
	def EnumWindows(stopcondition:(Window) => Boolean) = {
		User32.EnumWindows(
			new WNDENUMPROC{
				def callback(h:HWND, p:Pointer) = stopcondition(new Window(h))
			},
			Pointer.NULL
		)
	}
	def FindWindow(condition: (Window) => Boolean) = {
		var rv = Option.empty[Window]
		EnumWindows(
			x => { 
				if (condition(x)) {
					rv = Option(x)
					false
				} else {
					true
				}
			}
		)
		rv
	}
	def GetWindowByTitle(title:String) = {
		FindWindow(x => x.text == title)
	}
	def GetMineSweeper = GetWindowByTitle("Minesweeper") orElse GetWindowByTitle("Сапер")
	def FormatLastError = {
		val buffer = java.nio.CharBuffer.allocate(500)
		val code = Kernel32.GetLastError
		val length = Kernel32.FormatMessage(WinBase.FORMAT_MESSAGE_FROM_SYSTEM, Pointer.NULL, code, 0, buffer, buffer.length, Pointer.NULL) 
		val message = new String(buffer.array, 0, length)
		""+code+": "+message
	}
	implicit def toRectangle(rect: WinDef.RECT) =  {
		new java.awt.Rectangle(rect.left, rect.top, rect.right-rect.left, rect.bottom-rect.top)
	}
	def foregroundWindow = {
		val h = User32.GetForegroundWindow;
		if (h == null) {
			throw new WindowRaiseException("No foreground window.")
		}
		new Window(h)
	}
}



class Window(private val handle:HWND) {
//	FullUser32.WM_RBUTTONDOWN // Forcing class initialization
	import Window._
	assert(handle!=Pointer.NULL)
	def createSystemRegion:Region = {
		tryWith(new DeviceContext(handle)) { hdc =>
			{
				val rv = hdc.getSystemRgn
				rv
			}
		}
		
	}
	def hasPoint(x:Int, y:Int) = {
		import FullUser32.PointByValue
		val that = FullUser32.INSTANCE.WindowFromPoint(new PointByValue(x, y))
		if (that == null) {
//			println("Point %d:%d doesn't belong to any window".format(x,y))
			false
		} else {
			val w = new Window(that)
//			println("Point %d:%d belongs to %s(%s)".format(x,y, w.text, w.root.text))
			that == handle
		}
	}
	def isCompletelyVisible = {
		val rect = getRECT
		val margin = 2
		hasPoint(rect.left+margin, rect.top+margin) &&
		hasPoint(rect.left+margin, rect.bottom-margin) &&
		hasPoint(rect.right-margin, rect.top+margin) &&
		hasPoint(rect.right-margin, rect.bottom-margin) &&
		hasPoint((rect.right+rect.left)/2, (rect.bottom+rect.top)/2) &&
		hasPoint(rect.left+20, rect.bottom-20)
	}
	def anyText:String = {
		var w = this
		while (w != null) {
			val t = w.text
			if (t != null && t.length > 0)
				return t
			w = w.parent.getOrElse(null)
		}
		return ""
	}
	def text = {
		var length = User32.GetWindowTextLength(handle)
		val buffer = new Array[Char](length+1)
		length = User32.GetWindowText(handle, buffer, buffer.length)
//		println("Window title length: "+length)
		new String(buffer, 0, length)
	}
	def EnumChilds(stopcondition:(Window) => Boolean) {
		val proc = new WNDENUMPROC{
			def callback(h:HWND, p:Pointer) = stopcondition(new Window(h))
		}
		User32.EnumChildWindows(handle, proc, Pointer.NULL)
	}
	def EnumThreadWindows(stopcondition:(Window) => Boolean) {
		val proc = new WNDENUMPROC{
			def callback(h:HWND, p:Pointer) = stopcondition(new Window(h))
		}
		val thread = User32.GetWindowThreadProcessId(handle, new IntByReference())
		User32.EnumThreadWindows(thread, proc, Pointer.NULL)
	}
	def className = {
		var length = 500
		val buffer = new Array[Char](length+1)
		length = User32.GetClassName(handle, buffer, buffer.length)
//		println("Window title length: "+length)
		new String(buffer, 0, length)
	}
	
	private def rootRaised(execute: => Unit) = {
		import FullUser32._
		if (INSTANCE.IsIconic(handle)) 
			INSTANCE.ShowWindow(handle, SW_RESTORE);
		val flags = SWP_ASYNCWINDOWPOS | SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOSIZE
		if (INSTANCE.SetWindowPos(handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_ASYNCWINDOWPOS | SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOSIZE)) {
			try {
				execute
			} finally {
				INSTANCE.SetWindowPos(handle, HWND_NOTOPMOST, 0, 0, 0, 0, flags);
			}
			true
		}
		false
	}
	def raise:Boolean = {
/*		if (!FullUser32.INSTANCE.BringWindowToTop(handle)) 
			throw new WindowRaiseException(Window.FormatLastError)
		true
*/
		root.rootRaised({})
/*		for (i <- 1 until 10) {
			if (User32.SetForegroundWindow(handle))
				return true
			Thread.sleep(100)
		}
		return false
*/
	}
	def parent:Option[Window] =  {
		val rv = FullUser32.INSTANCE.GetParent(handle)
		if (rv!=Pointer.NULL) {
			Option(new Window(rv))
		} else {
			None
		}
	}
	def root = new Window(FullUser32.INSTANCE.GetAncestor(handle, FullUser32.GA_ROOT))
	
	private def clickInternal(x:Int, y:Int, flag:Int) {
		val robot = new Robot()
		val rect = new WinDef.RECT()
		if (!User32.GetWindowRect(handle, rect))
			throw new WindowRectException(Window.FormatLastError)
		if (!hasPoint(rect.left + x, rect.top+y)) 
			throw new MouseClickException("Point doesn't belong to this window.")
		robot.mouseMove(rect.left + x, rect.top+y)
		robot.mousePress(flag);
		robot.mouseRelease(flag);		
	}
	private def getRECT = {
		val rect = new WinDef.RECT()
		if (!User32.GetWindowRect(handle, rect))
			throw new WindowRectException(Window.FormatLastError)
		rect
	}
	def rectangle: Rectangle = {
		toRectangle(getRECT)
	}
	def lclick(x:Int, y:Int) {
		clickInternal(x, y, InputEvent.BUTTON1_MASK)
	}
	def rclick(x:Int, y:Int) {
		clickInternal(x, y, InputEvent.BUTTON3_MASK)
	}
	def bringForeground = {
		val f = User32.GetForegroundWindow;
		if (f == Pointer.NULL) {
			throw new WindowRaiseException("No window can be made foreground now.")
		} 
		if (f != handle && root.handle != f) {
			println("Bringing up "+ anyText)
			User32.SetForegroundWindow(handle)
		} else {
			true
		}
	}
	def captureImage = {
		import Window.toRectangle
		if (isCompletelyVisible) {
			robot.createScreenCapture(rectangle)
		} else {
//			println("System region box:"+createSystemRegion.getBox)
//			println("Rectangle:"+getRECT)
			null
		}
	}
}