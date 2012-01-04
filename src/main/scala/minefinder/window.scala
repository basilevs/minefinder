package minefinder;

import com.sun.jna.platform.win32.User32.{INSTANCE => User32}
import com.sun.jna.platform.win32.Kernel32.{INSTANCE => Kernel32}
import com.sun.jna.platform.win32.{WinBase, WinDef, WinUser}
import com.sun.jna.platform.win32.GDI32
import com.sun.jna.platform.win32.WinUser._
import com.sun.jna.platform.win32.WinDef._
import com.sun.jna.ptr.IntByReference
import com.sun.jna.Pointer

import java.awt.image.{BufferedImage}
import java.awt.Robot
import java.awt.Rectangle
import java.awt.event.InputEvent
import java.awt.Toolkit
import java.awt.Dimension

object Window {
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
}

class Window(handle:HWND) {
//	FullUser32.WM_RBUTTONDOWN // Forcing class initialization
	import Window._
	assert(handle!=Pointer.NULL)
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
	class WindowCaptureException(message:String) extends RuntimeException(message)
	class WindowRaiseException(message:String) extends RuntimeException(message)
	class WindowRectException(message:String) extends RuntimeException(message)
	class MouseClickException(message:String) extends RuntimeException(message) {
		def this() = this(Window.FormatLastError)
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
		}
		true
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
		root.rootRaised {
			val rect = new WinDef.RECT()
			if (!User32.GetWindowRect(handle, rect))
				throw new WindowRectException(Window.FormatLastError)
			robot.mouseMove(rect.left + x, rect.top+y)
			robot.mousePress(flag);
			Thread.sleep(100);
			robot.mouseRelease(flag);		
		}
	}
	def rectangle: Rectangle = {
		val rect = new WinDef.RECT()
		if (!User32.GetWindowRect(handle, rect))
			throw new WindowRectException(Window.FormatLastError)
		toRectangle(rect)
	}
	def sendInputs(inputs:Iterable[INPUT]) {
		for (input <-inputs) {
			val arr = new Array[INPUT](1)
			arr(0) = input
			if (User32.SendInput(FullUser32.Convert.toDWORD(arr.size), arr, input.size).longValue != 1)
				throw new MouseClickException()
		}
	}
	private val leftFlags = Seq[DWORD](FullUser32.MOUSEEVENTF_MOVE, FullUser32.MOUSEEVENTF_LEFTDOWN, FullUser32.MOUSEEVENTF_LEFTUP)
	private val rightFlags = Seq[DWORD](FullUser32.MOUSEEVENTF_MOVE, FullUser32.MOUSEEVENTF_RIGHTDOWN, FullUser32.MOUSEEVENTF_RIGHTUP)
	def lclick(x:Int, y:Int) {
		root.rootRaised {
			val dim = Toolkit.getDefaultToolkit().getScreenSize();
			val rect = rectangle
			def createInput(flags:DWORD) = {
				new FullUser32.MouseInput(flags, (rect.x + x).toFloat / dim.getWidth.toFloat, (rect.y +y).toFloat / dim.getHeight.toFloat)
			}
			sendInputs(leftFlags.map(createInput))
		}
	}
	def rclick(x:Int, y:Int) {
		root.rootRaised {
			val dim = Toolkit.getDefaultToolkit().getScreenSize();
			val rect = rectangle
			def createInput(flags:DWORD) = {
				new FullUser32.MouseInput(flags, (rect.x + x).toFloat / dim.getWidth.toFloat, (rect.y +y).toFloat / dim.getHeight.toFloat)
			}
			sendInputs(rightFlags.map(createInput))
		}
	}
	def captureImage = {
		val robot = new Robot()
		var rv:BufferedImage = null
		import Window.toRectangle
		root.rootRaised {
			Thread.sleep(300)
			val rect = new WinDef.RECT()
			if (!User32.GetWindowRect(handle, rect))
				throw new WindowRectException(Window.FormatLastError)
			rv = robot.createScreenCapture(rect)
		}
		rv
	}
}