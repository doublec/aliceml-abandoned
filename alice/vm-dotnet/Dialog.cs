using System;
using System.Threading;
using System.Text;
using Win32.Constants;
using Win32.Datatypes;
using Win32.API;

class Dialog {
    int wnd;
    int instance;
    System.Runtime.InteropServices.GCHandle root;
    int xDim;
    int yDim;
    StringBuilder b;
    public String retVal;
    public Dialog(int x, int y) {
	b      = new StringBuilder();
	retVal = "";
	xDim   = (x * 8);
	yDim   = (y * 15);
    }
    public virtual void Run() {
	int instance        = Kernel.GetModuleHandle(null);
	WndClassEx wndclass = new WndClassEx();
	Msg msg             = new Msg();
	String clName       = "Entry Area";

	Win32.WinCB wcb = new Win32.WinCB(this.CallBack);
	root            = System.Runtime.InteropServices.GCHandle.Alloc(wcb);

	wndclass.cbSize        = System.Runtime.InteropServices.Marshal.SizeOf(wndclass);
	wndclass.style         = ClassStyle.HRedraw | ClassStyle.VRedraw;
	wndclass.lpfnWndProc   = wcb;
	wndclass.cbClsExtra    = 0;
	wndclass.cbWndExtra    = 0;
	wndclass.hInstance     = instance;
	this.instance          = instance;
	wndclass.hIcon         = User.LoadIcon(0, IconID.Application);
	wndclass.hCursor       = User.LoadCursor(0, CursorID.Arrow);
	wndclass.hbrBackground = GDI.GetStockObject(Brush.White);

	wndclass.lpszMenuName  = null;
	wndclass.lpszClassName = clName;
	wndclass.hIconSm       = User.LoadIcon(0, IconID.Application);

	if (User.RegisterClassExA(wndclass) == 0) {
	    throw new System.ApplicationException("Unable to Register Entry Dialog");
	}

	wnd = User.CreateWindowEx(0, clName, clName, WinStyle.OverlappedWindow,
				  0, 0, xDim, yDim, 0, 0, instance, 0);

	if (wnd == 0) {
	    throw new System.ApplicationException(Error.GetSystemErrorMessage());
	}

	User.ShowWindow(wnd, ShowWindow.ShowNormal);
	User.UpdateWindow(wnd);

	while (User.GetMessage(msg, 0, 0, 0) != 0) {
	    User.TranslateMessage(msg);
	    User.DispatchMessage(msg);
	}

	root.Free();
    }
    public virtual int CallBack(int wnd, int msg, int w, int l) {
	Rect rect      = new Rect();
	PaintStruct ps = new PaintStruct();
	TextMetric tm  = new TextMetric();
	int dc;

	switch (msg) {
	case WinMsg.Create:
	    return 0;
	case WinMsg.Size:
	    return 0;
	case WinMsg.Char:
	    switch (w) {
	    case '\b':
		if (b.Length > 0) {
		    b = b.Remove(b.Length - 1, 1);
		}
		break;
	    case '\r':
		Notify();
		break;
	    default:
		b = b.Append((char) w);
		break;
	    }

	    User.InvalidateRect(wnd, null, true);
	    return 0;
	case WinMsg.Paint: {
	    int charX, charY, maxX, maxY;
	    String s;

	    if (b.Length == 0) {
		s = "";
	    } else {
		s = b.ToString();
	    }

	    dc = User.BeginPaint(wnd, ps);
	    User.GetClientRect(wnd, rect);

	    ps.paint.left   = rect.left;
	    ps.paint.top    = rect.top;
	    ps.paint.right  = rect.right;
	    ps.paint.bottom = rect.bottom;

	    GDI.SetTextAlign(dc, TextAlign.Left | TextAlign.Top);
	    GDI.SelectObject(dc, GDI.GetStockObject(SystemFont.FixedFont));

	    GDI.GetTextMetrics(dc, tm);
	    charX = tm.MaxCharWidth;
	    charY = (tm.Height +  tm.ExternalLeading);
	    maxX  = (rect.right / charX);
	    maxY  = (rect.bottom / charY);

	    GDI.SetTextColor(dc, 0);
	    GDI.TextOut(dc, 0, 0, s, s.Length);

	    rect.left   = (s.Length * charX);
	    rect.top    = 0;
	    rect.right  = ((s.Length + 1) * charX);
	    rect.bottom = charY;
	    User.FillRect(dc, rect, GDI.GetStockObject(Brush.Black));

	    User.EndPaint(wnd, ps);
	    return 0;
	}
	case WinMsg.Destroy:
	    User.PostQuitMessage(0);
	    return 0;
	}

	return User.DefWindowProc(wnd, msg, w, l);
    }
    public void Notify() {
	lock (this) {
	    if (b.Length == 0) {
		retVal = "";
	    } else {
		retVal = b.ToString();
		b      = new StringBuilder();
	    }
	    Monitor.PulseAll(this);
	}
    }
}

class ReadLine : Alice.Values.Procedure {
    public override object Apply(object obj) {
	Dialog dlg = Execute.dlg;

	lock (dlg) {
	    Monitor.Wait(dlg);
	    return dlg.retVal;
	}
    }
}

class Execute {
    public static Dialog dlg;
    public static object Main(object obj) {
	dlg = new Dialog(40, 3);
	new Thread(new ThreadStart(dlg.Run)).Start();

	return new object[1] {
	    new ReadLine()   // readLine
	};
    }
}
