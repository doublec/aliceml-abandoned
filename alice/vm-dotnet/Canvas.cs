using System;
using System.Threading;
using Win32.Constants;
using Win32.Datatypes;
using Win32.API;

class Helper {
    public static int LoWord(int l) {
	return ((int) ((short) l));
    }
    public static int HiWord(int l) {
	return (int) (((uint) l >> 16) & 0xFFFF);
    }
    public static int Abs(int a) {
	return ((a <= 0) ? -a : a);
    }
    public static int Min(int a, int b) {
	return ((a < b) ? a : b);
    }
    public static int Max(int a, int b) {
	return ((a > b) ? a : b);
    }
    public static void Print(System.String s) {
	//	System.Console.Write(s);
    }
    public static void PrintLn(System.String s) {
	//	System.Console.WriteLine(s);
    }
}

class LineEntry {
    public System.Collections.Hashtable line;
    public int max;
    public LineEntry() {
	line = new System.Collections.Hashtable();
	max  = 0;
    }
}

class CanvasEntry {
    public Canvas visual;
    public int x;
    public int y;
    public uint color;
    public String text;
    System.Collections.Hashtable home;
    Object key;
    public CanvasEntry(Canvas visual, int x, int y, String text, uint color,
		       System.Collections.Hashtable home, Object key) {
	this.visual = visual;
	this.x      = x;
	this.y      = y;
	this.text   = text;
	this.color  = color;
	this.home   = home;
	this.key    = key;
    }
    public void Undraw() {
	visual.AddToRemoveLine(this);
	home.Remove(key);
    }
}

class CanvasInfo {
    public int cxChar;
    public int cyChar;
    public int cxClient;
    public int cyClient;
    public int cxPage;
    public int cyPage;
}

class Canvas {
    static String clName = "Canvas";
    int wnd;
    int instance;
    CanvasInfo info;
    System.Collections.Hashtable lines;
    System.Collections.Hashtable tagDict;
    LineEntry removeLine;
    int maxX;
    int maxY;
    int scrollMaxX;
    int scrollMaxY;
    int curVertPos;
    int curHorzPos;
    int oldVertPos;
    int oldHorzPos;
    public Canvas() {
	maxX       = 0;
	maxY       = -1;
	curHorzPos = 0;
	curVertPos = 0;
	info       = new CanvasInfo();
	lines      = new System.Collections.Hashtable();
	tagDict    = new System.Collections.Hashtable();
	removeLine = new LineEntry();
    }
    public void CreateWindow(int parent, int instance, int width, int height) {
	RegisterCanvasClass();
	wnd = User.CreateWindowEx(WinStyle.ControlParent, clName, null,
				  WinStyle.Child | WinStyle.Visible |
				  WinStyle.HScroll | WinStyle.VScroll,
				  0, 0, width, height, parent, 0, instance, 0);
	
	if (wnd == 0) {
	    throw new System.ApplicationException(Error.GetSystemErrorMessage());
	}
	
	User.ShowWindow(wnd, ShowWindow.ShowNormal);
	User.UpdateWindow(wnd);
    }
    public virtual int CallBack(int wnd, int msg, int w, int l) {
	//	lock (this) {
	Rect rect      = new Rect();
	PaintStruct ps = new PaintStruct();
	ScrollInfo si  = new ScrollInfo();
	int dc;
	
	switch (msg) {
	case WinMsg.Create:
	    Helper.PrintLn((System.String) "Canvas: Create Msg");
	    return 0;
	case WinMsg.Paint:
	    Helper.PrintLn((System.String) "Canvas: Paint Msg");
	    //User.GetUpdateRect(wnd, rect, false);
	    dc = User.BeginPaint(wnd, ps);
	    User.GetClientRect(wnd, rect);
	    
	    ps.paint.left   = rect.left;
	    ps.paint.top    = rect.top;
	    ps.paint.right  = rect.right;
	    ps.paint.bottom = rect.bottom;
	    
	    Undraw(dc);
	    DrawAllItems(dc, curVertPos, Helper.Min((curVertPos + info.cyPage), maxY));

	    User.EndPaint(wnd, ps);
	    Helper.PrintLn("Canvas: Leaving Paint Handler");
	    return 0;
	case WinMsg.VScroll: {
	    int newVertPos;
	    Helper.PrintLn((System.String) "Canvas: VScroll Msg");

	    switch (Helper.LoWord(w)) {
	    case ScrollBar.Top:
		newVertPos = 0;
		break;
	    case ScrollBar.Bottom:
		newVertPos = maxY;
		break;
	    case ScrollBar.LineUp:
		newVertPos = Helper.Max(0, (curVertPos - 1));
		break;
	    case ScrollBar.LineDown:
		newVertPos = Helper.Min(scrollMaxY, (curVertPos + 1));
		break;
	    case ScrollBar.PageUp:
		newVertPos = Helper.Max(0, (curVertPos - info.cyPage));
		break;
	    case ScrollBar.PageDown:
		newVertPos = Helper.Min(scrollMaxY, (curVertPos + info.cyPage));
		break;
	    case ScrollBar.ThumbTrack:
		newVertPos = Helper.HiWord(w);
		break;
	    default:
		newVertPos = curVertPos;
		break;
	    }

	    if (newVertPos != curVertPos) {
		oldVertPos = curVertPos;
		curVertPos = newVertPos;
		User.SetScrollPos(wnd, ScrollBar.Vert, newVertPos, true);
		User.ScrollWindowEx(wnd, 0, info.cyChar * (oldVertPos - newVertPos),
				    null, null, 0, null,
				    ScrollWindow.Invalidate | ScrollWindow.Erase);
		User.UpdateWindow(wnd);
	    }
	    return 0;
	}
	case WinMsg.HScroll: {
	    int newHorzPos;

	    Helper.PrintLn((System.String) "Canvas: HScroll Msg");
	    switch (Helper.LoWord(w)) {
	    case ScrollBar.LineLeft:
		newHorzPos = Helper.Max(0, (curHorzPos - 1));
		break;
	    case ScrollBar.LineRight:
		newHorzPos = Helper.Min(scrollMaxX, (curHorzPos + 1));
		break;
	    case ScrollBar.PageLeft:
		newHorzPos = Helper.Max(0, (curHorzPos - info.cxPage));
		break;
	    case ScrollBar.PageRight:
		newHorzPos = Helper.Min(scrollMaxX, (curHorzPos + info.cxPage));
		break;
	    case ScrollBar.ThumbPosition:
		newHorzPos = Helper.HiWord(w);
		break;
	    default:
		newHorzPos = curHorzPos;
		break;
	    }
	    if (newHorzPos != curHorzPos) {
		oldHorzPos = curHorzPos;
		curHorzPos = newHorzPos;
		User.SetScrollPos(wnd, ScrollBar.Horz, newHorzPos, true);
		User.ScrollWindowEx(wnd, info.cxChar * (oldHorzPos - curHorzPos), 0,
				    null, null, 0, null,
				    ScrollWindow.Invalidate | ScrollWindow.Erase);
		User.UpdateWindow(wnd);
	    }
	    return 0;
	}
	case WinMsg.Destroy:
	    Helper.PrintLn((System.String) "Canvas: Destroy Msg");
	    User.PostQuitMessage(0);
	    return 0;
	}
	//	}
	return User.DefWindowProc(wnd, msg, w, l);
    }
    public void AdjustSize(int width, int height) {
	Helper.PrintLn((System.String) "Canvas: AdjustSize");
	Helper.Print((System.String) "width "); Helper.PrintLn(((Int32) width).ToString());
	Helper.Print((System.String) "height "); Helper.PrintLn(((Int32) height).ToString());
	
	ComputeFontMetrics(wnd, width, height);
	scrollMaxY = Helper.Max(maxY - info.cyPage, 0);
	scrollMaxX = Helper.Max(maxX - info.cxPage, 0);
	User.SetScrollRange(wnd, ScrollBar.Vert, 0, scrollMaxY, true);
	User.SetScrollRange(wnd, ScrollBar.Horz, 0, scrollMaxX, true);
	User.MoveWindow(wnd, 0, 0, width, height, true);
    }
    void RegisterCanvasClass() {
	int instance        = Kernel.GetModuleHandle(null);
	WndClassEx wndclass = new WndClassEx();
	Msg msg             = new Msg();

	Win32.WinCB wcb = new Win32.WinCB(this.CallBack);
	System.Runtime.InteropServices.GCHandle root =
	    System.Runtime.InteropServices.GCHandle.Alloc(wcb);

	wndclass.cbSize        = System.Runtime.InteropServices.Marshal.SizeOf(wndclass);
	wndclass.style         = ClassStyle.HRedraw | ClassStyle.VRedraw;
	wndclass.lpfnWndProc   = wcb;
	wndclass.cbClsExtra    = 0;
	wndclass.cbWndExtra    = 0;
	wndclass.hInstance     = instance;
	this.instance          = instance;
	wndclass.hIcon         = 0;
	wndclass.hCursor       = User.LoadCursor(0, CursorID.Arrow);
	wndclass.hbrBackground = GDI.GetStockObject(Brush.White);

	wndclass.lpszMenuName  = null;
	wndclass.lpszClassName = clName;
	wndclass.hIconSm       = User.LoadIcon(0, IconID.Application);

	if (User.RegisterClassExA(wndclass) == 0) {
	    throw new System.ApplicationException("Unable to Register Canvas");
	}
    }
    void ComputeFontMetrics(int wnd, int width, int height) {
	int hdc       = User.GetDC(wnd);
	TextMetric tm = new TextMetric();

	GDI.SelectObject(hdc, GDI.GetStockObject(SystemFont.FixedFont));

	GDI.GetTextMetrics(hdc, tm);
	info.cxChar   = tm.MaxCharWidth;
	info.cyChar   = (tm.Height + tm.ExternalLeading);
	User.ReleaseDC(wnd, hdc);
	info.cxClient = width;
	info.cyClient = height;
	info.cxPage   = (info.cxClient / info.cxChar);
	info.cyPage   = (info.cyClient / info.cyChar);
    }
    void DrawAllItems(int dc, int startY, int endY) {
	GDI.SetTextAlign(dc, TextAlign.Left | TextAlign.Top);
	GDI.SelectObject(dc, GDI.GetStockObject(SystemFont.FixedFont));
	
	for (int i = startY; i <= endY; i++) {
	    Object LineKey = (Int32) i;
	    if (lines.ContainsKey(LineKey)) {
		LineEntry entry                   = (LineEntry) lines[LineKey];
		System.Collections.Hashtable line = entry.line;

		for (int k = 0; k < entry.max; k++) {
		    Object ItemKey = (Int32) k;
		    if (line.ContainsKey(ItemKey)) {
			CanvasEntry ce = (CanvasEntry) line[ItemKey];
			GDI.SetTextColor(dc, ce.color);
			Helper.Print((System.String) "DrawAllItems: ");
			Helper.PrintLn((System.String) ce.text);
			GDI.TextOut(dc,
				    (ce.x - curHorzPos) * info.cxChar,
				    (ce.y - startY) * info.cyChar,
				    ce.text, ce.text.Length);
		    }
		}
	    }
	}
    }
    void Put(System.Collections.Hashtable dict, Object index, Object val) {
	if (dict.ContainsKey(index)) {
	    dict[index] = val;
	}
	else {
	    dict.Add(index, val);
	}
    }
    public void PrintXY(int x, int y, String text, int tag, int color) {
	lock (this) {
	    Object Key         = (Int32) y;
	    LineEntry entry    = null;
	    Object itemKey     = null;
	    CanvasEntry cEntry = null;
	    
	    maxY = Helper.Max(maxY, y);
	    maxX = Helper.Max(maxX, (x + text.Length));
	    if (lines.ContainsKey(Key)) {
		entry = (LineEntry) lines[Key];
	    }
	    else {
		entry = new LineEntry();
		lines.Add(Key, entry);
	    }
	    itemKey = (Int32) entry.max;
	    entry.max += 1;
	    if (itemKey == null) {
		Helper.PrintLn((System.String) "Canvas:printXY: null itemKey problem");
	    }
	    cEntry  = new CanvasEntry(this, x, y, text, (uint) color, entry.line, itemKey);
	    Put(entry.line, itemKey, cEntry);
	    Put(tagDict, (Int32) tag, cEntry);
	    Flush();
	}
    }
    public void ClearXY(int tag) {
	lock (this) {
	    ((CanvasEntry) tagDict[tag]).Undraw();
	    tagDict.Remove(tag);
	    Flush();
	}
    }
    public void AddToRemoveLine(CanvasEntry entry) {
	Int32 Key = (Int32) removeLine.max;
	
	removeLine.max += 1;
	removeLine.line.Add(Key, entry);
    }
    public void Undraw(int dc) {
	for (int i = 0; i < removeLine.max; i++) {
	    CanvasEntry entry = (CanvasEntry) removeLine.line[(Int32) i];
	    int maxY          = (curVertPos + info.cyPage);
	    
	    if ((entry.y >= curVertPos) && (entry.y <= maxY)) {
		Rect rect = new Rect();
		int newX  = (entry.x - curHorzPos);
		int newY  = (entry.y - curVertPos);

		Helper.Print((System.String) "Undraw: ");
		Helper.PrintLn(entry.text);
		Helper.PrintLn(((Int32) entry.x).ToString());
		Helper.PrintLn(((Int32) entry.y).ToString());
		
		rect.left   = newX * info.cxChar;
		rect.top    = newY * info.cyPage;
		rect.right  = (newX + entry.text.Length) * info.cxChar;
		rect.bottom = (newY + 1) * info.cyChar;
		User.FillRect(dc, rect, GDI.GetStockObject(Brush.White));

//  		GDI.SetTextColor(dc, 0xFFFFFF);
//  		GDI.TextOut(dc,
//  			    (entry.x - curHorzPos) * info.cxChar,
//  			    (entry.y - curVertPos) * info.cyChar,
//  			    entry.text, entry.text.Length);
	    }
	    removeLine.line.Remove(i);
	}
	removeLine.max = 0;
    }
    public void Flush() {
	User.InvalidateRect(wnd, null, false);
	scrollMaxY = Helper.Max(maxY - info.cyPage, 0);
	scrollMaxX = Helper.Max(maxX - info.cxPage, 0);
	User.SetScrollRange(wnd, ScrollBar.Vert, 0, scrollMaxY, true);
	User.SetScrollRange(wnd, ScrollBar.Horz, 0, scrollMaxX, true);
    }
}

class TreeNode {
    protected static int curTag = 0;
    protected Canvas visual;
    protected TreeNode parent;
    protected Object val;
    protected Object tyVal;
    protected int index;
    protected int depth;
    protected int xDim;
    protected int yDim;
    protected bool dazzle;
    protected bool dirty;
    protected int tag;
    public TreeNode(TreeNode parent, Object val, Object tyVal,
		    int index, Canvas visual, int depth) {
	this.parent = parent;
	this.val    = val;
	this.tyVal  = tyVal;
	this.index  = index;
	this.depth  = depth;
	this.visual = visual;
	this.dazzle = true;
	this.dirty  = true;
	this.tag    = curTag++;
    }
    public int GetXDim() {
	return xDim;
    }
    public int GetRootIndex() {
	if (parent == null) {
	    return index;
	}
	else {
	    return parent.GetRootIndex();
	}
    }
    public int GetYDim() {
	return yDim;
    }
    public virtual int GetLastXDim() {
	return xDim;
    }
    public virtual bool IsHorzMode() {
	return true;
    }
    public virtual void Layout() {
	xDim = 0;
	yDim = 1;
    }
    public virtual void Draw(int x, int y) {
	return;
    }
    public virtual void Undraw() {
	return;
    }
    public virtual void Replace(TreeNode parent, Object val, Object tyVal,
				int index, int depth) {
	Helper.PrintLn((System.String) "TreeNode: Replace: something goes wrong");
	throw new System.Exception("Unable to Replace base type");
    }

}

class SimpleNode : TreeNode {
    protected String str;
    protected int x;
    protected int y;
    protected int color;
    public SimpleNode(TreeNode parent, Object val, Object tyVal,
		      int index, Canvas visual, int depth) :
	base(parent, val, tyVal, index, visual, depth) {
	color = 0;
    }
    public override void Layout() {
	if (dazzle) {
	    str    = CreateString();
	    xDim   = str.Length;
	    yDim   = 1;
	    dazzle = false;
	    dirty  = true;
	}
    }
    protected void DoDraw(int x, int y) {
	this.x = x;
	this.y = y;
	visual.PrintXY(x, y, str, tag, color);
    }
    public override void Draw(int x, int y) {
	if (dirty) {
	    DoDraw(x, y);
	    dirty = false;
	}
	else {
	    if ((x != this.x) || (this.y != y)) {
		visual.ClearXY(tag);
		DoDraw(x, y);
	    }
	}
    }
    public override void Undraw() {
	if (!dirty) {
	    visual.ClearXY(tag);
	    dirty = true;
	}
    }
    public virtual String CreateString() {
	return null;
    }
					 
}

class IntNode : SimpleNode {
    public IntNode(TreeNode parent, Object val, Object tyVal,
		   int index, Canvas visual, int depth) :
	base(parent, val, tyVal, index, visual, depth) {
	color = 0xF020A0;
    }
    public override String CreateString() {
	if (val is Int32) {
	    int tmp = (int) val;

	    if (tmp < 0) {
		return System.String.Concat("~", ((Int32) Math.Abs(tmp)).ToString());
	    }
	    else {
		return val.ToString();
	    }
	}
	else {
	    return val.ToString();
	}
    }
}

class WordNode : SimpleNode {
    public WordNode(TreeNode parent, Object val, Object tyVal,
		    int index, Canvas visual, int depth) :
	base(parent, val, tyVal, index, visual, depth) {
	color = 0xF020A0;
    }
    public override String CreateString() {
	return System.String.Concat("0wx", val.ToString());
    }
}

class CharNode : SimpleNode {
    public CharNode(TreeNode parent, Object val, Object tyVal,
		    int index, Canvas visual, int depth) :
	base(parent, val, tyVal, index, visual, depth) {
	color = 0x8F8FBC;
    }
    public override String CreateString() {
	return System.String.Concat("#\"", val.ToString(), "\"");
    }
}

class BoolNode : SimpleNode {
    public BoolNode(TreeNode parent, Object val, Object tyVal,
		    int index, Canvas visual, int depth) :
	base(parent, val, tyVal, index, visual, depth) {
	color = 0xF020A0;
    }
    public override String CreateString() {
	return (val.Equals((Int32) 0) ? "false" : "true");
    }
}

class FunNode : SimpleNode {
    public FunNode(TreeNode parent, Object val, Object tyVal,
		    int index, Canvas visual, int depth) :
	base(parent, val, tyVal, index, visual, depth) {
	color = 0;
    }
    public override String CreateString() {
	return "<Fun>";
    }
}

class StringNode : SimpleNode {
    public StringNode(TreeNode parent, Object val, Object tyVal,
		      int index, Canvas visual, int depth) :
	base(parent, val, tyVal, index, visual, depth) {
	color = 0x2222b2;
    }
    public override String CreateString() {
	return System.String.Concat("\"", val.ToString(), "\"");
    }
}

class SimpleStringNode : SimpleNode {
    public SimpleStringNode(TreeNode parent, Object val, Object tyVal,
			    int index, Canvas visual, int depth) :
	base(parent, val, tyVal, index, visual, depth) {
	color = 0;
    }
    public override String CreateString() {
	return val.ToString();
    }
}

class RealNode : SimpleNode {
    public RealNode(TreeNode parent, Object val, Object tyVal,
		    int index, Canvas visual, int depth) :
	base(parent, val, tyVal, index, visual, depth) {
	color = 0xF020A0;
    }
    public override String CreateString() {
	return System.String.Concat(val.ToString());
    }
}

class FutureNode : SimpleNode {
    static Object Lock = new Object();
    Thread myThread;
    public FutureNode(TreeNode parent, Object val, Object tyVal,
		      int index, Canvas visual, int depth) :
	base(parent, val, tyVal, index, visual, depth) {
	color = 0x0000FF;
    }
    public override String CreateString() {
	return "<Future>";
    }
    public override void Draw(int x, int y) {
	if (dirty) {
	    DoDraw(x, y);
	    dirty = false;
	    myThread = new Thread(new ThreadStart(this.Run));
	    myThread.Start();
	}
	else {
	    if ((this.x != x) || (this.y != y)) {
		visual.ClearXY(tag);
		DoDraw(x, y);
	    }
	}
    }
    public override void Undraw() {
	if (!dirty) {
	    visual.ClearXY(tag);
	    dirty = true;
	    myThread.Abort();
	}
    }
    public virtual void Run() {
	Helper.PrintLn((System.String) "FutureNode:Run: Thread Launched");
	val = ((Alice.Values.Transient) val).Await(); // to be determined
	Helper.Print((System.String) "FutureNode:Run: calling update with");
	Helper.PrintLn((System.String) val.ToString());
	lock (Lock) {
	    Inspector.GetInspector().Update(this);
	}
    }
    public void Link() {
	visual.ClearXY(tag);
	if (parent != null) {
	    parent.Replace(parent, val, tyVal, index, depth);
	}
	else {
	    Inspector.GetInspector().Replace(parent, val, tyVal, index, depth);
	}
    }
}

class SeparatorNode : TreeNode {
    protected SimpleStringNode sep;
    protected int lxDim;
    public SeparatorNode(TreeNode parent, Object val, Object tyVal,
			 int index, Canvas visual, String sep) :
	base(parent, val, tyVal, index, visual, 0) {
	this.sep = new SimpleStringNode(this, sep, "sep", 0, visual, 0);
    }
    public override bool IsHorzMode() {
	return ((TreeNode) val).IsHorzMode();
    }
    public override void Layout() {
	TreeNode tval = (TreeNode) val;

	tval.Layout();
	sep.Layout();
	
	lxDim = (tval.GetLastXDim() + sep.GetXDim());
	xDim  = Helper.Max(tval.GetXDim(), lxDim);
	yDim  = tval.GetYDim();
    }
    public override int GetLastXDim() {
	return lxDim;
    }
    public override void Draw(int x, int y) {
	TreeNode tval = (TreeNode) val;
	Helper.PrintLn((System.String) "Enter SeparatorNode draw");
	tval.Draw(x, y);
	sep.Draw((x + tval.GetLastXDim()), (y + yDim - 1));
    }
    public void ChangeNode(TreeNode node) {
	if (val is LabelNode) {
	    ((LabelNode) val).ChangeNode(node);
	}
	else {
	    val = node;
	}
    }
    public void ChangeSep(string sep) {
	this.sep = new SimpleStringNode(this, sep, "sep", 0, visual, 0);
    }
}

class LabelNode : TreeNode {
    protected SimpleStringNode label;
    protected int lxDim;
    public LabelNode(TreeNode parent, Object val, Object tyVal,
		     int index, Canvas visual, String label) :
	base(parent, val, tyVal, index, visual, 0) {
	this.label = new SimpleStringNode(this, label, "label", 0, visual, 0);
    }
    public override bool IsHorzMode() {
	return ((TreeNode) val).IsHorzMode();
    }
    public override void Layout() {
	TreeNode tval = (TreeNode) val;

	tval.Layout();
	label.Layout();
	int labXDim = label.GetXDim();

	lxDim = (tval.GetLastXDim() + labXDim);
	xDim  = (tval.GetXDim() + labXDim);
	yDim  = tval.GetYDim();
    }
    public override int GetLastXDim() {
	return lxDim;
    }
    public override void Draw(int x, int y) {
	label.Draw(x, y);
	((TreeNode) val).Draw(x + label.GetXDim(), y);
    }
    public void ChangeNode(TreeNode node) {
	val = node;
    }
}

class ContainerNode : TreeNode {
    protected int lxDim;
    protected SimpleStringNode obrace;
    protected SimpleStringNode cbrace;
    protected System.Collections.Hashtable items;
    protected int width;
    protected int maxWidth;
    protected bool horzMode;
    public ContainerNode(TreeNode parent, Object val, Object tyVal,
			 int index, Canvas visual, int depth) :
	base(parent, val, tyVal, index, visual, depth) {
	items      = new System.Collections.Hashtable();
	width      = 0;
	maxWidth   = 0;
    }
    bool CheckHorzMode() {
	for (int i = 1; i <= width; i++) {
	    if (!((TreeNode) items[i]).IsHorzMode()) {
		return false;
	    }
	}
	return true;
    }
    public override bool IsHorzMode() {
	return false;
    }
    void HorzLayout() {
	xDim = 0;
	yDim = 1;
	
	for (int i = 1; i <= width; i++) {
	    TreeNode node = (TreeNode) items[i];

	    node.Layout();
	    xDim += node.GetXDim();
	}
	lxDim = xDim;
    }
    void VertLayout() {
	TreeNode node = null;

	xDim = 0;
	yDim = 0;
	for (int i = 1; i <= width; i++) {
	    node = (TreeNode) items[i];
	    node.Layout();
	    xDim = Helper.Max(xDim, node.GetXDim());
	    yDim += node.GetYDim();
	}
	lxDim = node.GetLastXDim();
    }
    public override void Layout() {
	if (dazzle) {
	    horzMode = CheckHorzMode();
	    if (horzMode) {
		HorzLayout();
	    }
	    else {
		VertLayout();
	    }
	    dazzle = false;
	    dirty  = true;
	}
    }
    public override int GetLastXDim() {
	return lxDim;
    }
    public virtual void AdjustXYDim(int pref, int succ) {
	lxDim += succ;
	xDim   = Helper.Max(xDim, lxDim);
	lxDim += pref;
	xDim  += pref;
    }
    void HorzDraw(int x, int y) {
	for (int i = 1; i <= width; i++) {
	    TreeNode node = (TreeNode) items[i];

	    node.Draw(x, y);
	    x += node.GetXDim();
	}
    }
    void VertDraw(int x, int y) {
	for (int i = 1; i <= width; i++) {
	    TreeNode node = (TreeNode) items[i];

	    node.Draw(x, y);
	    y += node.GetYDim();
	}
    }
    public override void Draw(int x, int y) {
	if (horzMode) {
	    HorzDraw(x, y);
	}
	else {
	    VertDraw(x, y);
	}
	dirty = false;
    }
    public override void Undraw() {
	if (!dirty) {
	    for (int i = 1; i <= width; i++) {
		((TreeNode) items[i]).Undraw();
	    }
	}
    }
    public void Notify() {
	dazzle = true;
	if (parent != null) {
	    ((ContainerNode) parent).Notify();
	}
    }
}

class TupleNode : ContainerNode {
    public TupleNode(TreeNode parent, Object val, Object tyVal,
		     int index, Canvas visual, int depth) :
	base(parent, val, tyVal, index, visual, depth) {
	Object[] arr = (Object[]) val;

	maxWidth = arr.Length;
	width    = Helper.Min(maxWidth, Inspector.GetWidth());

	CreateBraces();
	PerformInsertion(1, arr, (Object[]) tyVal);
    }
    public virtual void CreateBraces() {
	obrace = new SimpleStringNode(this, "(", "string", 0, visual, 0);
	cbrace = new SimpleStringNode(this, ")", "string", 0, visual, 0);
    }
    public virtual TreeNode CreateChildNode(Object val, Object tyVal, int index, int depth) {
	return Inspector.CreateNode(this, val, tyVal, index, depth);
    }
    public virtual void PerformInsertion(int i, Object[] arr, Object[] tyVal) {
	int newDepth  = (depth + 1);
	TreeNode node = null;
	
	Helper.Print((System.String) "Enter Tuple CreateNodes: ");
	Helper.PrintLn(((Int32) width).ToString());

	for (; (i <= width); i++) {
	    int arrI = (i - 1);
	    
	    node = CreateChildNode(arr[arrI], tyVal[arrI], i, newDepth);
	    items.Add(i, new SeparatorNode(this, node, "sep", i, visual, ","));
	}
	if (width < maxWidth) {
	    width += 1;
	    items.Add(width, new StringNode(this, "...", "string", width, visual, newDepth));
	}
	else {
	    items[width] = node;
	}
    }
    public override void Layout() {
	if (dazzle) {
	    base.Layout();
	    obrace.Layout();
	    cbrace.Layout();
	    AdjustXYDim(obrace.GetXDim(), cbrace.GetXDim());
	}
    }
    public override void Draw(int x, int y) {
	obrace.Draw(x, y);
	base.Draw(x + obrace.GetXDim(), y);
	cbrace.Draw((x + lxDim - 1), (y + yDim - 1));
    }
    public override void Undraw() {
	if (!dirty) {
	    obrace.Undraw();
	    base.Undraw();
	    cbrace.Undraw();
	}
    }
    public override void Replace(TreeNode parent, Object val, Object tyVal,
				 int index, int depth) {
	if (index < width) {
	    TreeNode node = Inspector.CreateNode(parent, val, tyVal, index, depth);
	    ((SeparatorNode) items[index]).ChangeNode(node);
	}
	else {
	    items[index] = Inspector.CreateNode(parent, val, tyVal, index, depth);
	}
	dazzle = true;
	if (parent != null) {
	    ((ContainerNode) parent).Notify();
	}
    }
}

class RecordNode : TupleNode {
    public RecordNode(TreeNode parent, Object val, Object tyVal,
		      int index, Canvas visual, int depth) :
	base(parent, val, tyVal, index, visual, depth) {}
    public override void CreateBraces() {
	obrace = new SimpleStringNode(this, "{", "string", 0, visual, 0);
	cbrace = new SimpleStringNode(this, "}", "string", 0, visual, 0);
    }
    public override TreeNode CreateChildNode(Object val, Object tyVal, int index, int depth) {
	Object[] entry = ((Types.Tuple) tyVal).types;
	String str     = System.String.Concat(((Types.String) entry[0]).label, "=");
	TreeNode node  = Inspector.CreateNode(this, val, entry[1], index, depth);

	return new LabelNode(this, node, "sep", index, visual, str);
    }
    public override void Replace(TreeNode parent, Object val, Object tyVal,
				 int index, int depth) {
	TreeNode node = Inspector.CreateNode(parent, val, tyVal, index, depth);
	if (index < width) {
	    ((SeparatorNode) items[index]).ChangeNode(node);
	}
	else {
	    ((LabelNode) items[index]).ChangeNode(node);
	}
	dazzle = true;
	if (parent != null) {
	    ((ContainerNode) parent).Notify();
	}
    }
}

class ListNode : ContainerNode {
    protected bool showBraces;
    public ListNode(TreeNode parent, Object val, Object tyVal,
		    int index, Canvas visual, int depth) :
	base(parent, val, tyVal, index, visual, depth) {
	int maxAWidth = Inspector.GetWidth();

	maxWidth   = maxAWidth;
	width      = maxAWidth;
	obrace     = new SimpleStringNode(this, "[", "string", 0, visual, 0);
	cbrace     = new SimpleStringNode(this, "]", "string", 0, visual, 0);
	PerformInsertion(1, val, (Types.List) tyVal);
    }
    void Put(int i, Object node) {
	if (items.ContainsKey(i)) {
	    items[i] = node;
	}
	else {
	    items.Add(i, node);
	}
    }
    void PerformInsertion(int i, Object val, Types.List tyVal) {
	int newDepth  = (depth + 1);

	Helper.Print((System.String) "Enter List: PerformInsertion with");
	Helper.PrintLn((System.String) val.ToString());

	while (i <= width) {
	    if (val is Alice.Values.Transient) {
		val = ((Alice.Values.Transient) val).Deref();
	    }
	    if (val is Alice.Values.TagVal) {
		Alice.Values.TagVal tval = (Alice.Values.TagVal) val;

		Helper.Print((System.String) "List: PerformInsertion: Tag of TagVal is ");
		Helper.PrintLn(((Int32) tval.GetTag()).ToString());
		if (tval.GetTag() == 0) {
		    Object[] values = (Object[]) tval.Value;
		    TreeNode node = Inspector.CreateNode(this, values[0], tyVal.type, i, newDepth);

		    Put(i, new SeparatorNode(this, node, "sep", i, visual, "::"));
		    val = values[1];
		    i++;
		}
	    }
	    else {
		if (Alice.Builtins.Future_isFuture.StaticApply(val).Equals((Int32) 1)) {
		    width      = i;
		    showBraces = false;
		    if (Alice.Builtins.Future_isFailed.StaticApply(val).Equals((Int32) 1)) {
			Put(i, new SimpleStringNode(this, "<Failed>", tyVal, index,
						    visual, newDepth));
		    }
		    else {
			Put(i, Inspector.CreateNode(this, val, tyVal, i, newDepth));
		    }
		    break;
		}
		else {
		    if ((val is System.Int32) && (((Int32) val) == (Int32) 1)) {
			width      = (i - 1);
			showBraces = true;
			ChangeSeparator();
		    }
		    else {
			width      = i;
			showBraces = false;
			Put(i, Inspector.CreateNode(this, val, tyVal.type, i, newDepth));
		    }
		    break;
		}
	    }
	}
	Helper.PrintLn((System.String) "Leaving ListNode: PerformInsertion");
    }
    void ChangeSeparator() {
	for (int i = 1; i < width; i++) {
	    ((SeparatorNode) items[i]).ChangeSep(",");
	}
	((SeparatorNode) items[width]).ChangeSep(""); // Ugly Hack
    }
    public override void Layout() {
	if (dazzle) {
	    if (showBraces) {
		base.Layout();
		obrace.Layout();
		cbrace.Layout();
		AdjustXYDim(obrace.GetXDim(), cbrace.GetXDim());
	    }
	    else {
		base.Layout();
	    }
	}
    }
    public override void Draw(int x, int y) {
	if (showBraces) {
	    obrace.Draw(x, y);
	    base.Draw(x + obrace.GetXDim(), y);
	    cbrace.Draw((x + lxDim - 1), (y + yDim - 1));
	}
	else {
	    base.Draw(x, y);
	}
    }
    public override void Undraw() {
	if (!dirty) {
	    if (showBraces) {
		obrace.Undraw();
		base.Undraw();
		cbrace.Undraw();
	    }
	    else {
		base.Undraw();
	    }
	}
    }
    public override void Replace(TreeNode parent, Object val, Object tyVal,
				 int index, int depth) {

	if ((index < width) || (showBraces)) {
	    TreeNode node = Inspector.CreateNode(parent, val, tyVal, index, depth);
	    ((SeparatorNode) items[index]).ChangeNode(node);
	}
	else {
	    width = maxWidth;
	    PerformInsertion(index, val, (Types.List) this.tyVal);
	}
	dazzle = true;
	if (parent != null) {
	    ((ContainerNode) parent).Notify();
	}
			     
    }
}

class UnknownTypeException : System.Exception {
    Object value;
    public UnknownTypeException(Object val) {
	value = val;
    }
    public override System.String ToString() {
	return System.String.Concat("UnknownTypeException:", value.ToString());
    }
}				

namespace Types {
    public class Int {}
    public class Word {}
    public class Char {}
    public class String {
	public System.String label;
	public String(System.String label) {
	    this.label = label;
	}
    }
    public class Real {}
    public class Bool {}
    public class Fun {}
    public class Tuple {
	public Object[] types;
	public Tuple(Object[] types) {
	    this.types = types;
	}
    }
    public class Record {
	public Object[] types;
	public Record(Object[] types) {
	    this.types = types;
	}
    }
    public class List {
	public Object type;
	public List(Object type) {
	    this.type = type;
	}
    }
}

class Inspector {
    static int maxWidth  = 20;
    static int maxDepth  = 10;
    static Inspector ins = null;
    int wnd;
    int instance;
    static Canvas canvas;
    System.Collections.Hashtable items;
    int maxNum;
    int curY;

    public Inspector(Canvas canvas) {
	Inspector.canvas = canvas; // Hack
	Inspector.ins    = this; // Arg!!!
	this.items  = new System.Collections.Hashtable();
	this.maxNum = 0;
	this.curY   = 0;
    }
    public virtual int CallBack(int wnd, int msg, int w, int l) {
	Rect rect;
	
	switch (msg) {
	case WinMsg.Create:
	    rect = new Rect();
	    User.GetClientRect(wnd, rect);
	    canvas.CreateWindow(wnd, instance, rect.right, rect.bottom);
	    return 0;
	case WinMsg.Size:
	    canvas.AdjustSize(Helper.LoWord(l), Helper.HiWord(l));
	    return 0;
	case WinMsg.Destroy:
	    User.PostQuitMessage(0);
	    return 0;
	}
	
	return User.DefWindowProc(wnd, msg, w, l);
    }
    public virtual void Run() {
	int instance        = Kernel.GetModuleHandle(null);
	WndClassEx wndclass = new WndClassEx();
	Msg msg             = new Msg();
	String clName       = "Alice Inspecor";

	Win32.WinCB wcb = new Win32.WinCB(this.CallBack);
	System.Runtime.InteropServices.GCHandle root =
	    System.Runtime.InteropServices.GCHandle.Alloc(wcb);

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
	    throw new System.ApplicationException("Unable to Register Inspector");
	}

	wnd = User.CreateWindowEx(0, clName, clName, WinStyle.OverlappedWindow,
				  0, 0, 400, 400, 0, 0, instance, 0);

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
    public static int GetWidth() {
	return maxWidth;
    }
    public static int GetDepth() {
	return maxDepth;
    }
    public static TreeNode CreateNode(TreeNode parent, Object val,
				      Object tyVal, int index, int depth) {
	Helper.Print((System.String) "Inspector: CreateNode: val is ");
	Helper.PrintLn((System.String) val.ToString());
	Helper.Print((System.String) "Inspector: CreateNode: tyVal is ");
	Helper.PrintLn((System.String) tyVal.ToString());
	if (depth <= maxDepth) {
	    if (val is Alice.Values.Transient) {
		val = ((Alice.Values.Transient) val).Deref();
	    }
	    if (Alice.Builtins.Future_isFuture.StaticApply(val).Equals((Int32) 1)) {
		if (Alice.Builtins.Future_isFailed.StaticApply(val).Equals((Int32) 1)) {
		    return new SimpleStringNode(parent, "<Failed>", tyVal, index, canvas, depth);
		}
		else {
		    return new FutureNode(parent, val, tyVal, index, canvas, depth);
		}
	    }
	    else if (tyVal is Types.Int) {
		return new IntNode(parent, val, tyVal, index, canvas, depth);
	    }
	    else if (tyVal is Types.Word) {
		return new WordNode(parent, val, tyVal, index, canvas, depth);
	    }
	    else if (tyVal is Types.Char) {
		return new CharNode(parent, val, tyVal, index, canvas, depth);
	    }
	    else if (tyVal is Types.Bool) {
		return new BoolNode(parent, val, tyVal, index, canvas, depth);
	    }
	    else if (tyVal is Types.Fun) {
		return new FunNode(parent, val, tyVal, index, canvas, depth);
	    }
	    else if (tyVal is Types.String) {
		return new StringNode(parent, val, tyVal, index, canvas, depth);
	    }
	    else if (tyVal is Types.Real) {
		return new RealNode(parent, val, tyVal, index, canvas, depth);
	    }
	    else if (tyVal is Types.Tuple) {
		return new TupleNode(parent, val, ((Types.Tuple) tyVal).types,
				     index, canvas, depth);
	    }
	    else if (tyVal is Types.Record) {
		Helper.PrintLn((System.String) "Creating new Record Node");
		return new RecordNode(parent, val, ((Types.Record) tyVal).types,
				      index, canvas, depth);
	    }
	    else if (tyVal is Types.List) {
		return new ListNode(parent, val, tyVal, index, canvas, depth);
	    }
	    else {
		Helper.Print((System.String) "Inspector.CreateNode: Something went wrong:");
		Helper.PrintLn((System.String) tyVal.ToString());
		throw new UnknownTypeException(tyVal);
	    }
	}
	else {
	    return new StringNode(parent, "...", "string", index, canvas, depth);
	}
    }
    public static Inspector GetInspector() {
	return Inspector.ins;
    }
    public virtual void Replace(TreeNode parent, Object val, Object tyVal,
				int index, int depth) {
	items[index] = CreateNode(parent, val, tyVal, index, depth);
    }
    public void Update(FutureNode val) {
	lock (this) {
	    int i    = val.GetRootIndex();
	    int newY = 0;

	    for (int k = (i - 1); (k >= 1); k--) {
		TreeNode node = (TreeNode) items[k];
		newY += node.GetYDim();
	    }

	    val.Link();

	    for (; (i <= maxNum); i++) {
		TreeNode node = (TreeNode) items[i];

		node.Undraw();
		node.Layout();
		node.Draw(0, newY);
		newY += node.GetYDim();
	    }
	    curY = newY;
	    canvas.Flush();
	}
    }
    public void Inspect(Object val, Object tyVal) {
	lock (this) {
	    maxNum += 1;
	    TreeNode node = CreateNode(null, val, tyVal, maxNum, 0);
	    items.Add(maxNum, node);
	    node.Layout();
	    node.Draw(0, curY);
	    curY += node.GetYDim();
	    canvas.Flush();
	}
    }
}

class TranslateType {
    static int index = 0;
    static System.Collections.Hashtable table = new System.Collections.Hashtable();

    public static int NewType(Object tyVal) {
	index += 1;
	table.Add(index, tyVal);
	return index;
    }
    public static Object[] TranslateBlock(Object[] src) {
	int len      = src.Length;
	Object[] dst = new Object[len];
	
	for (int i = 0; i < len; i++) {
	    dst[i] = Translate(src[i]);
	}
	
	return dst;
    }
    public static Object Translate(Object tyVal) {
	tyVal = table[tyVal];

	Helper.Print((System.String) "Translate: tyVal is ");
	Helper.PrintLn((System.String) tyVal.ToString());
	if (tyVal is Types.Int) {
	    return tyVal;
	}
	else if (tyVal is Types.Word) {
	    return tyVal;
	}
	else if (tyVal is Types.Real) {
	    return tyVal;
	}
	else if (tyVal is Types.Char) {
	    return tyVal;
	}
	else if (tyVal is Types.Bool) {
	    return tyVal;
	}
	else if (tyVal is Types.String) {
	    return tyVal;
	}
	else if (tyVal is Types.Fun) {
	    return tyVal;
	}
	else if (tyVal is Types.Tuple) {
	    return new Types.Tuple(TranslateBlock(((Types.Tuple) tyVal).types));
	}
	else if (tyVal is Types.Record) {
	    return new Types.Record(TranslateBlock(((Types.Record) tyVal).types));
	}
	else if (tyVal is Types.List) {
	    return new Types.List(Translate(((Types.List) tyVal).type));
	}
	else {
	    Helper.Print((System.String) "Translate: unknown tyVal: ");
	    Helper.PrintLn((System.String) tyVal.ToString());
	    return tyVal;
	}
    }
}

class MakeBasicType : Alice.Values.Procedure {
    public Object StaticApply(Object obj) {
	String str   = (System.String) obj;
	Object value = null;

	Helper.PrintLn("Entered MakeBasicType");
	if (str.Equals("int")) {
	    value = TranslateType.NewType(new Types.Int());
	}
	else if (str.Equals("word")) {
	    value = TranslateType.NewType(new Types.Word());
	}
	else if (str.Equals("char")) {
	    value = TranslateType.NewType(new Types.Char());
	}
	else if (str.Equals("real")) {
	    value = TranslateType.NewType(new Types.Real());
	}
	else if (str.Equals("bool")) {
	    value = TranslateType.NewType(new Types.Bool());
	}
	else {
	    value = TranslateType.NewType(new Types.String((System.String) str));
	}
	Helper.PrintLn("Leaving MakeBasicType");
	return value;
    }
    public override Object Apply(Object obj) {
	return StaticApply(obj);
    }
}

public class MakeArrowType : Alice.Values.Procedure3 {
    public static Object StaticApply(Object a, Object b, Object c) {
	Object value = null;

	Helper.PrintLn("Entered MakeArrowType");
	value = TranslateType.NewType(new Types.Fun());
	Helper.PrintLn("Leaving MakeArrowType");
	return value;
    }
    public override Object Apply(Object a, Object b, Object c) {
	return StaticApply(a, b, c);
    }
}

class MakeListType : Alice.Values.Procedure {
    public static Object StaticApply(Object obj) {
	Object value = null;

	Helper.PrintLn("Entered MakeListType");
	value = TranslateType.NewType(new Types.List(obj));
	Helper.PrintLn("Leaving MakeListType");
	return value;
    }
    public override Object Apply(Object obj) {
	return StaticApply(obj);
    }
}

class MakeRecordType : Alice.Values.Procedure {
    public static Object StaticApply(Object obj) {
	Object value = null;

	Helper.PrintLn("Entered MakeRecordType");
	value = TranslateType.NewType(new Types.Record((Object[]) obj));
	Helper.PrintLn("Leaving MakeRecordType");
	return value;
    }
    public override Object Apply(Object obj) {
	return StaticApply(obj);
    }
}

class MakeTupleType : Alice.Values.Procedure {
    public static Object StaticApply(Object obj) {
	Object value = null;

	Helper.PrintLn("Entered MakeTupleType");
	value = TranslateType.NewType(new Types.Tuple((Object[]) obj));
	Helper.PrintLn("Leaving MakeTupleType");
	return value;
    }
    public override Object Apply(Object obj) {
	return StaticApply(obj);
    }
}

class InspectApply : Alice.Values.Procedure2 {
    public static Object StaticApply(Object val, Object ty) {
	Helper.PrintLn((System.String) "Entered Inspect Wrapper");
	Inspector.GetInspector().Inspect(val, TranslateType.Translate(ty));
	Helper.PrintLn((System.String) "Leaving Inspect Wrapper");
	return Alice.Prebound.unit;
    }
    public override Object Apply(Object val, Object ty) {
	return StaticApply(val, ty);
    }
}

public class Execute {
    public static Object Main(Object obj) {
	Canvas canvas  = new Canvas();
	Inspector insp = new Inspector(canvas);
	new Thread(new ThreadStart(insp.Run)).Start();

	return new object[7] {
	    null,                   //--** $ty
	    new MakeArrowType(),    // makeArrowType
	    new MakeBasicType(),    // makeBasicType
	    new MakeListType(),     // makeListType
	    new MakeRecordType(),   // makeRecordType
	    new MakeTupleType(),    // makeTupleType
	    new InspectApply()      // nativeInspect
	};
    }
}
