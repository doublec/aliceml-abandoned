using System;
using System.Runtime.InteropServices;
using Win32.Constants;
using Win32.Datatypes;

namespace Win32 {
    namespace Constants {
	public sealed class Brush {
	    public const int White  = 0;
	    public const int LtGray = 1;
	    public const int Gray   = 2;
	    public const int DkGray = 3;
	    public const int Black  = 4;
	    public const int Null   = 5;
	    public const int Hollow = Null;
	}
	public sealed class ClassStyle {
	    public const int VRedraw         = 0x0001;
	    public const int HRedraw         = 0x0002;
	    public const int KeyCvtWindow    = 0x0004;
	    public const int DblClks         = 0x0008;
	    public const int OwnDC           = 0x0020;
	    public const int ClassDC         = 0x0040;
	    public const int ParentDC        = 0x0080;
	    public const int NoKeyCvt        = 0x0100;
	    public const int NoClose         = 0x0200;
	    public const int SaveBits        = 0x0800;
	    public const int ByteAlignClient = 0x1000;
	    public const int ByteAlignWindow = 0x2000;
	    public const int GlobalClass     = 0x4000;
	    public const int IME             = 0x00010000;
	}
	public sealed class CreateWin {
	    public const int Default = u8000;
	    public const int u8000 = -2147483648;
	}
	public sealed class CursorID {
	    public const int Arrow       = 32512;
	    public const int IBeam       = 32513;
	    public const int Wait        = 32514;
	    public const int Cross       = 32515;
	    public const int UpArrow     = 32516;
	    public const int SizeNWSE    = 32642;
	    public const int SizeNESW    = 32643;
	    public const int SizeWE      = 32644;
	    public const int SizeNS      = 32645;
	    public const int SizeAll     = 32646;
	    public const int No          = 32648;
	    public const int Hand        = 32649;
	    public const int AppStarting = 32650;
	    public const int Help        = 32651;
	}
	public sealed class IconID {
	    public const int Application = 32512;
	    public const int Hand        = 32513;
	    public const int Question    = 32514;
	    public const int Exclamation = 32515;
	    public const int Asterisk    = 32516;
	    public const int WinLogo     = 32517;

	    public const int Warning     = Exclamation;
	    public const int Error       = Hand;
	    public const int Information = Asterisk;
	}
	public class ScrollBar {
	    public const int Horz          = 0;
	    public const int Vert          = 1;
	    public const int Ctl           = 2;
	    public const int Both          = 3;

	    public const int LineUp        = 0;
	    public const int LineLeft      = 0;
	    public const int LineDown      = 1;
	    public const int LineRight     = 1;
	    public const int PageUp        = 2;
	    public const int PageLeft      = 2;
	    public const int PageDown      = 3;
	    public const int PageRight     = 3;
	    public const int ThumbPosition = 4;
	    public const int ThumbTrack    = 5;
	    public const int Top           = 6;
	    public const int Left          = 6;
	    public const int Bottom        = 7;
	    public const int Right         = 7;
	    public const int EndScroll     = 8;
	}
	public class SI {
	    public const int Range    = 0x0001;
	    public const int Page     = 0x0002;
	    public const int Pos      = 0x0004;
	    public const int TrackPos = 0x0010;
	    public const int All      = Range | Page | Pos | TrackPos;
	}
	public class ScrollWindow {
	    public const int ScrollChildren = 0x0001; // Scroll children within *lprcScroll
	    public const int Invalidate     = 0x0002; // Invalidate after scrolling
	    public const int Erase          = 0x0004; // If SW_INVALIDATE, don't send WM_ERASEBACKGROUND
	    public const int SmoothScroll   = 0x0010; // Use smooth scrolling
	}

	public class SystemFont {
	    public const int OEMFixedFont = 10;
	    public const int NormalFont   = 13;
	    public const int FixedFont    = 16;
	}

	public sealed class ShowWindow {
	    public const int Hide            = 0;
	    public const int ShowNormal      = 1;
	    public const int Normal          = 1;
	    public const int ShowMinimized   = 2;
	    public const int ShowMaximized   = 3;
	    public const int Maximize        = 3;
	    public const int ShowNoActivate  = 4;
	    public const int Show            = 5;
	    public const int Minimize        = 6;
	    public const int ShowMinNoActive = 7;
	    public const int ShowNA          = 8;
	    public const int Restore         = 9;
	    public const int ShowDefault     = 10;
	    public const int ForceMinimize   = 11;
	}

	public enum StructFormatEnum {
	    Ansi    = 1,
	    Unicode = 2,
	    Auto    = 3,
	}

	public sealed class SystemMetric {
	    public const int CXScreen             = 0;
	    public const int CYScreen             = 1;
	    public const int CXVScroll            = 2;
	    public const int CYHScroll            = 3;
	    public const int CYCaption            = 4;
	    public const int CXBorder             = 5;
	    public const int CYBorder             = 6;
	    public const int CXDlgFrame           = 7;
	    public const int CYDlgFrame           = 8;
	    public const int CYVThumb             = 9;
	    public const int CXHThumb             = 10;
	    public const int CXIcon               = 11;
	    public const int CYIcon               = 12;
	    public const int CXCursor             = 13;
	    public const int CYCursor             = 14;
	    public const int CYMenu               = 15;
	    public const int CXFullScreen         = 16;
	    public const int CYFullScreen         = 17;
	    public const int CYKanjiWindow        = 18;
	    public const int MousePresent         = 19;
	    public const int CYVScroll            = 20;
	    public const int CXHScroll            = 21;
	    public const int Debug                = 22;
	    public const int SwapButton           = 23;
	    public const int Reserved1            = 24;
	    public const int Reserved2            = 25;
	    public const int Reserved3            = 26;
	    public const int Reserved4            = 27;
	    public const int CXMin                = 28;
	    public const int CYMin                = 29;
	    public const int CXSize               = 30;
	    public const int CYSize               = 31;
	    public const int CXFrame              = 32;
	    public const int CYFrame              = 33;
	    public const int CXMinTrack           = 34;
	    public const int CYMinTrack           = 35;
	    public const int CXDoubleClk          = 36;
	    public const int CYDoubleClk          = 37;
	    public const int CXIconSpacing        = 38;
	    public const int CYIconSpacing        = 39;
	    public const int MenuDropAlignment    = 40;
	    public const int PenWindows           = 41;
	    public const int DBCSEnabled          = 42;
	    public const int CMouseButtons        = 43;

	    public const int CXFixedFrame         = CXDlgFrame;  /* win40 name change */
	    public const int CYFixedFrame         = CYDlgFrame;  /* win40 name change */
	    public const int CXSizeFrame          = CXFrame;     /* win40 name change */
	    public const int CYSizeFrame          = CYFrame;     /* win40 name change */

	    public const int Secure               = 44;
	    public const int CXEdge               = 45;
	    public const int CYEdge               = 46;
	    public const int CXMinSpacing         = 47;
	    public const int CYMinSpacing         = 48;
	    public const int CXSmIcon             = 49;
	    public const int CYSmIcon             = 50;
	    public const int CYSmCaption          = 51;
	    public const int CXSmSize             = 52;
	    public const int CYSmSize             = 53;
	    public const int CXMenuSize           = 54;
	    public const int CYMenuSize           = 55;
	    public const int Arrange              = 56;
	    public const int CXMinimized          = 57;
	    public const int CYMinimized          = 58;
	    public const int CXMaxTrack           = 59;
	    public const int CYMaxTrack           = 60;
	    public const int CXMaximized          = 61;
	    public const int CYMaximized          = 62;
	    public const int Network              = 63;
	    public const int CleanBoot            = 67;
	    public const int CXDrag               = 68;
	    public const int CYDrag               = 69;

	    public const int ShowSounds           = 70;

	    public const int CXMenuCheck          = 71;   /* Use instead of GetMenuCheckMarkDimensions()! */
	    public const int CYMenuCheck          = 72;
	    public const int SlowMachine          = 73;
	    public const int MidEastEnabled       = 74;

	    public const int MouseWheelPresent    = 75;

	    public const int XVirtualScreen       = 76;
	    public const int YVirtualScreen       = 77;
	    public const int CXVirtualScreen      = 78;
	    public const int CYVirtualScreen      = 79;
	    public const int CMonitors            = 80;
	    public const int SameDisPlayformat    = 81;

	    public const int CMetrics             = 83;
	}

	public class TextAlign {
	    public const int NoUpdateCP     = 0;
	    public const int UpdateCP       = 1;

	    public const int Left           = 0;
	    public const int Right          = 2;
	    public const int Center         = 6;

	    public const int Top            = 0;
	    public const int Bottom         = 8;
	    public const int Baseline       = 24;

	    public const int RTLReading     = 256;
	    public const int Mask       = (Baseline+Center+UpdateCP+RTLReading);
	}
	public sealed class WinDT {
	    public const int Top            = 0x00000000;
	    public const int Left           = 0x00000000;
	    public const int Center         = 0x00000001;
	    public const int Right          = 0x00000002;
	    public const int VCenter        = 0x00000004;
	    public const int Bottom         = 0x00000008;
	    public const int WordBreak      = 0x00000010;
	    public const int SingleLine     = 0x00000020;
	    public const int ExpandTabs     = 0x00000040;
	    public const int TabStop        = 0x00000080;
	    public const int NoClip         = 0x00000100;
	    public const int ExternalLeading= 0x00000200;
	    public const int CalcRect       = 0x00000400;
	    public const int NoPrefix       = 0x00000800;
	    public const int Internal       = 0x00001000;

	    public const int EditControl    = 0x00002000;
	    public const int Path_Ellipsis  = 0x00004000;
	    public const int End_Ellipsis   = 0x00008000;
	    public const int ModifyString   = 0x00010000;
	    public const int RtlReading     = 0x00020000;
	    public const int Word_Ellipsis  = 0x00040000;
	}

	public sealed class WinMsg {
	    public const int Null      = 0x0000;
	    public const int Create    = 0x0001;
	    public const int Destroy   = 0x0002;
	    public const int Move      = 0x0003;
	    public const int Size      = 0x0005;
	    public const int Activate  = 0x0006;
	    /*
	     * Activate state values
	     */
	    public const int AState_Inactive    = 0;
	    public const int AState_Active      = 1;
	    public const int AState_ClickActive = 2;

	    public const int SetFocus  = 0x0007;
	    public const int KillFocus = 0x0008;
	    public const int Enable    = 0x000A;
	    public const int SetRedraw = 0x000B;
	    public const int SetText   = 0x000C;
	    public const int GetText   = 0x000D;
	    public const int GetTextLength	= 0x000E;
	    public const int Paint		= 0x000F;
	    public const int Close		= 0x0010;
	    public const int QueryEndSession = 0x0011;
	    public const int Quit      = 0x0012;
	    public const int QueryOpen = 0x0013;
	    public const int EraseBkgnd	= 0x0014;
	    public const int SysColorChange	= 0x0015;
	    public const int EndSession	= 0x0016;
	    public const int ShowWindow	= 0x0018;
	    public const int WinIniChange	= 0x001A;
	    public const int SettingChange	= WinIniChange;

	    public const int DevModeChange  = 0x001B;
	    public const int ActivateApp	= 0x001C;
	    public const int FontChange	= 0x001D;
	    public const int TimeChange	= 0x001E;
	    public const int CancelMode	= 0x001F;
	    public const int SetCursor	= 0x0020;
	    public const int MouseActivate	= 0x0021;
	    public const int ChildActivate	= 0x0022;
	    public const int QueueSync	= 0x0023;
	    public const int GetMinMaxInfo	= 0x0024;

	    public const int PaintIcon                  = 0x0026;
	    public const int IconEraseBkgnd             = 0x0027;
	    public const int NextDlgCtl                 = 0x0028;
	    public const int SpoolerStatus              = 0x002A;
	    public const int DrawItem                   = 0x002B;
	    public const int MeasureItem                = 0x002C;
	    public const int DeleteItem                 = 0x002D;
	    public const int VKeyToItem                 = 0x002E;
	    public const int CharToItem                 = 0x002F;
	    public const int SetFont                    = 0x0030;
	    public const int GetFont                    = 0x0031;
	    public const int SetHotkey                  = 0x0032;
	    public const int GetHotkey                  = 0x0033;
	    public const int QueryDragIcon              = 0x0037;
	    public const int CompareItem                = 0x0039;
	    public const int GetObject                  = 0x003D;
	    public const int Compacting                 = 0x0041;
	    public const int CommNotify                 = 0x0044;  /* no longer suported */
	    public const int WindowPosChanging          = 0x0046;
	    public const int WindowPosChanged           = 0x0047;
	    public const int Power                      = 0x0048;
	    public const int CopyData                   = 0x004A;
	    public const int CancelJournal              = 0x004B;
	    public const int Notify                     = 0x004E;
	    public const int InputLangChangeRequest     = 0x0050;
	    public const int InputLangChange            = 0x0051;
	    public const int TCard                      = 0x0052;
	    public const int Help                       = 0x0053;
	    public const int UserChanged                = 0x0054;
	    public const int NotifyFormat               = 0x0055;
	    public const int ContextMenu                = 0x007B;
	    public const int StyleChanging              = 0x007C;
	    public const int StyleChanged               = 0x007D;
	    public const int DisplayChange              = 0x007E;
	    public const int GetIcon                    = 0x007F;
	    public const int SetIcon                    = 0x0080;
	    public const int NcCreate                   = 0x0081;
	    public const int NcDestroy                  = 0x0082;
	    public const int NcCalcSize                 = 0x0083;
	    public const int NcHitTest                  = 0x0084;
	    public const int NcPaint                    = 0x0085;
	    public const int NcActivate                 = 0x0086;
	    public const int GetDlgCode                 = 0x0087;
	    public const int SyncPaint                  = 0x0088;
	    public const int NcMouseMove                = 0x00A0;
	    public const int NclButtonDown              = 0x00A1;
	    public const int NclButtonUp                = 0x00A2;
	    public const int NclButtonDblClk            = 0x00A3;
	    public const int NcrButtonDown              = 0x00A4;
	    public const int NcrButtonUp                = 0x00A5;
	    public const int NcrButtonDblClk            = 0x00A6;
	    public const int NcmButtonDown              = 0x00A7;
	    public const int NcmButtonUp                = 0x00A8;
	    public const int NcmButtonDblClk            = 0x00A9;
	    public const int KeyFirst                   = 0x0100;
	    public const int KeyDown                    = 0x0100;
	    public const int KeyUp                      = 0x0101;
	    public const int Char                       = 0x0102;
	    public const int DeadChar                   = 0x0103;
	    public const int SysKeyDown                 = 0x0104;
	    public const int SysKeyUp                   = 0x0105;
	    public const int SysChar                    = 0x0106;
	    public const int SysDeadChar                = 0x0107;
	    public const int KeyLast                    = 0x0108;
	    public const int Ime_StartComposition       = 0x010D;
	    public const int Ime_EndComposition         = 0x010E;
	    public const int Ime_Composition            = 0x010F;
	    public const int Ime_KeyLast                = 0x010F;
	    public const int InitDialog                 = 0x0110;
	    public const int Command                    = 0x0111;
	    public const int SysCommand                 = 0x0112;
	    public const int Timer                      = 0x0113;
	    public const int HScroll                    = 0x0114;
	    public const int VScroll                    = 0x0115;
	    public const int InitMenu                   = 0x0116;
	    public const int InitMenuPopup              = 0x0117;
	    public const int MenuSelect                 = 0x011F;
	    public const int MenuChar                   = 0x0120;
	    public const int EnterIdle                  = 0x0121;
	    public const int MenuRButtonUp              = 0x0122;
	    public const int MenuDrag                   = 0x0123;
	    public const int MenuGetObject              = 0x0124;
	    public const int UnInitMenuPopup            = 0x0125;
	    public const int MenuCommand                = 0x0126;
	    public const int CtlColorMsgBox             = 0x0132;
	    public const int CtlColorEdit               = 0x0133;
	    public const int CtlColorListBox            = 0x0134;
	    public const int CtlColorBtn                = 0x0135;
	    public const int CtlColorDlg                = 0x0136;
	    public const int CtlColorScrollBar          = 0x0137;
	    public const int CtlColorStatic             = 0x0138;
	    public const int MouseFirst                 = 0x0200;
	    public const int MouseMove                  = 0x0200;
	    public const int LButtonDown                = 0x0201;
	    public const int LButtonUp                  = 0x0202;
	    public const int LButtonDblClk              = 0x0203;
	    public const int RButtonDown                = 0x0204;
	    public const int RButtonUp                  = 0x0205;
	    public const int RButtonDblClk              = 0x0206;
	    public const int MButtonDown                = 0x0207;
	    public const int MButtonUp                  = 0x0208;
	    public const int MButtonDblClk              = 0x0209;
	    public const int MouseWheel                 = 0x020A;
	    public const int MouseLast                  = 0x020A;
	    public const int ParentNotify               = 0x0210;
	    public const int EnterMenuLoop              = 0x0211;
	    public const int ExitMenuLoop               = 0x0212;
	    public const int NextMenu                   = 0x0213;
	    public const int Sizing                     = 0x0214;
	    public const int CaptureChanged             = 0x0215;
	    public const int Moving                     = 0x0216;
	    public const int PowerBroadcast             = 0x0218;      // r_winuser pbt
	    public const int DeviceChange               = 0x0219;
	    public const int MdiCreate                  = 0x0220;
	    public const int MdiDestroy                 = 0x0221;
	    public const int MdiActivate                = 0x0222;
	    public const int MdiRestore                 = 0x0223;
	    public const int MdiNext                    = 0x0224;
	    public const int MdiMaximize                = 0x0225;
	    public const int MdiTile                    = 0x0226;
	    public const int MdiCascade                 = 0x0227;
	    public const int MdiIconArrange             = 0x0228;
	    public const int MdiGetActive               = 0x0229;
	    public const int MdiSetMenu                 = 0x0230;
	    public const int EnterSizeMove              = 0x0231;
	    public const int ExitSizeMove               = 0x0232;
	    public const int DropFiles                  = 0x0233;
	    public const int MdirefreshMenu             = 0x0234;
	    public const int Ime_SetContext             = 0x0281;
	    public const int Ime_Notify                 = 0x0282;
	    public const int Ime_Control                = 0x0283;
	    public const int Ime_CompositionFull        = 0x0284;
	    public const int Ime_Select                 = 0x0285;
	    public const int Ime_Char                   = 0x0286;
	    public const int Ime_Request                = 0x0288;
	    public const int Ime_KeyDown                = 0x0290;
	    public const int Ime_KeyUp                  = 0x0291;
	    public const int MouseHover                 = 0x02A1;
	    public const int MouseLeave                 = 0x02A3;
	    public const int Cut                        = 0x0300;
	    public const int Copy                       = 0x0301;
	    public const int Paste                      = 0x0302;
	    public const int Clear                      = 0x0303;
	    public const int Undo                       = 0x0304;
	    public const int RenderFormat               = 0x0305;
	    public const int RenderAllFormats           = 0x0306;
	    public const int DestroyClipboard           = 0x0307;
	    public const int DrawClipboard              = 0x0308;
	    public const int PaintClipboard             = 0x0309;
	    public const int VscrollClipboard           = 0x030A;
	    public const int SizeClipboard              = 0x030B;
	    public const int AskCbFormatName            = 0x030C;
	    public const int ChangeCbChain              = 0x030D;
	    public const int HscrollClipboard           = 0x030E;
	    public const int QueryNewPalette            = 0x030F;
	    public const int PaletteIsChanging          = 0x0310;
	    public const int PaletteChanged             = 0x0311;
	    public const int Hotkey                     = 0x0312;
	    public const int Print                      = 0x0317;
	    public const int PrintClient                = 0x0318;
	    public const int HandheldFirst              = 0x0358;
	    public const int HandheldLast               = 0x035F;
	    public const int AfxFirst                   = 0x0360;
	    public const int AfxLast                    = 0x037F;
	    public const int PenwinFirst                = 0x0380;
	    public const int PenwinLast                 = 0x038F;
	    public const int App                        = 0x8000;
	    public const int User                       = 0x0400;
	}

	public class WinSound {
	    public const int Sync          = 0x0000; // play synchronously (default)
	    public const int Async         = 0x0001; // play asynchronously
	    public const int NoDefault     = 0x0002; // silence (!default) if sound not found
	    public const int Memory        = 0x0004; // pszSound points to a memory file
	    public const int Loop          = 0x0008; // loop the sound until next sndPlaySound
	    public const int NoStop        = 0x0010; // don't stop any currently playing sound

	    public const int NoWait        = 0x00002000; // don't wait if the driver is busy
	    public const int Alias         = 0x00010000; // name is a registry alias
	    public const int Alias_ID      = 0x00110000; // alias is a predefined ID
	    public const int Filename      = 0x00020000; // name is file name 
	    public const int Resource      = 0x00040004; // name is resource name or atom
	    public const int Purge         = 0x0040;     // purge non-static events for task
	    public const int Application   = 0x0080;    // look for application specific association
	}

	public sealed class WinStyle {
	    public const int Overlapped       = 0x00000000;
	    public const int PopUp            = CreateWin.u8000;
	    public const int Child            = 0x40000000;
	    public const int Minimize         = 0x20000000;
	    public const int Visible          = 0x10000000;
	    public const int Disabled         = 0x08000000;
	    public const int ClipSiblings     = 0x04000000;
	    public const int ClipChildren     = 0x02000000;
	    public const int Maximize         = 0x01000000;
	    public const int Caption          = 0x00C00000;
	    public const int Border           = 0x00800000;
	    public const int DlgFrame         = 0x00400000;
	    public const int VScroll          = 0x00200000;
	    public const int HScroll          = 0x00100000;
	    public const int SysMenu          = 0x00080000;
	    public const int ThickFrame       = 0x00040000;
	    public const int Group            = 0x00020000;
	    public const int TabStop          = 0x00010000;

	    public const int MinimizeBox      = 0x00020000;
	    public const int MaximizeBox      = 0x00010000;

	    public const int Tiled            = Overlapped;
	    public const int Iconic           = Minimize;
	    public const int SizeBox          = ThickFrame;
	    public const int TiledWindow      = OverlappedWindow;

	    /*
	     * Common Window Styles
	     */
	    public const int OverlappedWindow =
		(Overlapped | Caption | SysMenu | ThickFrame | MinimizeBox | MaximizeBox);

	    public const int PopUpWindow = (PopUp | Border | SysMenu);
	    public const int ChildWindow = Child;

	    /*
	     * Extended Window Styles
	     */
	    public const int DlgModalFrame     = 0x00000001;
	    public const int NoParentNotify    = 0x00000004;
	    public const int Topmost           = 0x00000008;
	    public const int AcceptFiles       = 0x00000010;
	    public const int Transparent       = 0x00000020;
	    public const int MDIChild          = 0x00000040;
	    public const int ToolWindow        = 0x00000080;
	    public const int WindowEdge        = 0x00000100;
	    public const int ClientEdge        = 0x00000200;
	    public const int ContextHelp       = 0x00000400;

	    public const int Right             = 0x00001000;
	    public const int Left              = 0x00000000;
	    public const int RTLReading        = 0x00002000;
	    public const int LTRReading        = 0x00000000;
	    public const int LeftScrollbar     = 0x00004000;
	    public const int RightScrollbar    = 0x00000000;

	    public const int ControlParent     = 0x00010000;
	    public const int StaticEdge        = 0x00020000;
	    public const int AppWindow         = 0x00040000;

	    public const int ExOverlappedWindow = (WindowEdge | ClientEdge);
	    public const int PaletteWindow = (WindowEdge | ToolWindow | Topmost);
	}
    }
    namespace Datatypes {
	[StructLayout(LayoutKind.Sequential)]
	public class Msg {
	    public int wnd;
	    public int message;
	    public int w;
	    public int l;
	    public int time;
	    public int x;
	    public int y;
	}
	[StructLayout(LayoutKind.Sequential)]
	public class PaintStruct {
	    public int dc;
	    public bool erase;
	    public Rect paint;
	    public bool restore;
	    public bool inc_update;
	    [MarshalAs(UnmanagedType.ByValArray, SizeConst=32)]
	    public byte[] reserved;
	}
	[StructLayout(LayoutKind.Sequential)]
	public struct Point {
	    public int x;
	    public int y;
	}
	[StructLayout(LayoutKind.Sequential)]
	public class Rect {
	    public int left;
	    public int top;
	    public int right;
	    public int bottom;
	}
	[StructLayout(LayoutKind.Sequential)]
	public class ScrollInfo {
	    public int cbSize;
	    public int fMask;
	    public int nMin;
	    public int nMax;
	    public int nPage;
	    public int nPos;
	    public int nTrackPos;
	}
	[StructLayout(LayoutKind.Sequential)]
	public class TextMetric {
	    public int Height;
	    public int Ascent;
	    public int Descent;
	    public int InternalLeading;
	    public int ExternalLeading;
	    public int AveCharWidth;
	    public int MaxCharWidth;
	    public int Weight;
	    public int Overhang;
	    public int DigitizedAspectX;
	    public int DigitizedAspectY;
	    public char FirstChar;
	    public char LastChar;
	    public char DefaultChar;
	    public char BreakChar;
	    public byte Italic;
	    public byte Underlined;
	    public byte StruckOut;
	    public byte PitchAndFamily;
	    public byte CharSet;
	}
	[StructLayout(LayoutKind.Sequential)]
	public class WndClassEx { // sizeof=48
	    public int cbSize;
	    public int style;
	    public WinCB lpfnWndProc;
	    public int cbClsExtra;
	    public int cbWndExtra;
	    public int hInstance;
	    public int hIcon;
	    public int hCursor;
	    public int hbrBackground;
	    public string lpszMenuName;
	    public string lpszClassName;
	    public int hIconSm;
	}
    }
    namespace API {
	public class Error {
	    [DllImport("KERNEL32", CharSet=CharSet.Auto, SetLastError=true)]
	    static extern int FormatMessage
		(
		 int dwFlags,                        // source and processing options
		 int lpSource,                       // pointer to  message source
		 int dwMessageId,                    // requested message identifier
		 int dwLanguageId,                   // language identifier for requested message
		 System.Text.StringBuilder lpBuffer, // pointer to message buffer
		 int nSize,                          // maximum size of message buffer
		 int Arguments                       // pointer to array of message inserts
		);
	    static int MAKELANGID(int p, int s) {
		return ((((short)(s)) << 10) | (short)(p));
	    }
	    const int FORMAT_MESSAGE_FROM_SYSTEM = 0x00001000;
	    const int FORMAT_MESSAGE_IGNORE_INSERTS = 0x00000200;
	    const int LANG_NEUTRAL = 0x00;
	    const int SUBLANG_DEFAULT = 0x01;

	    public static string GetSystemErrorMessage() {
		System.Text.StringBuilder msgbuf = new System.Text.StringBuilder(256);
		int e                            = System.Runtime.InteropServices.Marshal.GetLastWin32Error();

		FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
			      0,
			      e,
			      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
			      msgbuf,
			      msgbuf.Capacity,
			      0);
		return msgbuf.ToString();
	    }
	}
	public class GDI {
	    [DllImport("GDI32")]
	    public static extern int GetStockObject(int obj);
	    [DllImport("GDI32")]
	    public static extern int GetTextMetrics(int dc, [In, Out] TextMetric tm);
	    [DllImport("GDI32")]
	    public static extern int TextOut(int dc, int x, int y, string text, int len);
	    [DllImport("GDI32")]
	    public static extern int SetTextAlign(int dc, int mode);
	    [DllImport("GDI32")]
	    public static extern int SetTextColor(int dc, uint rgbcolor);
	    [DllImport("GDI32")]
	    public static extern int SelectObject(int dc, int font);
	}
	public class Kernel {
	    [DllImport("KERNEL32")]
	    public static extern int GetModuleHandle(string name);
	}
	public class User {
	    [DllImport("USER32")]
	    public static extern int LoadIcon(int instance, int id);

	    [DllImport("USER32")]
	    public static extern int LoadCursor(int instance, int id);

	    [DllImport("USER32")]
	    public static extern int RegisterClassExA([In, Out] WndClassEx w);

	    [DllImport("USER32", CharSet=CharSet.Auto, SetLastError=true)]
	    public static extern int CreateWindowEx
		(int dwExStyle, string lpClassName, string lpWindowName,
		 uint dwStyle, int X, int Y, int nWidth, int nHeight,
		 int hWndParent, int hMenu, int hInstance, int lpParam);

	    [DllImport("USER32")]
	    public static extern void ShowWindow(int wnd, int show);

	    [DllImport("USER32")]
	    public static extern void UpdateWindow(int wnd);

	    [DllImport("USER32")]
	    public static extern bool MoveWindow(int wnd, int x, int y,
						 int width, int height,
						 bool rp);

	    [DllImport("USER32")]
	    public static extern int GetMessage([In, Out] Msg m, int wnd,
						int min, int max);

	    [DllImport("USER32")]
	    public static extern int TranslateMessage([In, Out] Msg m);

	    [DllImport("USER32")]
	    public static extern int DispatchMessage([In, Out] Msg m);

	    [DllImport("USER32")]
	    public static extern int BeginPaint(int wnd,
						[In, Out] PaintStruct ps);

	    [DllImport("USER32")]
	    public static extern int EndPaint(int wnd,
					      [In, Out] PaintStruct ps);

	    [DllImport("USER32")]
	    public static extern void GetClientRect(int wnd,
						    [In, Out] Rect rect);

	    [DllImport("USER32")]
	    public static extern void DrawText(int dc, string text, int count,
					       Rect rect, int format);

	    [DllImport("USER32")]
	    public static extern void PostQuitMessage(int code);

	    [DllImport("USER32")]
	    public static extern int DefWindowProc(int wnd, int msg,
						   int w, int l);

	    [DllImport("USER32")]
	    public static extern int FillRect(int dc, Rect rect, int brush);

	    [DllImport("USER32", CharSet=CharSet.Auto)]
	    public static extern int MessageBox(int handle, string text,
						string caption, int type);

	    [DllImport("USER32")]
	    public static extern int GetSystemMetrics(int sm);

	    [DllImport("USER32")]
	    public static extern int GetDC(int wnd);

	    [DllImport("USER32")]
	    public static extern int ReleaseDC(int wnd, int dc);

	    [DllImport("USER32")]
	    public static extern int SetScrollRange(int wnd, int bar,
						    int minpos, int maxpos,
						    bool redraw);

	    [DllImport("USER32")]
	    public static extern int SetScrollPos(int wnd, int bar, int pos,
						  bool redraw);

	    [DllImport("USER32")]
	    public static extern int GetScrollPos(int wnd, int bar);

	    [DllImport("USER32")]
	    public static extern int SetScrollInfo(int wnd, int bar,
						   ScrollInfo si, bool redraw);

	    [DllImport("USER32")]
	    public static extern bool ShowScrollBar(int wnd, int bar,
						    bool view);

	    [DllImport("USER32")]
	    public static extern bool GetScrollInfo(int wnd, int bat,
						    [In, Out] ScrollInfo si);

	    [DllImport("USER32")]
	    public static extern int InvalidateRect(int wnd, Rect r,
						    bool erase);

	    [DllImport("USER32")]
	    public static extern int ScrollWindowEx(int wnd, int dx, int dy,
						    Rect scroll, Rect clip,
						    int rgn, Rect update,
						    int flags);

	    [DllImport("USER32")]
	    public static extern int GetUpdateRect(int wnd,
						   [In, Out] Rect rect,
						   bool erase);
	}
    }
    public delegate int WinCB(int wnd, int msg, int w, int l);
}
