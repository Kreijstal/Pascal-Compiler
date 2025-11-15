unit Windows;

interface

uses
    ctypes;

{-------------------------------------------------------------------------}
{ Basic Win32 types and records (stubbed for compilation)                  }
{-------------------------------------------------------------------------}

type
    { Pointer-sized integers }
    ptrint  = cintptr;
    ptruint = cuintptr;

    { Common Win32 integral types }
    BOOL    = cint32;
    WordBool = cint32;
    UINT    = cuint32;
    DWORD   = cuint32;
    LONG    = clong;
    ULONG   = culong;
    WPARAM  = cuintptr;
    LPARAM  = cintptr;
    LRESULT = cintptr;
    COLORREF = cuint32;

    { Opaque handle types }
    HWND    = pointer;
    HDC     = pointer;
    HBRUSH  = pointer;
    HRGN    = pointer;
    HGDIOBJ = pointer;
    HMENU   = pointer;
    HINST   = pointer;
    HICON   = pointer;
    HCURSOR = pointer;
    HFONT   = pointer;

    { String pointer }
    pchar   = ^char;

    { Basic structs }
    TPOINT = record
        X: cint32;
        Y: cint32;
    end;
    PPOINT = ^TPOINT;

    TRECT = record
        Left:   cint32;
        Top:    cint32;
        Right:  cint32;
        Bottom: cint32;
    end;
    PRECT = ^TRECT;

    TSIZE = record
        cx: cint32;
        cy: cint32;
    end;

    TPAINTSTRUCT = record
        hdc: HDC;
        fErase: BOOL;
        rcPaint: TRECT;
        fRestore: BOOL;
        fIncUpdate: BOOL;
        rgbReserved1: array[0..31] of cuint8;
    end;

    TSCROLLINFO = record
        cbSize: UINT;
        fMask: UINT;
        nMin:  cint32;
        nMax:  cint32;
        nPage: UINT;
        nPos:  cint32;
        nTrackPos: cint32;
    end;

    TWndClassEx = record
        cbSize:        UINT;
        style:         UINT;
        lpfnWndProc:   pointer;      { WNDPROC }
        cbClsExtra:    cint32;
        cbWndExtra:    cint32;
        hInstance:     HINST;
        hIcon:         HICON;
        hCursor:       HCURSOR;
        hbrBackground: HBRUSH;
        lpszMenuName:  pchar;
        lpszClassName: pchar;
        hIconSm:       HICON;
    end;

    TMsg = record
        hwnd: HWND;
        message: UINT;
        wParam: WPARAM;
        lParam: LPARAM;
        time: DWORD;
        pt: TPOINT;
    end;
    PMSG = ^TMsg;

    PINTEGER = ^cint32;

{-------------------------------------------------------------------------}
{ Headless Win32 API wrappers via GPC runtime (interface)                  }
{-------------------------------------------------------------------------}

function gpc_get_tick_count64: culonglong; external name 'gpc_get_tick_count64';
procedure gpc_sleep_ms(milliseconds: cint32); external name 'gpc_sleep_ms';
function gpc_query_performance_counter: culonglong; external name 'gpc_query_performance_counter';
function gpc_query_performance_frequency: culonglong; external name 'gpc_query_performance_frequency';
function gpc_is_debugger_present: BOOL; external name 'gpc_is_debugger_present';

function GetTickCount64: culonglong;
procedure Sleep(milliseconds: DWORD);
function QueryPerformanceCounter(out value: culonglong): BOOL;
function QueryPerformanceFrequency(out freq: culonglong): BOOL;
function IsDebuggerPresent: BOOL;

{-------------------------------------------------------------------------}
{ Common constants (minimal subset needed by examples)                     }
{-------------------------------------------------------------------------}

const
    { Window styles }
    WS_OVERLAPPED      = $00000000;
    WS_VISIBLE         = $10000000;
    WS_SYSMENU         = $00080000;
    WS_MINIMIZEBOX     = $00020000;
    WS_CLIPSIBLINGS    = $04000000;
    WS_CLIPCHILDREN    = $02000000;
    WS_OVERLAPPEDWINDOW = $00CF0000;

    { Class styles }
    CS_BYTEALIGNCLIENT = $1000;

    { Brush / system colors }
    COLOR_WINDOW       = 5;

    { Message IDs }
    WM_NULL            = $0000;
    WM_CREATE          = $0001;
    WM_DESTROY         = $0002;
    WM_SIZE            = $0005;
    WM_SETFOCUS        = $0007;
    WM_ERASEBKGND      = $0014;
    WM_COMMAND         = $0111;
    WM_VSCROLL         = $0115;
    WM_CTLCOLORSCROLLBAR = $0137;
    WM_PAINT           = $000F;
    WM_INITDIALOG      = $0110;

    { Virtual keys }
    VK_TAB             = $09;
    VK_SHIFT           = $10;

    { Dialog IDs }
    IDOK               = 1;
    IDCANCEL           = 2;

    { Get/SetWindowLong indexes }
    GWL_WNDPROC        = -4;
    GWL_ID             = -12;

    { Scroll info flags / bars / actions }
    SIF_RANGE          = $0001;
    SIF_PAGE           = $0002;
    SIF_POS            = $0004;
    SB_CTL             = 2;
    SB_TOP             = 6;
    SB_BOTTOM          = 7;
    SB_LINEUP          = 0;
    SB_LINEDOWN        = 1;
    SB_PAGEUP          = 2;
    SB_PAGEDOWN        = 3;
    SB_THUMBPOSITION   = 4;
    SB_THUMBTRACK      = 5;

    { Text alignment / output }
    TA_LEFT            = 0;
    TA_TOP             = 0;
    TA_CENTER          = 6;
    TA_BOTTOM          = 8;
    ETO_CLIPPED        = $0004;
    ETO_OPAQUE         = $0002;

    { Image / LoadImage }
    IMAGE_ICON         = 1;
    LR_DEFAULTCOLOR    = $0000;

    { GetClassLong/Background index }
    GCL_HBRBACKGROUND  = -10;

{-------------------------------------------------------------------------}
{ Helper intrinsics                                                         }
{-------------------------------------------------------------------------}

function LOWORD(v: ptruint): cuint16; inline;
begin
    LOWORD := cuint16(v and $FFFF);
end;

function HIWORD(v: ptruint): cuint16; inline;
begin
    HIWORD := cuint16((v shr 16) and $FFFF);
end;

procedure ZeroMemory(p: pointer; size: cuint32); inline;
begin
    { Stub for compilation; no-op }
end;

{-------------------------------------------------------------------------}
{ Minimal host helpers (existing)                                           }
{-------------------------------------------------------------------------}

function GetHostName: string;
function GetDomainName: string;

implementation

function gpc_windows_get_hostname_string: string; external;
function gpc_windows_get_domainname_string: string; external;
function gpc_get_tick_count64: culonglong; external;
procedure gpc_sleep_ms(milliseconds: cint32); external;
function gpc_query_performance_counter: culonglong; external;
function gpc_query_performance_frequency: culonglong; external;
function gpc_is_debugger_present: BOOL; external;

function GetHostName: string;
begin
    GetHostName := gpc_windows_get_hostname_string;
end;

function GetDomainName: string;
begin
    GetDomainName := gpc_windows_get_domainname_string;
end;

function GetTickCount64: culonglong;
begin
    GetTickCount64 := gpc_get_tick_count64;
end;

procedure Sleep(milliseconds: DWORD); external name 'gpc_sleep_ms';

function QueryPerformanceCounter(out value: culonglong): BOOL;
var v: culonglong;
begin
    v := gpc_query_performance_counter;
    value := v;
    if v <> 0 then
      QueryPerformanceCounter := 1
    else
      QueryPerformanceCounter := 0;
end;

function QueryPerformanceFrequency(out freq: culonglong): BOOL;
var f: culonglong;
begin
    f := gpc_query_performance_frequency;
    freq := f;
    if f <> 0 then
      QueryPerformanceFrequency := 1
    else
      QueryPerformanceFrequency := 0;
end;

function IsDebuggerPresent: BOOL;
begin
    IsDebuggerPresent := gpc_is_debugger_present;
end;

end.
