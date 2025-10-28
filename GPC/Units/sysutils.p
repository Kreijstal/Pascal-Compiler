unit SysUtils;

interface

type
    TDateTime = longint;
    NativeUInt = longint;
    UInt64 = longint;
    AnsiString = string;

procedure Sleep(milliseconds: integer);
function GetTickCount64: longint;
function MillisecondsBetween(startTick, endTick: longint): longint;
function Now: TDateTime;
function FormatDateTime(const Format: string; DateTime: TDateTime): string;
function IntToStr(Value: longint): string;

implementation

function FormatDateTime(const Format: string; DateTime: TDateTime): string; external;
function IntToStr(Value: longint): string; external;

procedure Sleep(milliseconds: integer);
begin
    asm
        movl %edi, %edi
        call gpc_sleep_ms
    end
end;

function GetTickCount64: longint;
var
    tick: longint;
begin
    asm
        call gpc_get_tick_count64
        movq %rax, -8(%rbp)
    end;
    GetTickCount64 := tick;
end;

function MillisecondsBetween(startTick, endTick: longint): longint;
begin
    if endTick >= startTick then
        MillisecondsBetween := endTick - startTick
    else
        MillisecondsBetween := startTick - endTick;
end;

function Now: TDateTime;
var
    value: TDateTime;
begin
    asm
        call gpc_now_ms
        movq %rax, -8(%rbp)
    end;
    Now := value;
end;

end.
