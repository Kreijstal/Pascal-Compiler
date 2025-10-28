unit SysUtils;

interface

type
    TDateTime = longint;
    AnsiString = string;
    NativeUInt = longint;

procedure Sleep(milliseconds: integer);
function GetTickCount64: longint;
function MillisecondsBetween(startTick, endTick: longint): longint;
function Now: TDateTime;
function IntToStr(value: longint): AnsiString;
function FormatDateTime(const FormatStr: string; DateTime: TDateTime): AnsiString;

implementation

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
    tick: longint;
begin
    tick := GetTickCount64();
    Now := tick div 1000;
end;

function DigitToString(Value: longint): AnsiString;
var
    DigitValue: longint;
begin
    if Value < 0 then
        DigitValue := 0
    else if Value > 9 then
        DigitValue := 9
    else
        DigitValue := Value;

    if DigitValue = 0 then
        DigitToString := '0'
    else if DigitValue = 1 then
        DigitToString := '1'
    else if DigitValue = 2 then
        DigitToString := '2'
    else if DigitValue = 3 then
        DigitToString := '3'
    else if DigitValue = 4 then
        DigitToString := '4'
    else if DigitValue = 5 then
        DigitToString := '5'
    else if DigitValue = 6 then
        DigitToString := '6'
    else if DigitValue = 7 then
        DigitToString := '7'
    else if DigitValue = 8 then
        DigitToString := '8'
    else
        DigitToString := '9';
end;

function IntToStr(value: longint): AnsiString;
var
    remainder: longint;
    working: longint;
    prefix: AnsiString;
    digits: AnsiString;
begin
    if value = 0 then
    begin
        IntToStr := '0';
        exit;
    end;

    if value < 0 then
    begin
        working := -value;
        prefix := '-';
    end
    else
    begin
        working := value;
        prefix := '';
    end;

    digits := '';
    while working > 0 do
    begin
        remainder := working mod 10;
        digits := DigitToString(remainder) + digits;
        working := working div 10;
    end;

    IntToStr := prefix + digits;
end;

function FormatDateTime(const FormatStr: string; DateTime: TDateTime): AnsiString;
begin
    FormatDateTime := '00:00.000';
end;

end.
