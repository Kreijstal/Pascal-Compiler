unit SysUtils;

interface

uses
    ctypes;

type
    TDateTime = longint;
    AnsiString = string;
    NativeUInt = cuint64;
    Uint64 = cuint64;

procedure Sleep(milliseconds: integer);
function GetTickCount64: longint;
function MillisecondsBetween(startTick, endTick: longint): longint;
function Now: TDateTime;
function IntToStr(value: longint): AnsiString;
function StrToInt(const S: AnsiString): longint;
function UpperCase(const S: AnsiString): AnsiString;
function LowerCase(const S: AnsiString): AnsiString;
function Trim(const S: AnsiString): AnsiString;
function StringReplace(const S, OldPattern, NewPattern: AnsiString): AnsiString;
function Pos(Substr: AnsiString; S: AnsiString): integer;
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
    ticks: TDateTime;
begin
    asm
        call gpc_now
        movq %rax, -8(%rbp)
    end;
    Now := ticks;
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

    DigitToString := Chr(Ord('0') + DigitValue);
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

function StrToInt(const S: AnsiString): longint;
var
    i: integer;
    result: longint;
    sign: integer;
    digit: integer;
begin
    result := 0;
    sign := 1;
    i := 1;
    
    if Length(S) = 0 then
    begin
        StrToInt := 0;
        exit;
    end;
    
    if S[1] = '-' then
    begin
        sign := -1;
        i := 2;
    end
    else if S[1] = '+' then
        i := 2;
    
    while i <= Length(S) do
    begin
        if (S[i] >= '0') and (S[i] <= '9') then
        begin
            digit := Ord(S[i]) - Ord('0');
            result := result * 10 + digit;
        end
        else
        begin
            StrToInt := 0;
            exit;
        end;
        i := i + 1;
    end;
    
    StrToInt := result * sign;
end;

function UpperCase(const S: AnsiString): AnsiString;
var
    i: integer;
    result: AnsiString;
begin
    result := '';
    for i := 1 to Length(S) do
    begin
        if (S[i] >= 'a') and (S[i] <= 'z') then
            result := result + Chr(Ord(S[i]) - 32)
        else
            result := result + S[i];
    end;
    UpperCase := result;
end;

function LowerCase(const S: AnsiString): AnsiString;
var
    i: integer;
    result: AnsiString;
begin
    result := '';
    for i := 1 to Length(S) do
    begin
        if (S[i] >= 'A') and (S[i] <= 'Z') then
            result := result + Chr(Ord(S[i]) + 32)
        else
            result := result + S[i];
    end;
    LowerCase := result;
end;

function Trim(const S: AnsiString): AnsiString;
var
    start, finish: integer;
begin
    start := 1;
    while (start <= Length(S)) and (S[start] = ' ') do
        start := start + 1;
    
    finish := Length(S);
    while (finish >= start) and (S[finish] = ' ') do
        finish := finish - 1;
    
    if start > finish then
        Trim := ''
    else
        Trim := Copy(S, start, finish - start + 1);
end;

function StringReplace(const S, OldPattern, NewPattern: AnsiString): AnsiString;
var
    i: integer;
    result: AnsiString;
    temp: AnsiString;
begin
    result := '';
    i := 1;
    
    while i <= Length(S) do
    begin
        if Copy(S, i, Length(OldPattern)) = OldPattern then
        begin
            result := result + NewPattern;
            i := i + Length(OldPattern);
        end
        else
        begin
            result := result + S[i];
            i := i + 1;
        end;
    end;
    
    StringReplace := result;
end;

function Pos(Substr: AnsiString; S: AnsiString): integer;
var
    i, j: integer;
    found: boolean;
begin
    if Length(Substr) = 0 then
    begin
        Pos := 0;
        exit;
    end;
    
    for i := 1 to Length(S) - Length(Substr) + 1 do
    begin
        found := true;
        for j := 1 to Length(Substr) do
        begin
            if S[i + j - 1] <> Substr[j] then
            begin
                found := false;
                break;
            end;
        end;
        if found then
        begin
            Pos := i;
            exit;
        end;
    end;
    
    Pos := 0;
end;

function FormatDateTime(const FormatStr: string; DateTime: TDateTime): AnsiString;
var
    resultPtr: AnsiString;
begin
    asm
        call gpc_format_datetime
        movq %rax, -8(%rbp)
    end;
    FormatDateTime := resultPtr;
end;

end.
