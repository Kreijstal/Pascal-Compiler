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
    stamp: TDateTime;
begin
    asm
        call gpc_now
        movq %rax, -8(%rbp)
    end;
    Now := stamp;
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

function FormatDateTime(const FormatStr: string; DateTime: TDateTime): AnsiString;
    function PadNumber(Value: longint; Width: integer): AnsiString;
    begin
        PadNumber := IntToStr(Value);
        while Length(PadNumber) < Width do
            PadNumber := '0' + PadNumber;
    end;

var
    i: integer;
    run: integer;
    ch: char;
    hours, minutes, seconds, millis: longint;
    remaining: longint;
    token: AnsiString;
begin
    remaining := DateTime;
    if remaining < 0 then
        remaining := -remaining;

    hours := (remaining div 3600000) mod 24;
    minutes := (remaining div 60000) mod 60;
    seconds := (remaining div 1000) mod 60;
    millis := remaining mod 1000;

    FormatDateTime := '';
    i := 1;
    while i <= Length(FormatStr) do
    begin
        ch := FormatStr[i];
        run := 1;
        while (i + run <= Length(FormatStr)) and
              (UpCase(FormatStr[i + run]) = UpCase(ch)) do
            Inc(run);

        case UpCase(ch) of
            'H': FormatDateTime := FormatDateTime + PadNumber(hours, run);
            'N': FormatDateTime := FormatDateTime + PadNumber(minutes, run);
            'S': FormatDateTime := FormatDateTime + PadNumber(seconds, run);
            'Z': begin
                    if run < 3 then
                        run := 3;
                    if run > 6 then
                        run := 6;
                    FormatDateTime := FormatDateTime + PadNumber(millis, run);
                 end;
        else
            token := Copy(FormatStr, i, run);
            FormatDateTime := FormatDateTime + token;
        end;

        Inc(i, run);
    end;
end;

end.
