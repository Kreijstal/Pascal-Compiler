unit StringUtils;

interface

uses
    SysUtils;

type
    TString = string;
    CharSet = set of char;
    PString = ^string;

var
    CharactersTrue: CharSet = ['Y', 'y'];
    CharactersFalse: CharSet = ['N', 'n'];

procedure AppendStr(var s: string; const Source: string);
procedure StrCut(var s: string; MaxLength: Integer);
function StrCount(const SubStr: string; s: string): Integer;
function StrReplace(const s, Source, Dest: string): TString;
function Char2Boolean(ch: Char; var Dest: Boolean): Boolean;
function Char2Digit(ch: Char): Integer;
function QuoteStringEscape(const s: string; EscapeChar: Char;
  QuoteHigh: Boolean): TString;
function QuoteString(const s: string): TString;
function QuoteEnum(const s: string): TString;
function UnQuoteString(var s: string): Boolean;
function UnQPString(var s: string): Boolean;
function ShellQuoteString(const s: string): TString;
function ExpandTabs(var s: string; TabSize: Integer): Boolean;
function ExpandCEscapeSequences(const s: string; RemoveQuoteChars,
  AllowOctal: Boolean): TString;
procedure StrSkipSpaces(const s: string; var i: Integer);
function StrReadQuoted(const s: string; var i: Integer; var Dest: string): Boolean;
function StrReadDelimited(const s: string; var i: Integer; var Dest: string;
  Delimiter: Char): Boolean;
function StrReadWord(const s: string; var i: Integer; var Dest: string): Boolean;
function StrReadConst(const s: string; var i: Integer; const Expected: string): Boolean;
function StrReadComma(const s: string; var i: Integer): Boolean;
function StrReadInt(const s: string; var i: Integer; var Dest: Integer): Boolean;
function StrReadReal(const s: string; var i: Integer; var Dest: Real): Boolean;
function StrReadBoolean(const s: string; var i: Integer; var Dest: Boolean): Boolean;
function StrReadEnum(const s: string; var i: Integer; var Dest: Integer;
  const IDs: array of PString): Boolean;

implementation

function HexDigit(Value: Integer): Char;
const
    Digits: array[0..15] of Char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
begin
    if Value < 0 then
        Value := 0;
    if Value > 15 then
        Value := 15;
    HexDigit := Digits[Value];
end;

function NeedsQuotedPrintable(ch, EscapeChar: Char; QuoteHigh: Boolean): Boolean;
begin
    if ch = '=' then
        NeedsQuotedPrintable := True
    else if (EscapeChar <> #0) and (ch = EscapeChar) then
        NeedsQuotedPrintable := True
    else if (Ord(ch) < 32) then
        NeedsQuotedPrintable := True
    else if QuoteHigh and (Ord(ch) > 126) then
        NeedsQuotedPrintable := True
    else
        NeedsQuotedPrintable := False;
end;

procedure AppendStr(var s: string; const Source: string);
var
    base: string;
begin
    base := s;
    s := base + Source;
end;

procedure StrCut(var s: string; MaxLength: Integer);
begin
    if MaxLength < 0 then
        MaxLength := 0;
    if Length(s) > MaxLength then
        SetLength(s, MaxLength);
end;

function StrCount(const SubStr: string; s: string): Integer;
var
    idx, lenSub, limit: Integer;
begin
    if SubStr = '' then
    begin
        StrCount := 0;
        exit;
    end;
    lenSub := Length(SubStr);
    StrCount := 0;
    idx := 1;
    limit := Length(s) - lenSub + 1;
    while idx <= limit do
    begin
        if Copy(s, idx, lenSub) = SubStr then
            Inc(StrCount);
        Inc(idx);
    end;
end;

function StrReplace(const s, Source, Dest: string): TString;
var
    search, remainder: TString;
    idx, lenSource: Integer;
begin
    if Source = '' then
    begin
        StrReplace := s;
        exit;
    end;
    remainder := s;
    search := '';
    lenSource := Length(Source);
    idx := Pos(Source, remainder);
    while idx > 0 do
    begin
        search := search + Copy(remainder, 1, idx - 1) + Dest;
        if idx + lenSource <= Length(remainder) then
            remainder := Copy(remainder, idx + lenSource, Length(remainder) - (idx + lenSource) + 1)
        else
            remainder := '';
        idx := Pos(Source, remainder);
    end;
    StrReplace := search + remainder;
end;

function CharInDefaultTrue(ch: Char): Boolean;
begin
    if (ch = 'Y') or (ch = 'y') then
        CharInDefaultTrue := True
    else
        CharInDefaultTrue := False;
end;

function CharInDefaultFalse(ch: Char): Boolean;
begin
    if (ch = 'N') or (ch = 'n') then
        CharInDefaultFalse := True
    else
        CharInDefaultFalse := False;
end;

function Char2Boolean(ch: Char; var Dest: Boolean): Boolean;
var
    upper: Char;
begin
    upper := ch;
    if (upper >= 'a') and (upper <= 'z') then
        upper := Chr(Ord(upper) - (Ord('a') - Ord('A')));

    if (upper = 'Y') then
    begin
        Dest := True;
        Char2Boolean := True;
    end
    else if (upper = 'N') then
    begin
        Dest := False;
        Char2Boolean := True;
    end
    else
    begin
        Dest := False;
        Char2Boolean := False;
    end;
end;

function Char2Digit(ch: Char): Integer;
begin
    if (ch >= '0') and (ch <= '9') then
        Char2Digit := Ord(ch) - Ord('0')
    else if (ch >= 'A') and (ch <= 'Z') then
        Char2Digit := Ord(ch) - Ord('A') + 10
    else if (ch >= 'a') and (ch <= 'z') then
        Char2Digit := Ord(ch) - Ord('a') + 10
    else
        Char2Digit := -1;
end;

function QuoteStringEscape(const s: string; EscapeChar: Char;
  QuoteHigh: Boolean): TString;
var
    i: Integer;
    value: Integer;
    output: TString;
begin
    output := '';
    for i := 1 to Length(s) do
    begin
        if NeedsQuotedPrintable(s[i], EscapeChar, QuoteHigh) then
        begin
            value := Ord(s[i]);
            output := output + '=' + HexDigit(value div 16) + HexDigit(value mod 16);
        end
        else
            output := output + s[i];
    end;
    QuoteStringEscape := output;
end;

function QuoteString(const s: string): TString;
begin
    QuoteString := '"' + QuoteStringEscape(s, '"', True) + '"';
end;

function QuoteEnum(const s: string): TString;
begin
    QuoteEnum := QuoteStringEscape(s, ',', False);
end;

function DecodeHex(const hi, lo: Char; var value: Integer): Boolean;
var
    highNibble, lowNibble: Integer;
begin
    highNibble := Char2Digit(hi);
    if highNibble < 0 then
    begin
        DecodeHex := False;
        exit;
    end;
    lowNibble := Char2Digit(lo);
    if lowNibble < 0 then
    begin
        DecodeHex := False;
        exit;
    end;
    value := (highNibble shl 4) + lowNibble;
    DecodeHex := True;
end;

function UnQPString(var s: string): Boolean;
var
    i, value: Integer;
    output: string;
begin
    output := '';
    i := 1;
    UnQPString := True;
    while i <= Length(s) do
    begin
        if (s[i] = '=') and (i + 2 <= Length(s)) then
        begin
            if DecodeHex(s[i + 1], s[i + 2], value) then
            begin
                output := output + Chr(value);
                Inc(i, 3);
            end
            else
            begin
                UnQPString := False;
                output := output + s[i];
                Inc(i);
            end;
        end
        else
        begin
            output := output + s[i];
            Inc(i);
        end;
    end;
    s := output;
end;

function UnQuoteString(var s: string): Boolean;
var
    inner: string;
begin
    if Length(s) < 2 then
    begin
        UnQuoteString := False;
        exit;
    end;
    if (s[1] <> '"') or (s[Length(s)] <> '"') then
    begin
        UnQuoteString := False;
        exit;
    end;
    inner := Copy(s, 2, Length(s) - 2);
    if not UnQPString(inner) then
    begin
        UnQuoteString := False;
        exit;
    end;
    s := inner;
    UnQuoteString := True;
end;

function ShellQuoteString(const s: string): TString;
var
    i: Integer;
    output: TString;
begin
    output := '''';
    for i := 1 to Length(s) do
    begin
        if s[i] = '''' then
            output := output + '''"''"'''
        else
            output := output + s[i];
    end;
    output := output + '''';
    ShellQuoteString := output;
end;

function ExpandTabs(var s: string; TabSize: Integer): Boolean;
var
    i, column, spaces: Integer;
    output: string;
begin
    if TabSize <= 0 then
        TabSize := 8;
    column := 0;
    output := '';
    for i := 1 to Length(s) do
    begin
        if s[i] = #9 then
        begin
            spaces := TabSize - (column mod TabSize);
            while spaces > 0 do
            begin
                output := output + ' ';
                Inc(column);
                Dec(spaces);
            end;
        end
        else
        begin
            output := output + s[i];
            if s[i] in [#10, #13] then
                column := 0
            else
                Inc(column);
        end;
    end;
    s := output;
    ExpandTabs := True;
end;

function ExpandCEscapeSequences(const s: string; RemoveQuoteChars,
  AllowOctal: Boolean): TString;
var
    i, octValue, digits: Integer;
    output: string;
begin
    output := '';
    i := 1;
    while i <= Length(s) do
    begin
        if (s[i] = '\') and (i < Length(s)) then
        begin
            Inc(i);
            case s[i] of
                'n': output := output + #10;
                'r': output := output + #13;
                't': output := output + #9;
                'b': output := output + #8;
                'f': output := output + #12;
                '\': output := output + '\';
                '''': output := output + '''';
                '"': output := output + '"';
                '0'..'7':
                    begin
                        if AllowOctal then
                        begin
                            octValue := Ord(s[i]) - Ord('0');
                            digits := 1;
                            while (digits < 3) and (i + 1 <= Length(s)) and (s[i + 1] in ['0'..'7']) do
                            begin
                                Inc(i);
                                octValue := (octValue shl 3) + (Ord(s[i]) - Ord('0'));
                                Inc(digits);
                            end;
                            output := output + Chr(octValue);
                        end
                        else if RemoveQuoteChars then
                            output := output + s[i]
                        else
                            output := output + '\' + s[i];
                    end;
            else
                if RemoveQuoteChars then
                    output := output + s[i]
                else
                    output := output + '\' + s[i];
            end;
        end
        else
            output := output + s[i];
        Inc(i);
    end;
    ExpandCEscapeSequences := output;
end;

procedure StrSkipSpaces(const s: string; var i: Integer);
begin
    while (i <= Length(s)) and (s[i] in [' ', #9]) do
        Inc(i);
end;

function StrReadDelimitedInternal(const s: string; var i: Integer;
  var Dest: string; Delimiter: Char): Boolean;
var
    startIdx: Integer;
begin
    StrSkipSpaces(s, i);
    if (i > Length(s)) or (s[i] <> Delimiter) then
    begin
        StrReadDelimitedInternal := False;
        exit;
    end;
    startIdx := i;
    Inc(i);
    while (i <= Length(s)) and (s[i] <> Delimiter) do
        Inc(i);
    if (i > Length(s)) or (s[i] <> Delimiter) then
    begin
        StrReadDelimitedInternal := False;
        exit;
    end;
    Dest := Copy(s, startIdx + 1, i - startIdx - 1);
    Inc(i);
    StrReadDelimitedInternal := True;
end;

function StrReadQuoted(const s: string; var i: Integer; var Dest: string): Boolean;
var
    temp: string;
begin
    if not StrReadDelimitedInternal(s, i, temp, '"') then
    begin
        StrReadQuoted := False;
        exit;
    end;
    temp := '"' + temp + '"';
    if not UnQuoteString(temp) then
    begin
        StrReadQuoted := False;
        exit;
    end;
    Dest := temp;
    StrReadQuoted := True;
end;

function StrReadDelimited(const s: string; var i: Integer; var Dest: string;
  Delimiter: Char): Boolean;
begin
    StrReadDelimited := StrReadDelimitedInternal(s, i, Dest, Delimiter);
end;

function StrReadWord(const s: string; var i: Integer; var Dest: string): Boolean;
var
    startIdx: Integer;
begin
    StrSkipSpaces(s, i);
    if i > Length(s) then
    begin
        StrReadWord := False;
        exit;
    end;
    startIdx := i;
    while (i <= Length(s)) and not (s[i] in [' ', #9, ',']) do
        Inc(i);
    Dest := Copy(s, startIdx, i - startIdx);
    StrReadWord := Dest <> '';
end;

function StrReadConst(const s: string; var i: Integer; const Expected: string): Boolean;
var
    len: Integer;
begin
    StrSkipSpaces(s, i);
    len := Length(Expected);
    if (len = 0) or ((i + len - 1) > Length(s)) then
    begin
        StrReadConst := False;
        exit;
    end;
    if Copy(s, i, len) = Expected then
    begin
        Inc(i, len);
        StrReadConst := True;
    end
    else
        StrReadConst := False;
end;

function StrReadComma(const s: string; var i: Integer): Boolean;
begin
    StrReadComma := StrReadConst(s, i, ',');
end;

function StrReadInt(const s: string; var i: Integer; var Dest: Integer): Boolean;
var
    startIdx, code: Integer;
    temp: string;
begin
    StrSkipSpaces(s, i);
    if i > Length(s) then
    begin
        StrReadInt := False;
        exit;
    end;
    startIdx := i;
    if s[i] in ['+', '-'] then
        Inc(i);
    while (i <= Length(s)) and (s[i] in ['0'..'9']) do
        Inc(i);
    temp := Copy(s, startIdx, i - startIdx);
    if temp = '' then
    begin
        StrReadInt := False;
        exit;
    end;
    Val(temp, Dest, code);
    StrReadInt := code = 0;
    if not StrReadInt then
        i := startIdx;
end;

function StrReadReal(const s: string; var i: Integer; var Dest: Real): Boolean;
var
    startIdx, code: Integer;
    temp: string;
begin
    StrSkipSpaces(s, i);
    startIdx := i;
    if (i <= Length(s)) and (s[i] in ['+', '-']) then
        Inc(i);
    while (i <= Length(s)) and (s[i] in ['0'..'9']) do
        Inc(i);
    if (i <= Length(s)) and (s[i] = '.') then
    begin
        Inc(i);
        while (i <= Length(s)) and (s[i] in ['0'..'9']) do
            Inc(i);
    end;
    if (i <= Length(s)) and (s[i] in ['E', 'e']) then
    begin
        Inc(i);
        if (i <= Length(s)) and (s[i] in ['+', '-']) then
            Inc(i);
        while (i <= Length(s)) and (s[i] in ['0'..'9']) do
            Inc(i);
    end;
    temp := Copy(s, startIdx, i - startIdx);
    if temp = '' then
    begin
        StrReadReal := False;
        exit;
    end;
    Val(temp, Dest, code);
    StrReadReal := code = 0;
    if not StrReadReal then
        i := startIdx;
end;

function StrReadBoolean(const s: string; var i: Integer; var Dest: Boolean): Boolean;
begin
    StrSkipSpaces(s, i);
    if i > Length(s) then
    begin
        StrReadBoolean := False;
        exit;
    end;
    StrReadBoolean := Char2Boolean(s[i], Dest);
    if StrReadBoolean then
        Inc(i);
end;

function StrReadEnum(const s: string; var i: Integer; var Dest: Integer;
  const IDs: array of PString): Boolean;
var
    wordValue: string;
    idx: Integer;
begin
    if not StrReadWord(s, i, wordValue) then
    begin
        StrReadEnum := False;
        exit;
    end;
    for idx := 0 to High(IDs) do
    begin
        if (IDs[idx] <> nil) and (IDs[idx]^ = wordValue) then
        begin
            Dest := idx;
            StrReadEnum := True;
            exit;
        end;
    end;
    StrReadEnum := False;
end;

end.
