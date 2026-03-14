program string_utils_basic;

{$mode objfpc}
{$H+}

uses
    SysUtils;

var
    s: string;
    flag: Boolean;

procedure StrCut(var Dest: string; MaxLength: Integer);
begin
    if MaxLength < 0 then
        MaxLength := 0;
    if Length(Dest) > MaxLength then
        SetLength(Dest, MaxLength);
end;

function StrCount(const SubStr, Dest: string): Integer;
var
    i: Integer;
    lenSub: Integer;
begin
    if SubStr = '' then
    begin
        StrCount := 0;
        exit;
    end;

    lenSub := Length(SubStr);
    StrCount := 0;
    for i := 1 to Length(Dest) - lenSub + 1 do
        if Copy(Dest, i, lenSub) = SubStr then
            Inc(StrCount);
end;

function StrReplace(const Dest, Source, Replacement: string): string;
begin
    StrReplace := StringReplace(Dest, Source, Replacement, [rfReplaceAll]);
end;

function AnsiContainsStr(const AText, ASubText: string): Boolean;
begin
    if ASubText = '' then
        AnsiContainsStr := True
    else
        AnsiContainsStr := Pos(ASubText, AText) > 0;
end;

function AnsiContainsText(const AText, ASubText: string): Boolean;
begin
    AnsiContainsText := Pos(UpperCase(ASubText), UpperCase(AText)) > 0;
end;

function ContainsStr(const AText, ASubText: string): Boolean;
begin
    ContainsStr := AnsiContainsStr(AText, ASubText);
end;

function ContainsText(const AText, ASubText: string): Boolean;
begin
    ContainsText := AnsiContainsText(AText, ASubText);
end;

function AnsiStartsStr(const SubStr, S: string): Boolean;
begin
    AnsiStartsStr := Copy(S, 1, Length(SubStr)) = SubStr;
end;

function AnsiStartsText(const SubStr, S: string): Boolean;
begin
    AnsiStartsText := UpperCase(Copy(S, 1, Length(SubStr))) = UpperCase(SubStr);
end;

function AnsiEndsStr(const SubStr, S: string): Boolean;
begin
    if Length(SubStr) > Length(S) then
        AnsiEndsStr := False
    else
        AnsiEndsStr := Copy(S, Length(S) - Length(SubStr) + 1, Length(SubStr)) = SubStr;
end;

function AnsiEndsText(const SubStr, S: string): Boolean;
begin
    if Length(SubStr) > Length(S) then
        AnsiEndsText := False
    else
        AnsiEndsText :=
            UpperCase(Copy(S, Length(S) - Length(SubStr) + 1, Length(SubStr))) =
            UpperCase(SubStr);
end;

function StartsStr(const SubStr, S: string): Boolean;
begin
    StartsStr := AnsiStartsStr(SubStr, S);
end;

function StartsText(const SubStr, S: string): Boolean;
begin
    StartsText := AnsiStartsText(SubStr, S);
end;

function EndsStr(const SubStr, S: string): Boolean;
begin
    EndsStr := AnsiEndsStr(SubStr, S);
end;

function EndsText(const SubStr, S: string): Boolean;
begin
    EndsText := AnsiEndsText(SubStr, S);
end;

function DupeString(const S: string; Count: Integer): string;
var
    i: Integer;
begin
    DupeString := '';
    for i := 1 to Count do
        DupeString := DupeString + S;
end;

function AnsiReplaceStr(const S, OldPattern, NewPattern: string): string;
begin
    AnsiReplaceStr := StringReplace(S, OldPattern, NewPattern, [rfReplaceAll]);
end;

function AnsiReplaceText(const S, OldPattern, NewPattern: string): string;
begin
    AnsiReplaceText := StringReplace(S, OldPattern, NewPattern, [rfReplaceAll, rfIgnoreCase]);
end;

function Char2Boolean(ch: Char; var Dest: Boolean): Boolean;
begin
    if (ch = 'Y') or (ch = 'y') then
    begin
        Dest := True;
        Char2Boolean := True;
    end
    else if (ch = 'N') or (ch = 'n') then
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

begin
    s := 'Hello';
    AppendStr(s, ' World');
    writeln('Append=', s);
    StrCut(s, 5);
    writeln('Cut=', s);
    writeln('Count=', StrCount('ana', 'bananas'));
    writeln('Replace=', StrReplace('abc abc', 'abc', 'x'));

    if Char2Boolean('y', flag) then
        writeln('Bool=', flag)
    else
        writeln('Bool=?');
    writeln('Digit=', Char2Digit('F'));

    if AnsiContainsStr('Pascal', 'sca') then
        writeln('AnsiContainsStr=TRUE')
    else
        writeln('AnsiContainsStr=FALSE');

    if AnsiContainsText('Pascal', 'SCAL') then
        writeln('AnsiContainsText=TRUE')
    else
        writeln('AnsiContainsText=FALSE');

    if AnsiStartsStr('Pas', 'Pascal') then
        writeln('AnsiStartsStr=TRUE')
    else
        writeln('AnsiStartsStr=FALSE');

    if AnsiStartsText('pas', 'Pascal') then
        writeln('AnsiStartsText=TRUE')
    else
        writeln('AnsiStartsText=FALSE');

    if AnsiEndsStr('cal', 'Pascal') then
        writeln('AnsiEndsStr=TRUE')
    else
    writeln('AnsiEndsStr=FALSE');

    if AnsiEndsText('CAL', 'Pascal') then
        writeln('AnsiEndsText=TRUE')
    else
        writeln('AnsiEndsText=FALSE');

    if ContainsStr('Pascal', 'pas') then
        writeln('ContainsStr=TRUE')
    else
        writeln('ContainsStr=FALSE');

    if ContainsText('Pascal', 'pas') then
        writeln('ContainsText=TRUE')
    else
        writeln('ContainsText=FALSE');

    if StartsStr('Hello', 'Hello World') then
        writeln('StartsStr=TRUE')
    else
        writeln('StartsStr=FALSE');

    if StartsText('hello', 'Hello World') then
        writeln('StartsText=TRUE')
    else
        writeln('StartsText=FALSE');

    if EndsStr('World', 'Hello World') then
        writeln('EndsStr=TRUE')
    else
        writeln('EndsStr=FALSE');

    if EndsText('world', 'Hello World') then
        writeln('EndsText=TRUE')
    else
        writeln('EndsText=FALSE');

    writeln('Dupe=', DupeString('ab', 3));
    writeln('AnsiReplaceStr=', AnsiReplaceStr('Foo foo', 'Foo', 'Bar'));
    writeln('AnsiReplaceText=', AnsiReplaceText('Foo foo', 'FOO', 'Bar'));
end.
