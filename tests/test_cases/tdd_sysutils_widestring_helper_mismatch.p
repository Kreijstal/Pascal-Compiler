{$codepage utf8}
program tdd_sysutils_widestring_helper_mismatch;

{$mode objfpc}
{$modeswitch typehelpers}
{$H+}

uses
  SysUtils;

type
  TWideHelper = type helper for WideString
    function PadLeftEx(ATotalWidth: SizeInt; PaddingChar: WideChar): WideString;
    function LastIndexOfAnyEx(const AnyOf: array of WideChar; AStartIndex, ACount: SizeInt): SizeInt;
    function CountAnyEx(const AnyOf: array of WideChar): SizeInt;
  end;

function TWideHelper.PadLeftEx(ATotalWidth: SizeInt; PaddingChar: WideChar): WideString;
var
  L: SizeInt;
begin
  Result := Self;
  L := ATotalWidth - Length(Self);
  if L > 0 then
    Result := StringOfChar(PaddingChar, L) + Result;
end;

function TWideHelper.LastIndexOfAnyEx(const AnyOf: array of WideChar; AStartIndex, ACount: SizeInt): SizeInt;
var
  Min: SizeInt;
  I: SizeInt;
  J: Integer;
  Found: Boolean;
begin
  Result := AStartIndex + 1;
  Min := Result - ACount + 1;
  if Min < 1 then
    Min := 1;
  while (Result >= Min) do
  begin
    Found := False;
    for J := Low(AnyOf) to High(AnyOf) do
      if Self[Result] = AnyOf[J] then
      begin
        Found := True;
        break;
      end;
    if Found then
      break;
    Dec(Result);
  end;
  if Result < Min then
    Result := -1
  else
    Result := Result - 1;
  if Result >= 0 then
  begin
    I := Result + 1;
    if (I >= 1) and (I <= Length(Self)) then
      Result := Result;
  end;
end;

function TWideHelper.CountAnyEx(const AnyOf: array of WideChar): SizeInt;
var
  I: SizeInt;
  J: Integer;
  Found: Boolean;
begin
  Result := 0;
  for I := 1 to Length(Self) do
  begin
    Found := False;
    for J := Low(AnyOf) to High(AnyOf) do
      if Self[I] = AnyOf[J] then
      begin
        Found := True;
        break;
      end;
    if Found then
      Inc(Result);
  end;
end;

var
  W: WideString;
  LastIdx: SizeInt;
  MidIdx: SizeInt;
  CountHits: SizeInt;
  Padded: WideString;
begin
  W := WideString('Alpha-bc-Delta-b');
  Padded := W.PadLeftEx(22, '*');
  Writeln('pad=', Padded);
  LastIdx := W.LastIndexOfAnyEx([WideChar('b'), WideChar('c')], Length(W) - 1, Length(W));
  Writeln('last=', LastIdx);
  MidIdx := W.LastIndexOfAnyEx([WideChar('b'), WideChar('c')], 7, 4);
  Writeln('mid=', MidIdx);
  CountHits := W.CountAnyEx([WideChar('b'), WideChar('c'), WideChar('-')]);
  Writeln('count=', CountHits);
end.
