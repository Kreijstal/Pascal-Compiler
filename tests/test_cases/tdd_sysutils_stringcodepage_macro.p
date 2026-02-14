program tdd_sysutils_stringcodepage_macro;

{$mode objfpc}

const
  CP_UTF8 = 65001;
  CP_OTHER = 1252;

function FakeStringCodePage(const S: string): LongInt;
var
  sum: LongInt;
  i: Integer;
begin
  sum := 0;
  for i := 1 to Length(S) do
    sum := sum + Ord(S[i]) * i;
  if ((sum mod 5) = 0) or ((Length(S) mod 2) = 0) then
    Result := CP_UTF8
  else
    Result := CP_OTHER;
end;

function MarkUtf8(const Name: string): Boolean;
var
  UTF8: Boolean;
begin
{$ifdef KGPC}
  {UTF8:=FakeStringCodePage}(Name)=CP_UTF8;
{$else}
  UTF8 := FakeStringCodePage(Name) = CP_UTF8;
{$endif}
  Result := UTF8;
end;

function CompositeScore(const Name: string; idx: Integer): LongInt;
var
  i: Integer;
  s: LongInt;
begin
  s := 0;
  for i := 1 to Length(Name) do
    s := s + Ord(Name[i]) * (i + idx);
  if MarkUtf8(Name) then
    s := s + Length(Name) * 17
  else
    s := s - Length(Name) * 9;
  Result := s;
end;

var
  names: array[0..4] of string;
  total: LongInt;
  i: Integer;
begin
  names[0] := 'Alpha';
  names[1] := 'Beta';
  names[2] := 'Gamma';
  names[3] := 'Delta';
  names[4] := 'Epsilon';

  total := 0;
  for i := 0 to High(names) do
    total := total + CompositeScore(names[i], i + 1);

  if MarkUtf8('Zeta') then
    total := total + 111
  else
    total := total - 111;

  if MarkUtf8('Omega') then
    total := total + 333
  else
    total := total - 333;

  Writeln('Total=', total);
end.
