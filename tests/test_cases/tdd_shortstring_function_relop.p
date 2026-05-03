program TddShortStringFunctionRelop;
{$mode objfpc}
{$H-}

var
  UpperTable: array[char] of char;

type
  PShortString = ^ShortString;

procedure InitUpper;
var
  C: char;
begin
  for C := Chr(0) to Chr(255) do
    UpperTable[C] := C;
  for C := 'a' to 'z' do
    UpperTable[C] := Chr(Ord(C) - 32);
end;

function UpperName(const S: string): string;
var
  I: LongInt;
begin
  for I := 1 to Length(S) do
    UpperName[I] := UpperTable[S[I]];
  UpperName[0] := S[0];
end;

var
  Name: string;
  ExpectedName: PShortString;

begin
  InitUpper;
  New(ExpectedName);
  ExpectedName^ := 'SYSTEM';
  Name := UpperName('System');
  Writeln(Name);
  Writeln(Length(Name));
  Writeln(Ord(UpperName('System') = ExpectedName^));
  Writeln(Ord(UpperName('System') <> ExpectedName^));
end.
