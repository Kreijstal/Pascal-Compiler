program tdd_typinfo_basic;

{$mode objfpc}

uses
  TypInfo;

type
  TMyEnum = (meFirst, meSecond);

begin
  Writeln(GetEnumName(TypeInfo(TMyEnum), Ord(meSecond)));
end.
