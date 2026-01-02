program rtl_strpos_demo;
{$mode objfpc}
{$H+}

uses sysutils;

var
  s: AnsiString;
  sub: AnsiString;
  p: PChar;
  subPtr: PChar;
  posPtr: PChar;
  rposPtr: PChar;
  len: SizeInt;
  posIdx: SizeInt;
  rposIdx: SizeInt;
begin
  s := 'hello';
  sub := 'll';
  p := PChar(s);
  subPtr := PChar(sub);

  len := StrLen(p);
  posPtr := StrPos(p, subPtr);
  rposPtr := StrRScan(p, 'l');

  posIdx := -1;
  if posPtr <> nil then
    posIdx := (PtrUInt(posPtr) - PtrUInt(p)) + 1;

  rposIdx := -1;
  if rposPtr <> nil then
    rposIdx := (PtrUInt(rposPtr) - PtrUInt(p)) + 1;

  writeln('StrLen=', len);
  writeln('StrPos=', posIdx);
  writeln('StrRScan=', rposIdx);
end.
