{ Regression test: AnsiString variable passed to a var parameter
  declared as RawByteString (or UnicodeString -> UnicodeString) must
  be accepted via FPC's codepage-compatibility rule.

  FPC treats AnsiString and RawByteString as sharing the dynstring
  layout — an AnsiString variable is a valid argument for a var
  parameter declared as RawByteString (encoding=CP_NONE).  Same for
  the UnicodeString family.

  KGPC's overload resolution (SemCheck_overload.c) recognises every
  pair of primitive string tags as compatible for var/out params, so
  these calls type-check without a forced exact-type match.

  This anchors the behavior so future tightening cannot accidentally
  break the AnsiString/RawByteString interop that pp.pas and the FPC
  RTL rely on (Delete / Insert / SetCodePage / fpc_ansistr_*). }
program regr_varparam_ansistring_to_rawbytestring;

procedure ChopFirst(var s: RawByteString);
begin
  Delete(s, 1, 1);
end;

procedure PutAt(const xy: RawByteString; var dest: RawByteString;
                pos: LongInt);
begin
  Insert(xy, dest, pos);
end;

procedure ChopFirstAnsi(var s: AnsiString);
begin
  Delete(s, 1, 1);
end;

var
  a: AnsiString;
  r: RawByteString;
begin
  a := 'hello';
  ChopFirst(a);            { AnsiString -> var RawByteString }
  PutAt('xy', a, 1);       { AnsiString -> var RawByteString, second slot }
  WriteLn(a);              { expect: xyello }

  r := 'world';
  ChopFirstAnsi(r);        { RawByteString -> var AnsiString (reverse) }
  WriteLn(r);              { expect: orld }
end.
