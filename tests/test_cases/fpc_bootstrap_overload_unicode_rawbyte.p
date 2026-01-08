{
  Test case: Overload resolution fails when both UnicodeString and RawByteString
  overloads are present in a unit.

  BUG: KGPC fails to match string literal 'hello' to ShortString overload
  when both UnicodeString and RawByteString overloads exist.

  FPC correctly resolves 'hello' to the ShortString overload.

  This blocks FPC bootstrap because system.pp has:
    Procedure Assign(out f:File; const Name: ShortString);
    Procedure Assign(out f:File; const Name: UnicodeString);
    Procedure Assign(out f:File; const Name: RawByteString);
}
program fpc_bootstrap_overload_unicode_rawbyte;
{$mode objfpc}

const
  CP_NONE = $FFFF;

type
  RawByteString = type AnsiString(CP_NONE);
  UnicodeString = type WideString;

procedure Test(const Name: ShortString);
begin
  WriteLn('ShortString: ', Name);
end;

procedure Test(const Name: UnicodeString);
begin
  WriteLn('UnicodeString');
end;

procedure Test(const Name: RawByteString);
begin
  WriteLn('RawByteString');
end;

begin
  Test('hello');  { Should call ShortString overload }
end.
