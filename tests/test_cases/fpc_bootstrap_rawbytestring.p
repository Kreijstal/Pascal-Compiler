{ Test RawByteString type - required for FPC bootstrap }
{ FPC RTL unixutil.pp uses RawByteString for string handling }
{ RawByteString is a built-in type alias for code-page-aware strings }
program fpc_bootstrap_rawbytestring;

{$mode objfpc}

procedure TestRaw(const s: RawByteString);
begin
  WriteLn('Raw: ', s);
end;

var
  s: RawByteString;
begin
  s := 'Hello Raw';
  TestRaw(s);
  TestRaw('Direct Raw');
  WriteLn('Done');
end.
