{ Regression: unit-qualified System.Assign(f, s) with a ShortString *variable*
  argument must pass the shortstring to FPC's Assign(File, ShortString) overload
  AS a shortstring, not silently convert it to an AnsiString.

  The qualified-call path used to route System.Assign through the coarse builtin
  name-mangler, which only sees the STRING_TYPE tag and cannot tell a ShortString
  from an AnsiString.  It picked the RawByteString overload and inserted a
  shortstring->ansistring conversion of the argument; the ShortString-overload
  body (Assign(f, ShortString) -> Assign(f, RawByteString(Name))) then read the
  ansistring's first DATA byte as a length byte and copied from data+1, so the
  filename lost its first character.  Unqualified Assign was unaffected because
  it goes through the full overload resolver, which scores ShortString as an
  exact match.

  This is exactly how FPC 3.2.2's TCFileStream.Create -> system.assign(FHandle,
  AFileName) failed to open "system.pp" (it tried "ystem.pp") during bootstrap.

  The file is first created via a string literal (known-good path); it is then
  reopened via a ShortString variable through System.Assign.  If the leading
  character is dropped, the reopen targets a different, non-existent path and
  Reset fails (IOResult <> 0).

  The file/shortstring locals live inside a procedure on purpose: a *global*
  `var f: file; s: shortstring` pair trips a separate, layout-sensitive
  stdio-init memory-corruption bug (over-read in kgpc_string_to_char_array)
  that has nothing to do with the qualified-Assign fix under test.  Keeping
  them local isolates this regression to the argument-conversion path. }
program fpc_bootstrap_system_assign_shortstring_var;

procedure RoundTrip;
var
  f : file;
  s : shortstring;
begin
  System.Assign(f, 'tests/output/sys_assign_ss_var.dat');   { literal: correct path }
  System.Rewrite(f, 1);
  System.Close(f);

  s := 'tests/output/sys_assign_ss_var.dat';                { ShortString variable }
  System.Assign(f, s);                                      { the bug trigger }
  {$I-} System.Reset(f, 1); {$I+}
  if IOResult <> 0 then
    WriteLn('OPEN FAILED')
  else
  begin
    System.Close(f);
    WriteLn('roundtrip');
  end;
end;

begin
  RoundTrip;
end.
