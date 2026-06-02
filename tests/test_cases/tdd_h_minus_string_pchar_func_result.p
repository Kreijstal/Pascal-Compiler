program tdd_h_minus_string_pchar_func_result;
{$H-}

{ Regression mirroring FPC's compiler ogcoff.pas TCoffObjInput.Read_str under
  the default $H- (ShortString) string mode:

      function Read_str(strpos): string;     { string = ShortString here }
      begin Read_str := string(PChar(@FCoffStrs[strpos])); end;

  The `string(PChar(...))` cast records SHORTSTRING_TYPE as its target.  The
  old lowering passed the raw C-string pointer to kgpc_string_to_shortstring,
  whose C-string heuristic guesses the length from src[0] and drops the first
  character when (unsigned char)src[0] == strlen-1.  A 47-char COFF section
  name beginning with '.' (0x2E = 46) hits that coincidence: the leading '.'
  vanished, so the section name missed the Win64 link-script glob and the
  internal linker raised Internal error 202102001 in the KGPC->FPC self-host.

  Now `string(PChar)` materialises a genuine managed string (StrPas) before it
  reaches the ShortString result, so the '.' survives. }

var
  FCoffStrs: pchar;
  secname: string;
  buf: array[0..63] of char;
  i: integer;

const
  src = '.text.n_system_$$_sysgetheapstatus$$theapstatus';  { 47 chars }

function Read_str(strpos: longword): string;
begin
  Read_str := string(PChar(@FCoffStrs[strpos]));
end;

begin
  for i := 0 to length(src) - 1 do
    buf[i] := src[i + 1];
  buf[length(src)] := #0;
  FCoffStrs := @buf[0];
  secname := Read_str(0);
  writeln(secname);
end.
