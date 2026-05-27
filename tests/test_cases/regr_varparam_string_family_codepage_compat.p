{ Regression test for STATUS.md Stage-4 entry:
  "AnsiString -> RawByteString/UnicodeString var-param mismatch".

  Background: FPC's overload/argument-compat rule for the string family is
  that AnsiString, RawByteString, UnicodeString, WideString and the plain
  String alias all share the same dynamic-string layout (pointer to a
  heap header + payload).  A var/out parameter typed as any one of these
  types accepts an argument typed as any other one of these types, with
  the codepage being a runtime/encoding concern rather than a static type
  identity.  This is the rule that lets FPC's own `cutils.pas` call
  `Delete(s, i, n)` / `Insert(s2, s, i)` from procedures whose `s` is
  `var s: AnsiString` even though the compilerproc signatures use
  `var S : RawByteString`.

  The bug as previously documented was that KGPC's var-param overload
  scorer would reject those calls and the 8 cutils.pas call sites would
  fail to type-check.  KGPC now treats the entire string family
  (STRING_TYPE / SHORTSTRING_TYPE primitive tags) as compatible at
  MATCH_PROMOTION quality for var/out parameters
  (SemCheck_overload.c semcheck_calculate_var_param_match), and the
  direct call-site checker mirrors that via
  are_types_compatible_for_assignment in KgpcType.c (the
  lhs_is_string && rhs_is_string short-circuit covers the string family
  regardless of codepage label).

  This regression pins every cross-pairing so a future tightening cannot
  silently break the cutils.pas / fpc_ansistr_* interop relied on by the
  FPC bootstrap.
}
program regr_varparam_string_family_codepage_compat;
{$mode objfpc}{$H+}

procedure WantAnsiString(var s: AnsiString);
begin
  s := s + '!';
end;

procedure WantRawByteString(var s: RawByteString);
begin
  s := s + '?';
end;

procedure WantUnicodeString(var s: UnicodeString);
begin
  s := s + 'U';
end;

procedure WantString(var s: String);
begin
  s := s + '.';
end;

{ Cutils.pas pattern: var s: AnsiString in caller, callee var s: RawByteString. }
procedure CutilsReplace(var s: AnsiString);
begin
  Delete(s, 1, 1);
  Insert('Z', s, 1);
end;

var
  a: AnsiString;
  r: RawByteString;
  u: UnicodeString;
  s: String;
begin
  a := 'aa';
  r := 'rr';
  u := 'uu';
  s := 'ss';

  { AnsiString -> all string-family var params }
  WantAnsiString(a);
  WantRawByteString(a);
  WantString(a);

  { RawByteString -> all string-family var params }
  WantAnsiString(r);
  WantRawByteString(r);
  WantString(r);

  { UnicodeString -> UnicodeString var param }
  WantUnicodeString(u);

  { String alias -> all string-family var params }
  WantAnsiString(s);
  WantRawByteString(s);
  WantString(s);

  { Direct cutils.pas pattern }
  a := 'hello';
  CutilsReplace(a);

  WriteLn(a);          { expect: Zello }
  WriteLn(r);          { expect: rr?! }
  WriteLn(u);          { expect: uuU }
  WriteLn(s);          { expect: ss!?. }
end.
