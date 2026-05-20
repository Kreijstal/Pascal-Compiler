{ Regression test for KGPC bug where a Char-typed const declared via a bare
  identifier alias (`cbeg = uns` where `uns: Char = #1`) was incorrectly stored
  with STRING_TYPE during cross-unit prepush, causing
  `c in [cbeg..cend]` codegen to emit address comparisons against the
  string-rodata buffer instead of value comparisons against the char
  ordinal. This blocked FPC pp.pas self-host: scanner.pas's
  `internal_macro_escape_begin..end` test always evaluated false, so the
  internally-generated abstract-method stub `begin <#1>System.AbstractError;
  end;` raised "Illegal character #1" the moment the scanner saw the escape
  byte. }
program regr_char_const_alias;

uses regr_char_const_alias_unit;

begin
  if check_escape(#1) then
    writeln('Char #1 IS in [ce_begin..ce_end]')
  else
    writeln('FAIL: Char #1 is NOT in [ce_begin..ce_end]');

  if check_escape(#2) then
    writeln('FAIL: Char #2 IS in [ce_begin..ce_end]')
  else
    writeln('Char #2 is NOT in [ce_begin..ce_end]');

  if check_escape(#0) then
    writeln('FAIL: Char #0 IS in [ce_begin..ce_end]')
  else
    writeln('Char #0 is NOT in [ce_begin..ce_end]');
end.
