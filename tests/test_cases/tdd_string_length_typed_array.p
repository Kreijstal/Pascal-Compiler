{ Regression test: shortstring element-type declared as `string[length('...')]`
  must evaluate the `length()` builtin at type-resolution time.

  Without the fix, KGPC parsed `string[length(literal)]` as `string[0]`
  because evaluate_const_int_expr did not recognize the `length()`
  builtin applied to a string literal. The typed-const array then ended
  up with a 1-byte element load (`movzbl (%r12), %ebx`) paired with a
  256-byte element stride, so indexed access dereferenced the length
  byte and passed that byte as a pointer to `kgpc_shortstring_to_shortstring`.

  This mirrors FPC's pattern in `ogelf.pas`:
    secnames : array[TAsmSectiontype] of
      string[length('__DATA, __datacoal_nt,coalesced')] = (...); }
{$mode objfpc}
{$H-}
program tdd_string_length_typed_array;

type
  TKind = (k_a, k_b, k_c);

procedure pick(k: TKind);
const
  names: array[TKind] of string[length('__DATA, __datacoal_nt,coalesced')] =
    ('alpha', 'beta', 'gamma');
var
  s: string;
begin
  s := names[k];
  writeln(s);
end;

begin
  pick(k_a);
  pick(k_b);
  pick(k_c);
end.
