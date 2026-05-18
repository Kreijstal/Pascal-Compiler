{ Helper unit for regr_char_const_alias.

  Reproduces FPC scanner.pas's interface-section pattern:

    internal_macro_escape_unit_namespace_name = #1;
    internal_macro_escape_begin = internal_macro_escape_unit_namespace_name;
    internal_macro_escape_end   = internal_macro_escape_unit_namespace_name;

  When a unit publishes a CHAR const and a second CHAR const that aliases
  it (no typecast, just a bare identifier reference), the prepush pass
  during cross-unit loading must classify the alias as CHAR_TYPE — not
  STRING_TYPE — otherwise codegen routes the alias through the string
  rodata path and `c in [alias..alias]` collapses to comparing pointers. }
unit regr_char_const_alias_unit;

interface

const
  ce_escape = #1;
  ce_begin  = ce_escape;
  ce_end    = ce_escape;

function check_escape(c: char): boolean;

implementation

function check_escape(c: char): boolean;
begin
  Result := c in [ce_begin..ce_end];
end;

end.
