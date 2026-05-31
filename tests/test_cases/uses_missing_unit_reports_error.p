program uses_missing_unit_reports_error;

{ A `uses` clause that names a unit with no source on any search path must be
  a hard error, exactly as FPC reports it ("Can't find unit X").  Silently
  skipping the reference would let compilation continue with an incomplete
  symbol table and hide real bugs (e.g. a missing -Fu path during bootstrap). }
uses
  this_unit_definitely_does_not_exist_xyz;

begin
  WriteLn('unreachable');
end.
