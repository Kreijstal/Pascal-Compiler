program tdd_pp_bootstrap_aliased_record_overload;
{ Regression: var-param call must accept an actual whose record_info
  differs from the formal's record_info but resolves to the same
  canonical record via the symbol table's alias chain.

  Reproduces the win-target FPC pp.pas failure where
    function FileTimeToSystemTime(const lpFileTime: TFileTime;
                                  var   lpSystemTime: TSystemTime): BOOL;
  was rejected at filutil.inc:669: rtl/win/sysos.inc declares TFileTime
  as a raw record, wininc/struct.inc declares the same identifier as a
  transparent alias for FILETIME, so the symbol table holds two
  distinct RecordType nodes under the same name and the resolver
  treated them as incompatible. }
uses tdd_pp_bootstrap_aliased_record_overload_lo,
     tdd_pp_bootstrap_aliased_record_overload_hi;

var
  r: TSharedRec; { resolves to one declaration of the name }
begin
  r.a := 10;
  r.b := 20;
  { Stamp's formal parameter is `var R: TSharedRec` — resolved against
    the OTHER declaration of the same name when the unit is compiled.
    The call is valid because both names alias the same canonical
    record. }
  Stamp(r);
  WriteLn(r.a, ' ', r.b);
  WriteLn('OK');
end.
