program tdd_ptr_deref_field_helper;
{$mode objfpc}{$H+}
{ Test case for issue #482:
  Cross-unit pointer dereference with SysUtils type helper in scope.
  TCGPara.Location is PCGParaLocation (forward-declared pointer to record).
  When dereferenced, field access should resolve to TCGParaLocation record,
  NOT to TLongIntHelper from SysUtils. }

uses SysUtils, tdd_ptr_deref_field_helper_unit;

var
  cgpara: TCGPara;
  loc: TCGParaLocation;
begin
  loc.shiftval := 42;
  loc.Size := 8;
  loc.register_num := 3;
  loc.Next := nil;
  cgpara.Location := @loc;
  cgpara.IntSize := 4;

  { Cross-unit pointer dereference field access }
  WriteLn(cgpara.Location^.shiftval);
  WriteLn(cgpara.Location^.Size);
  WriteLn(cgpara.Location^.register_num);

  { Modify through pointer dereference }
  cgpara.Location^.shiftval := 99;
  WriteLn(loc.shiftval);

  WriteLn('OK');
end.
