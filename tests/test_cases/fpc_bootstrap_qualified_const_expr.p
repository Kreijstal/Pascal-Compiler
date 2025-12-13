{ Test qualified constant access in runtime expressions }
{ Required for FPC bootstrap: unix.pp uses patterns like: }
{   var x: Integer; x := UnixType.ARG_MAX; }
{ This tests unit.Constant syntax in assignment statements, }
{ not just const declarations which already work. }
program fpc_bootstrap_qualified_const_expr;

uses fpc_qualified_const_unit;

var
  x, y: Integer;
begin
  { Qualified constant access in runtime assignment }
  x := fpc_qualified_const_unit.CONST_VALUE_42;
  y := fpc_qualified_const_unit.CONST_MAX_1024;
  
  WriteLn(x);
  WriteLn(y);
  WriteLn(x + y);
end.
