{ Test: {$if declared()} compile-time check }
{ From FPC unix.pp - uses {$if declared(UseTZThreading)} pattern }
program fpc_bootstrap_if_declared;
{$mode objfpc}

var
  MyVar: Integer;

begin
  MyVar := 42;
  {$if declared(MyVar)}
  WriteLn('MyVar declared: ', MyVar);
  {$endif}
  {$if declared(NotDeclaredVar)}
  WriteLn('ERROR: should not be included');
  {$endif}
  WriteLn('Done');
end.
