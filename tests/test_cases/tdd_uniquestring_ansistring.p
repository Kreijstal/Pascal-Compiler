program tdd_uniquestring_ansistring;
{$mode objfpc}{$H+}
{ UniqueString(var S) must make a refcount-shared AnsiString into a private
  copy so a later in-place mutation does not affect the aliased string.
  KGPC registers UniqueString as a builtin proc; codegen must route the call
  to the runtime helper kgpc_string_unique rather than emit an undefined
  `uniquestring_s`.  FPC's own compiler (omfbase.pas, cfileutl.fixpath) relies
  on this when compiling the 3.2.2 release. }
var
  a, b: ansistring;
begin
  a := 'hello';
  b := a;            { b shares a's buffer (refcount 2) }
  UniqueString(b);   { b must become a private copy }
  b[1] := 'H';       { mutate b only }
  writeln(a);        { must still be 'hello' }
  writeln(b);        { must be 'Hello' }
end.
