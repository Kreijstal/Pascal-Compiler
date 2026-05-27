{ Regression: FPC's rtl/win/windirs.pp declares procedure-variable globals
  with calling-convention modifiers AND inline initializers, e.g.

      _CoTaskMemFree : procedure(p:Pointer); stdcall; = nil;

  Previously KGPC's procedure-type parser consumed the leading ';' before
  the calling-convention keyword but not the trailing ';' between the
  directive and the '=' initializer, so var-decl parsing failed at '='. }
{$mode objfpc}
program ProcVarStdcallWithInitializer;

var
  cb1: procedure(p: Pointer); stdcall; = nil;
  cb2: procedure(a: LongInt; b: LongInt); cdecl; = nil;
  cb3: function(x: LongInt): LongInt; stdcall; = nil;

begin
  if Assigned(cb1) then
    Writeln('cb1 assigned')
  else
    Writeln('cb1 nil');
  if Assigned(cb2) then
    Writeln('cb2 assigned')
  else
    Writeln('cb2 nil');
  if Assigned(cb3) then
    Writeln('cb3 assigned')
  else
    Writeln('cb3 nil');
end.
