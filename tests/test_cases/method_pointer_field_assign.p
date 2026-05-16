{ Regression test: method-pointer assignment between record fields and
  parameters must copy the full 16-byte (code+self) descriptor.

  Previously KGPC emitted an 8-byte pointer copy for "ctx.f := f" where f
  was a method-pointer parameter, so only the code half was copied and
  invoking ctx.f through an adapter dispatched with a NULL Self.  This
  bug caused pp.pas bootstrap to SIGSEGV in tcallnode.replaceparaload via
  BoundToStaticForEachNodeAdapter. }
program method_pointer_field_assign;
{$mode objfpc}

type
  TFunc = function(x: longint): longint of object;

  TCtx = record
    f: TFunc;
    arg: pointer;
  end;

  TThing = class
    fname: longint;
    constructor Create(n: longint);
    function method(x: longint): longint;
  end;

constructor TThing.Create(n: longint);
begin
  fname := n;
end;

function TThing.method(x: longint): longint;
begin
  Result := fname + x;
end;

function adapter(arg: pointer): longint;
var
  ctx: ^TCtx absolute arg;
begin
  Result := ctx^.f(42);
end;

procedure runtest(f: TFunc);
var
  ctx: TCtx;
begin
  ctx.f := f;          { the parameter -> record-field case }
  ctx.arg := nil;
  writeln('result = ', adapter(@ctx));
end;

var
  t: TThing;
begin
  t := TThing.Create(100);
  runtest(@t.method);
end.
