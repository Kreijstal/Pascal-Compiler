{$mode objfpc}
program test_funcvar_enum_return;
type
  TResult = (resA, resB, resC);
  TMyFunc = function(x: Integer; p: Pointer): TResult;

  TProcessor = class
    f: TMyFunc;
    procedure Process;
  end;

function MyFunc(x: Integer; p: Pointer): TResult;
begin
  if x > 0 then Result := resB
  else Result := resA;
end;

procedure TProcessor.Process;
var
  fr: TResult;
begin
  fr := f(1, nil);
  WriteLn(Ord(fr));
end;

var
  proc: TProcessor;
begin
  proc := TProcessor.Create;
  proc.f := @MyFunc;
  proc.Process;
  proc.Free;
end.
