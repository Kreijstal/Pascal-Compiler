program TddMethodCallbackParameter;
{$mode objfpc}

type
  TCallback = procedure(Data, Arg: Pointer) of object;
  TStaticCallback = procedure(Data, Arg: Pointer);

  TRunner = class
    Value: LongInt;
    procedure Hit(Data, Arg: Pointer);
    procedure ForEachCall(Proc2Call: TCallback; Arg: Pointer);
    procedure ForEachCall(Proc2Call: TStaticCallback; Arg: Pointer);
  end;

procedure TRunner.Hit(Data, Arg: Pointer);
begin
  Value := Value + PtrUInt(Data) + PtrUInt(Arg);
end;

procedure TRunner.ForEachCall(Proc2Call: TCallback; Arg: Pointer);
begin
  Proc2Call(Pointer(10), Arg);
end;

procedure TRunner.ForEachCall(Proc2Call: TStaticCallback; Arg: Pointer);
begin
  Proc2Call(Pointer(100), Arg);
end;

var
  Runner: TRunner;
begin
  Runner := TRunner.Create;
  Runner.Value := 1;
  Runner.ForEachCall(@Runner.Hit, Pointer(20));
  Writeln(Runner.Value);
end.
