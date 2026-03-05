program tdd_with_field_assign_vs_module_property;
{$mode objfpc}

type
  TProc = procedure(var X: LongInt);
  TThreadLike = record
    InitCriticalSection: TProc;
    SetThreadDebugNameU: TProc;
    Value: LongInt;
  end;

var
  R: TThreadLike;

procedure InitCriticalSection(var X: LongInt);
begin
  X := X + 1000;
end;

procedure SetThreadDebugNameU(var X: LongInt);
begin
  X := X + 2000;
end;

procedure CInitCriticalSection(var X: LongInt);
begin
  X := X + 2;
end;

procedure CSetThreadDebugNameU(var X: LongInt);
begin
  X := X + 1;
end;

begin
  R.Value := 10;
  with R do
  begin
    SetThreadDebugNameU := @CSetThreadDebugNameU;
    InitCriticalSection := @CInitCriticalSection;
  end;

  R.SetThreadDebugNameU(R.Value);
  R.InitCriticalSection(R.Value);
  WriteLn(R.Value);
end.
