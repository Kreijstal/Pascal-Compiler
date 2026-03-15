program tdd_fpc_feature_unicodestrings_with_field_assign;
{$mode objfpc}

type
  TProc = procedure(var X: LongInt);
  TRec = record
{$ifdef FPC_HAS_FEATURE_UNICODESTRINGS}
    SetThreadDebugNameU: TProc;
{$endif}
    Value: LongInt;
  end;

var
  R: TRec;

procedure CSetThreadDebugNameU(var X: LongInt);
begin
  X := X + 1;
end;

begin
  R.Value := 10;
  with R do
    SetThreadDebugNameU := @CSetThreadDebugNameU;
  R.SetThreadDebugNameU(R.Value);
  WriteLn(R.Value);
end.
