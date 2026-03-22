{ Regression test: function pointer field chain access
  Verifies that chained field access through a function pointer
  field works: rec.FuncPtrField.ResultField

  This pattern is used in heaptrc.pp (prevMgr.GetFPCHeapStatus.CurrHeapUsed).
}
{$mode objfpc}
program regression_funcptr_field_chain;

type
  TStatus = record
    Value: LongInt;
    Name: String;
  end;

  TGetStatus = function: TStatus;

  TManager = record
    GetStatus: TGetStatus;
  end;

function MyGetStatus: TStatus;
begin
  Result.Value := 99;
  Result.Name := 'OK';
end;

var
  mgr: TManager;
begin
  mgr.GetStatus := @MyGetStatus;
  WriteLn('Value=', mgr.GetStatus().Value);
  WriteLn('Name=', mgr.GetStatus().Name);
end.
