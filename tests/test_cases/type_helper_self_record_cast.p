{$mode objfpc}
{$modeswitch typehelpers}
{ Test: Type helper Self to record cast }
{ This test checks that Self can be cast to a record type within a type helper method }
program type_helper_self_record_cast;

type
  TSingleRec = packed record
    Data: LongWord;
  end;

  TSingleHelper = Type Helper for Single
    Function GetData: LongWord;
  end;

Function TSingleHelper.GetData: LongWord;
begin
  Result := TSingleRec(Self).Data;
end;

var
  S: Single;
  D: LongWord;
begin
  S := 1.0;
  D := S.GetData;
  WriteLn(D);
end.
