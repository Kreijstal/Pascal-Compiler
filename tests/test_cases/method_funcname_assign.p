program test_method_funcname_assign;
{$mode objfpc}

type
  TCounter = class
    FCount: LongInt;
    function GetCount: LongInt;
    function Increment: LongInt;
  end;

function TCounter.GetCount: LongInt;
begin
  GetCount := FCount;
end;

function TCounter.Increment: LongInt;
begin
  FCount := FCount + 1;
  Increment := FCount;
end;

var
  c: TCounter;
begin
  c := TCounter.Create;
  c.FCount := 10;
  WriteLn(c.GetCount);
  WriteLn(c.Increment);
  WriteLn(c.Increment);
  WriteLn(c.GetCount);
end.
