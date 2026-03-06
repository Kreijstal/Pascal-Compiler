program tdd_method_named_allocmem;
{$mode delphi}

type
  TRec = record
    function AllocMem(Size: LongInt): LongInt;
  end;

function TRec.AllocMem(Size: LongInt): LongInt;
begin
  Result := Size + 1;
end;

var
  R: TRec;
begin
  Writeln(R.AllocMem(41));
end.
