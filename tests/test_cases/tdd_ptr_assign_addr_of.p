program tdd_ptr_assign_addr_of;

{$mode objfpc}
{$modeswitch advancedrecords}


type
  TParam = record
    A: Integer;
  end;

  TRec = record
    Params: array[0..0] of TParam;
    function GetParams: PByte;
  end;

function TRec.GetParams: PByte;
begin
  Result := @Params;
end;

var
  R: TRec;

begin
  R.Params[0].A := 42;
  if PtrUInt(R.GetParams) <> 0 then
    Writeln('ok')
  else
    Writeln('bad');
end.
