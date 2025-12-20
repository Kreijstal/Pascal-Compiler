{ Test: Unit with dereferenced pointer field set to nil }
unit unit_pointer_deref_nil;

{$mode objfpc}

interface

type
  PMyRecord = ^TMyRecord;
  TMyRecord = record
    Value: Integer;
    Next: PMyRecord;
  end;

function CreateRecord(val: Integer): PMyRecord;

implementation

function CreateRecord(val: Integer): PMyRecord;
var
  p: PMyRecord;
begin
  New(p);
  p^.Value := val;
  p^.Next := nil;
  CreateRecord := p;
end;

end.
