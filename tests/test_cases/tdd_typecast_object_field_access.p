{$mode objfpc}
program tdd_typecast_object_field_access;

type
  TBase = object
  end;

  TChild = object(TBase)
    Buf: PChar;
  end;

var
  C: TChild;
  B: TBase;
begin
  C.Buf := 'abc';
  B := C;
  WriteLn(Assigned(TChild(B).Buf));
end.
