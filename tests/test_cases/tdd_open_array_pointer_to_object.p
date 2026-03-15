{$mode objfpc}
program tdd_open_array_pointer_to_object;

type
  TThing = object
    Value: LongInt;
  end;
  PThing = ^TThing;

procedure UseThings(const Things: array of PThing);
begin
  WriteLn(Things[0]^.Value);
end;

var
  Thing: TThing;
begin
  Thing.Value := 7;
  UseThings([@Thing]);
end.
