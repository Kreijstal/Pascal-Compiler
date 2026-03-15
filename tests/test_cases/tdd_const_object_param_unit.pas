{$mode objfpc}
unit tdd_const_object_param_unit;

interface

type
  TThing = object
    Value: LongInt;
    procedure Init;
  end;

procedure UseThing(const AThing: TThing);

implementation

procedure TThing.Init;
begin
  Value := 42;
end;

procedure UseThing(const AThing: TThing);
begin
  WriteLn(AThing.Value);
end;

end.
