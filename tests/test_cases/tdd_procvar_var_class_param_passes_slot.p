program tdd_procvar_var_class_param_passes_slot;
{$mode objfpc}

type
  TBox = class
    Value: LongInt;
    constructor Create(AValue: LongInt);
  end;

  TVisit = function(var Box: TBox; Arg: Pointer): LongInt;

constructor TBox.Create(AValue: LongInt);
begin
  inherited Create;
  Value := AValue;
end;

function Visit(var Box: TBox; Arg: Pointer): LongInt;
begin
  if Arg = nil then
    Box.Value := Box.Value + 5;
  Visit := Box.Value;
end;

procedure Perform(var Box: TBox; Visitor: TVisit);
begin
  Visitor(Box, nil);
end;

var
  Box: TBox;

begin
  Box := TBox.Create(37);
  Perform(Box, @Visit);
  writeln(Box.Value);
end.
