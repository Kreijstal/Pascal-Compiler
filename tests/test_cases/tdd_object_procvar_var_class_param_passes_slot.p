program tdd_object_procvar_var_class_param_passes_slot;
{$mode objfpc}

type
  TBox = class
    Value: LongInt;
    constructor Create(AValue: LongInt);
  end;

  TVisit = function(var Box: TBox; Arg: Pointer): LongInt;

  TContext = object
    Visitor: TVisit;
    Arg: Pointer;
    procedure Perform(var Box: TBox);
  end;

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

procedure TContext.Perform(var Box: TBox);
begin
  Visitor(Box, Arg);
end;

var
  Box: TBox;
  Context: TContext;

begin
  Box := TBox.Create(37);
  Context.Visitor := @Visit;
  Context.Arg := nil;
  Context.Perform(Box);
  writeln(Box.Value);
end.
