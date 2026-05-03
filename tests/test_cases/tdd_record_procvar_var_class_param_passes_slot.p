program tdd_record_procvar_var_class_param_passes_slot;
{$mode objfpc}

type
  TBox = class
    Value: LongInt;
    constructor Create(AValue: LongInt);
  end;

  TVisit = function(var Box: TBox; Arg: Pointer): LongInt;

  TContext = record
    Visitor: TVisit;
    Arg: Pointer;
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

procedure Perform(var Box: TBox; var Context: TContext);
begin
  Context.Visitor(Box, Context.Arg);
end;

var
  Box: TBox;
  Context: TContext;

begin
  Box := TBox.Create(37);
  Context.Visitor := @Visit;
  Context.Arg := nil;
  Perform(Box, Context);
  writeln(Box.Value);
end.
