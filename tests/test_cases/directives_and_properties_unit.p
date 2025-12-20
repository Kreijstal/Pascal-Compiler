unit directives_and_properties_unit;

interface

function GetUnitValue: LongInt;
procedure SetUnitValue(v: LongInt);
property UnitValue: LongInt read GetUnitValue write SetUnitValue;

procedure AddUnitValue(delta: LongInt); [cdecl];

implementation

var
  UnitValueStore: LongInt = 10;

function GetUnitValue: LongInt;
begin
  GetUnitValue := UnitValueStore;
end;

procedure SetUnitValue(v: LongInt);
begin
  UnitValueStore := v;
end;

procedure AddUnitValue(delta: LongInt); [cdecl];
begin
  UnitValueStore := UnitValueStore + delta;
end;

end.
