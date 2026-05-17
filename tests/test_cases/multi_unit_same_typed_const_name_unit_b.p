{$mode objfpc}
unit multi_unit_same_typed_const_name_unit_b;

{ Second helper: same shape, different data, SAME typed-const name
  (`mapentry`) and registers @mapentry to the same global linked list. }

interface

uses multi_unit_same_typed_const_name_unit_a;

implementation

const
  mapentry : tmapentry = (name : 'unit_b'; code : 22; next : nil);

begin
  registermap(@mapentry);
end.
