{$mode objfpc}
unit multi_unit_same_typed_const_name_unit_b;

interface

uses multi_unit_same_typed_const_name_unit_a;

implementation

const
  mymap : tmymap = (name : 'two'; cp : 2; next : nil);

begin
  registermymap(@mymap);
end.
