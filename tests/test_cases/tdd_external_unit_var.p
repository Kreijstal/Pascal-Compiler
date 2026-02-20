unit tdd_external_unit_var;

interface

var
  extvalue: longint; external name 'errno';

implementation

initialization
  if extvalue = 0 then
    extvalue := 1;
end.
