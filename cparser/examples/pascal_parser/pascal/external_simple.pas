program TestExternalSimple;

function SimpleExternal: integer; external;

var
  result: integer;

begin
  result := SimpleExternal;
end.