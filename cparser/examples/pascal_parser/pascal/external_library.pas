program TestExternalLibrary;

function ExternalWithLibrary: integer; external 'kernel32';

var
  result: integer;

begin
  result := ExternalWithLibrary;
end.