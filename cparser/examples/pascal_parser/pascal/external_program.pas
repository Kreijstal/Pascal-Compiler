program TestExternalProgram;

function SimpleExternal: integer; external;
function ExternalWithLibrary: integer; external 'kernel32';
function ExternalWithName: integer; external 'kernel32' name 'CreateFileA';
function ExternalWithIndex: integer; external 'kernel32' index 123;

procedure SimpleProcExternal; external;
procedure ProcExternalWithLibrary; external 'user32';
procedure ProcExternalWithName; external 'user32' name 'MessageBoxA';
procedure ProcExternalWithIndex; external 'user32' index 789;

var
  result: integer;

begin
  result := SimpleExternal;
  result := ExternalWithLibrary;
  result := ExternalWithName;
  result := ExternalWithIndex;
  SimpleProcExternal;
  ProcExternalWithLibrary;
  ProcExternalWithName;
  ProcExternalWithIndex;
end.