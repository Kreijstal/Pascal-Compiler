unit TestExternalVariants;

interface

function SimpleExternal: integer; external;
function ExternalWithLibrary: integer; external 'kernel32';
function ExternalWithName: integer; external 'kernel32' name 'CreateFileA';
function ExternalWithIndex: integer; external 'kernel32' index 123;
function ExternalWithNameAndIndex: integer; external 'kernel32' name 'CreateFileA' index 456;

procedure SimpleProcExternal; external;
procedure ProcExternalWithLibrary: external 'user32';
procedure ProcExternalWithName: external 'user32' name 'MessageBoxA';
procedure ProcExternalWithIndex: external 'user32' index 789;
procedure ProcExternalWithNameAndIndex: external 'user32' name 'MessageBoxA' index 999;

implementation

end.