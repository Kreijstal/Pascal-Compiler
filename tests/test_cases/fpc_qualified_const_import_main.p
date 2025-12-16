{ Test program that uses qualified constants from imported unit }
program qualified_const_import_main;

uses fpc_qualified_const_import;

const
  { This is the problematic pattern: referencing an imported constant with unit prefix }
  ALIASED_CONST = fpc_qualified_const_import.LOCAL_CONST;

begin
  writeln('LOCAL_CONST = ', fpc_qualified_const_import.LOCAL_CONST);
  writeln('ALIASED_CONST = ', ALIASED_CONST);
end.
