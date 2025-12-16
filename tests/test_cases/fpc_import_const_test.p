{ Test program for constant aliasing between units }
program fpc_import_const_test;

uses fpc_import_const_alias;

begin
  writeln('ALIASED_BASE = ', ALIASED_BASE);
  writeln('ALIASED_ANOTHER = ', ALIASED_ANOTHER);
end.
