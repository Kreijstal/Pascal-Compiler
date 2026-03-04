program tdd_fpexecl_rawbytestring_array_literal;

uses unix;

var
  prog: RawByteString;

begin
  prog := 'echo bundled-stdlib';
  if false then
    fpexecl(PAnsiChar('/bin/sh'), ['-c', prog]);
  writeln('ok');
end.
