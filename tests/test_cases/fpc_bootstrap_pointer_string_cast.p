{$mode objfpc}
{$H+}
program fpc_bootstrap_pointer_string_cast;

const
  libc = 'c';

function strlen(p: Pointer): NativeUInt; cdecl; external libc name 'strlen';

var
  A: AnsiString;
  R: RawByteString;

begin
  A := 'abc';
  R := 'wxyz';
  WriteLn(strlen(Pointer(A)));
  WriteLn(strlen(Pointer(R)));
end.
