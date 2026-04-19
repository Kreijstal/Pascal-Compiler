program fpc_bootstrap_sized_string_array_concat;

type
  TKind = (ka, kb);

const
  Names: array[TKind] of string[16] = ('ATHLON64', 'X86-64');

begin
  WriteLn('CPU: ', Names[ka]);
  WriteLn('Target=' + Names[kb]);
end.
