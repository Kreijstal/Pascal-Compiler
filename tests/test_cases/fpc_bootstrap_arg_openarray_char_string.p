program fpc_bootstrap_arg_openarray_char_string;

{$mode objfpc}

procedure TakesChars(const S: array of Char);
var
  i: Integer;
begin
  write('len=', Length(S));
  for i := 0 to High(S) do
    write(S[i]);
  writeln;
end;

begin
  TakesChars('abc');
end.
