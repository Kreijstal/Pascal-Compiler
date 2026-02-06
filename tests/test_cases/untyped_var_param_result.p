program untyped_var_param_result;

procedure CopyCount(var buffer; count: longint; var result: longint);
begin
  result := count;
end;

var
  buf: array[0..1] of byte;
  res: longint;

begin
  CopyCount(buf, 2, res);
  writeln(res);
end.
