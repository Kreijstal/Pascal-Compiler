{ Regression: open array descriptor register allocation under register pressure.
  Exercises the [s] inline open-array construct inside a procedure that has
  several arguments live simultaneously, stressing the codegen path that used
  to fail with "Unable to allocate register for open array descriptor". }
program open_array_reg_pressure;

procedure Print(const arr: array of string);
begin
  writeln(arr[0]);
end;

procedure Msg(a, b, c: longint; const s: string);
begin
  { With a, b, c occupying registers plus the implicit self-pointer and s,
    allocating a second register for the open-array descriptor was failing. }
  Print([s]);
end;

begin
  Msg(1, 2, 3, 'hello');
end.
