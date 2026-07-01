{ Regression: `for x in <open-array param>` (and `for x in <dynamic array>`)
  must iterate over the runtime length of the array.

  KGPC's for-in codegen used the array type's STATIC end_index as the loop
  bound.  For an open array (and a dynamic array) the length is only known at
  runtime -- the static bounds are encoded as end_index < start_index -- so the
  loop compared the index against a bogus negative bound and iterated ZERO
  times.  This silently broke FPC's x86 peephole optimiser: its
  MatchInstruction(... ; const ops: array of TAsmOp; ...) iterates the opcode
  list with `for op in ops`, always returned False, so the "shift count must be
  CL" guard in ReplaceRegisterInInstruction was skipped and a non-CL shift
  count was emitted -> "shl reg,reg8 invalid combination" while bootstrapping
  the RTL at -O2.

  Expected: the open-array search finds A_SHL in the shift list (the guard
  stays protected), and both open- and dynamic-array sums match. }
program for_in_open_array_runtime_length;

type
  TAsmOp = (A_NONE, A_MOV, A_ADD, A_SHL, A_SHR, A_SAR, A_RCL, A_ROL);

function OpInList(target: TAsmOp; const ops: array of TAsmOp): boolean;
var
  op: TAsmOp;
begin
  OpInList := false;
  for op in ops do
    if op = target then
    begin
      OpInList := true;
      exit;
    end;
end;

function SumOpen(const a: array of longint): longint;
var
  x, s: longint;
begin
  s := 0;
  for x in a do
    s := s + x;
  SumOpen := s;
end;

var
  dyn: array of longint;
  x, s: longint;
begin
  { Open array of enums: the MatchInstruction pattern. }
  if OpInList(A_SHL, [A_RCL, A_ROL, A_SAR, A_SHL, A_SHR]) then
    writeln('SHL found')
  else
    writeln('SHL MISSING');
  if OpInList(A_MOV, [A_RCL, A_ROL, A_SAR, A_SHL, A_SHR]) then
    writeln('MOV found')
  else
    writeln('MOV absent');

  { Open array of integers. }
  writeln('open sum=', SumOpen([10, 20, 30, 40]));

  { Dynamic array. }
  SetLength(dyn, 4);
  dyn[0] := 1; dyn[1] := 2; dyn[2] := 3; dyn[3] := 4;
  s := 0;
  for x in dyn do
    s := s + x;
  writeln('dyn sum=', s);
end.
