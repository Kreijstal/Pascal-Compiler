program open_array_length;

procedure DumpLen(const labelStr: String; const data: array of Integer);
begin
  writeln(labelStr, ':', Length(data));
end;

type
  TIntSlice = array[3..6] of Integer;

var
  local: array[1..4] of Integer;
  typed: TIntSlice;
  dyn: array of Integer;
begin
  local[1] := 10;
  local[2] := 20;
  local[3] := 30;
  local[4] := 40;

  typed[3] := 5;
  typed[4] := 6;
  typed[5] := 7;
  typed[6] := 8;

  SetLength(dyn, 5);

  DumpLen('static', local);
  DumpLen('typed', typed);
  DumpLen('dynamic', dyn);
end.
