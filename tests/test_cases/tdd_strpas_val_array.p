program tdd_strpas_val_array;

var
  Buffer: array[0..3] of AnsiChar;
  S: string;
  Value: LongInt;

begin
  Buffer[0] := '1';
  Buffer[1] := '2';
  Buffer[2] := '3';
  Buffer[3] := #0;
  S := StrPas(Buffer);
  Val(S, Value);
  Writeln(Value);
end.
