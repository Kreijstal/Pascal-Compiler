program rtl_core10_demo;
type
  CharArray = array[0..7] of char;
var
  dyn: array of longint;
  text: string;
  buffer: CharArray;
  idx: integer;
  code: integer;
  converted: string;
  memPtr: Pointer;
begin
  SetLength(dyn, 4);
  dyn[0] := 11;
  dyn[3] := 44;
  writeln('DynLen=', Length(dyn));

  text := 'hi';
  SetLength(text, 5);
  text[5] := '!';
  writeln('StrLen=', Length(text));

  writeln('Pos=', Pos('run', 'pascal runtime'));

  Str(12345, converted);
  writeln('StrVal=', converted);

  Val('77', idx, code);
  writeln('ValOut=', idx, ' Code=', code);

  memPtr := nil;
  writeln('AssignedNil=', Assigned(memPtr));
  GetMem(memPtr, 8);
  writeln('AssignedPtr=', Assigned(memPtr));

  FillChar(buffer, SizeOf(buffer), Ord('Z'));
  writeln('FillChar=', buffer[0], ',', buffer[7]);

  ReallocMem(memPtr, 16);
  FreeMem(memPtr);
  memPtr := nil;
  writeln('AssignedAfterFree=', Assigned(memPtr));
end.
