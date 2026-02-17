program test_nested_type_tdd;

{$mode objfpc}

type
  THeap = object
  type
    TMyHeader = record
      code: LongWord;
    end;
    PMyHeader = ^TMyHeader;
  end;

var
  buf: array[0..31] of Byte;
  p: Pointer;
  h: LongWord;

begin
  FillChar(buf, SizeOf(buf), 0);
  p := @buf[4];

  { Write through qualified nested type cast }
  THeap.PMyHeader(p)^.code := 42;

  { Read back }
  h := THeap.PMyHeader(p)^.code;

  if h <> 42 then
  begin
    writeln('FAIL: code mismatch, got ', h);
    halt(1);
  end;

  { Test with pointer to buffer start }
  THeap.PMyHeader(@buf[0])^.code := 99;
  h := THeap.PMyHeader(@buf[0])^.code;
  if h <> 99 then
  begin
    writeln('FAIL: buf[0] code mismatch, got ', h);
    halt(1);
  end;

  { Test with different offset }
  THeap.PMyHeader(@buf[8])^.code := 77;
  h := THeap.PMyHeader(@buf[8])^.code;
  if h <> 77 then
  begin
    writeln('FAIL: buf[8] code mismatch, got ', h);
    halt(1);
  end;

  writeln('PASS: nested type qualified typecast works');
end.
