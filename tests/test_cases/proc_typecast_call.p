program proc_typecast_call;
{$mode objfpc}
{ Test procedural type typecast followed by call: TypeName(source)(args) 
  This pattern is used extensively in FPC's text.inc for FileFunc(TextRec(t).InOutFunc)(TextRec(t)) }

type
  TTextHandler = procedure(var value: Integer);
  TBinaryHandler = procedure(var a, b: Integer);
  
  TTextRec = record
    InOutFunc: Pointer;
    CloseFunc: Pointer;
    Value: Integer;
  end;

procedure DoWrite(var value: Integer);
begin
  value := value + 10;
end;

procedure DoClose(var value: Integer);
begin
  value := -1;
end;

procedure DoSwap(var a, b: Integer);
var tmp: Integer;
begin
  tmp := a;
  a := b;
  b := tmp;
end;

var
  rec: TTextRec;
  x, y: Integer;
begin
  rec.Value := 5;
  rec.InOutFunc := @DoWrite;
  rec.CloseFunc := @DoClose;
  
  { Test 1: Single-arg procedural typecast call }
  TTextHandler(rec.InOutFunc)(rec.Value);
  WriteLn(rec.Value);  { expect 15 }

  { Test 2: Different handler through same pattern }
  TTextHandler(rec.CloseFunc)(rec.Value);
  WriteLn(rec.Value);  { expect -1 }

  { Test 3: Two-arg procedural typecast call }
  x := 100;
  y := 200;
  TBinaryHandler(@DoSwap)(x, y);
  WriteLn(x);  { expect 200 }
  WriteLn(y);  { expect 100 }
end.
