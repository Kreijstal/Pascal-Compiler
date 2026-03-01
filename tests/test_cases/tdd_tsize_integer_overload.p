{$mode objfpc}
program tdd_tsize_integer_overload;

{ TSize should resolve to an integer alias (like NativeUInt / QWord),
  not to a record type. Functions with TSize parameters should accept
  integer arguments. }

type
  TSize = QWord;

function DoRead(fd: LongInt; buf: PChar; nbytes: TSize): Int64;
begin
  { Just return nbytes to prove the overload resolved }
  Result := nbytes;
end;

function DoWrite(fd: LongInt; buf: PChar; nbytes: TSize): Int64;
begin
  Result := nbytes;
end;

function DoGetSize(buf: PChar; len: TSize): TSize;
begin
  Result := len;
end;

var
  buf: array[0..255] of Char;
  n: Integer;
  r: Int64;
  s: TSize;
begin
  n := 100;

  { Integer should be passable as TSize parameter }
  r := DoRead(0, @buf[0], n);
  writeln(r);

  { LongInt literal should be passable as TSize }
  r := DoWrite(1, @buf[0], 42);
  writeln(r);

  { TSize return value should be assignable to TSize variable }
  s := DoGetSize(@buf[0], 256);
  writeln(s);
end.
