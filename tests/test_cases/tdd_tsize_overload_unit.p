{$mode objfpc}
unit tdd_tsize_overload_unit;

{ Unit that defines TSize as an integer alias and a function using it,
  simulating the BaseUnix.FpRead(fd, buf, TSize) pattern. }

interface

type
  TSize = QWord;

function UnitRead(fd: LongInt; buf: PChar; nbytes: TSize): Int64;
function UnitWrite(fd: LongInt; buf: PChar; nbytes: TSize): Int64;

implementation

function UnitRead(fd: LongInt; buf: PChar; nbytes: TSize): Int64;
begin
  Result := nbytes;
end;

function UnitWrite(fd: LongInt; buf: PChar; nbytes: TSize): Int64;
begin
  Result := nbytes;
end;

end.
