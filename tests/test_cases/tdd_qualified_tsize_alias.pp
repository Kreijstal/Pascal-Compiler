{$mode objfpc}
unit tdd_qualified_tsize_alias;

interface

uses tdd_qualified_tsize_base;

type
  TSize = tdd_qualified_tsize_base.TSize;

function UnitRead(fd: LongInt; buf: PChar; nbytes: TSize): Int64;

implementation

function UnitRead(fd: LongInt; buf: PChar; nbytes: TSize): Int64;
begin
  Result := nbytes;
end;

end.
