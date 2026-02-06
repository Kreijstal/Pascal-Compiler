{$mode objfpc}
program reg_sysutils_inttohex_binstr;

function IntToHex(i: LongInt; digits: Integer): string; overload;
begin
  IntToHex := 'L';
end;

function IntToHex(i: Int64; digits: Integer): string; overload;
begin
  IntToHex := 'I';
end;

function BinStr(i: LongInt; digits: Byte): string; overload;
begin
  BinStr := 'L';
end;

function BinStr(i: Int64; digits: Byte): string; overload;
begin
  BinStr := 'I';
end;

var
  i: Integer;
begin
  i := 10;
  Writeln(IntToHex(i, 2));
  Writeln(IntToHex(10, 2));
  Writeln(BinStr(i, 4));
end.
