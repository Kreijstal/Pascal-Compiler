{$mode objfpc}
program tdd_lnfodwrf_readaddress;

{ TDD repro: inside an object method, the method name used as the
  result variable should be passable as a var parameter and to sizeof.
  This is the pattern from lnfodwrf.pp TEReader.ReadAddress:
    ReadNext(ReadAddress, sizeof(ReadAddress));
}

type
  SizeInt = Int64;

  TEReader = object
    function ReadNext(var dest; size: SizeInt): Boolean;
    function ReadAddress(addr_size: smallint): Int64;
  end;

function TEReader.ReadNext(var dest; size: SizeInt): Boolean;
var
  p: PByte;
begin
  p := @dest;
  p^ := 42;
  ReadNext := true;
end;

function TEReader.ReadAddress(addr_size: smallint): Int64;
begin
  ReadAddress := 0;
  ReadNext(ReadAddress, sizeof(ReadAddress));
end;

var
  er: TEReader;
begin
  if er.ReadAddress(8) = 42 then
    writeln('ok')
  else
    writeln('FAIL');
end.
