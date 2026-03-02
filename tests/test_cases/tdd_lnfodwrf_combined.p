{$mode objfpc}
program tdd_lnfodwrf_combined;

type
  TReader = object
    function Pos: longint;
  end;

function TReader.Pos: longint;
begin
  Pos := 42;
end;

var
  reader: TReader;
  Cache: array[0..1] of record
    addr: longint;
    name: string;
  end;

begin
  Cache[0].addr := 1;
  Cache[0].name := 'ok';
  if reader.Pos() = 42 then
    writeln(Cache[0].name);
end.
