program fpc_bootstrap_absolute_record_field;

{$mode objfpc}

type
  TPair = record
    First: LongInt;
    Second: LongInt;
  end;

var
  Pair: TPair;
  AliasSecond: LongInt absolute Pair.Second;

begin
  Pair.First := 10;
  Pair.Second := 20;
  writeln('start=', Pair.First, ',', Pair.Second, ',', AliasSecond);
  AliasSecond := 99;
  writeln('after=', Pair.First, ',', Pair.Second, ',', AliasSecond);
end.
