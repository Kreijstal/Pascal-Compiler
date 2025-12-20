program interlocked_exchange_add;

var
  value: LongInt;
  old: LongInt;

begin
  value := 10;
  old := InterlockedExchangeAdd(value, 5);
  writeln(old);
  writeln(value);
end.
