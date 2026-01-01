program interlocked_exchange_add_stmt;

var
  value: LongInt;

begin
  value := 1;
  InterlockedExchangeAdd(value, 2);
  writeln(value);
end.
