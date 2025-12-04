program forward_type_high_const;
{$mode objfpc}

{ Test: High/Low with type aliases to primitive types }
{ The type must be declared before the const for this to work }

type
  MyInt64 = Int64;
  MyInteger = Integer;

const
  MaxMyInt64 = High(MyInt64);
  MinMyInt64 = Low(MyInt64);
  MaxMyInteger = High(MyInteger);
  MinMyInteger = Low(MyInteger);

begin
  writeln('MaxMyInt64 = ', MaxMyInt64);
  writeln('MinMyInt64 = ', MinMyInt64);
  writeln('MaxMyInteger = ', MaxMyInteger);
  writeln('MinMyInteger = ', MinMyInteger);
end.
