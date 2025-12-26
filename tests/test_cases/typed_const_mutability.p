program typed_const_mutability;

type
  TState = (stOff, stOn);

const
  Counter: Integer = 1;
  Flag: Boolean = False;
  State: TState = stOff;

begin
  Counter := Counter + 2;
  Flag := not Flag;
  State := stOn;
  WriteLn(Counter, ',', Flag, ',', Ord(State));
end.
