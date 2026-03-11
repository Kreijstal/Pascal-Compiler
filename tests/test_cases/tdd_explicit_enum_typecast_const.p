program tdd_explicit_enum_typecast_const;

{$mode objfpc}

type
  TReg = (
    TRegLow := Low(LongInt),
    TRegHigh := High(LongInt)
  );

const
  V = Ord(TReg($01050000));

begin
  WriteLn(V);
end.
