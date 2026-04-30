program tdd_enum_longint_range_uint32_typecast_wrap;
{$mode objfpc}

type
  TRegister = (
    TRegisterLowEnum := Low(LongInt),
    TRegisterHighEnum := High(LongInt)
  );

const
  WrappedRegister = Ord(TRegister($FFFFFFFF));

begin
  writeln(WrappedRegister);
end.
