{ FPC Bootstrap Gap: MaxLongint and other integer boundary constants
  FPC's objpas.pp uses:
    const MaxInt = MaxLongint;
  This requires built-in integer boundary constants.
}
program fpc_maxlongint;

const
  MAX_VAL = MaxLongint;  { Should be 2147483647 }
  MIN_VAL = -MaxLongint - 1;  { Should be -2147483648 }

begin
  writeln('MaxLongint = ', MAX_VAL);
  writeln('MinLongint = ', MIN_VAL);
end.
