{ Regression: `record DIV record` must dispatch to the user-defined
  `operator div` overload, not silently fall back to a 32-bit integer
  idivl on the first 4 bytes of the record (which previously crashed
  with SIGFPE for any record whose first 4 bytes contained a 0 in the
  divisor side).

  Mirror of record_op_overload_or.p but for integer DIV. Same FPC code
  pattern as Tconstexprint.div in compiler/constexp.pas — the operator
  was being silently skipped because semcheck used "op_div" as the
  method-name suffix for DIV, but encode_operator_name() in
  from_cparser_init_and_registry.c registers `div` as "op_intdiv"
  ("op_div" is reserved for the real-division `/` operator). }

program record_op_overload_div;

type
  TBits = record
    flag  : boolean;          { first 4 bytes are NOT the integer payload }
    value : qword;
  end;

operator div (const a, b : TBits) : TBits;
begin
  result.flag  := false;
  result.value := a.value div b.value;
end;

var
  x, y, z : TBits;
begin
  x.flag  := true;  x.value := qword($7fffffff);
  y.flag  := false; y.value := qword(8);
  z := x div y;
  WriteLn('flag=', z.flag, ' value=', z.value);
end.
