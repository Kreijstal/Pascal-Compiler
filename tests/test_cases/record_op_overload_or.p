{ Regression: `record OR record` must dispatch to the user-defined
  `operator or` overload, not silently fall back to a 32-bit bitwise OR
  on the first 4 bytes of the record (which previously produced 0 for
  FPC's Tconstexprint pattern: a record whose first field is a boolean). }

program record_op_overload_or;

type
  TBits = record
    flag  : boolean;          { lays bytes ahead of value -> first 4 bytes
                                of the record are NOT the integer payload }
    value : qword;
  end;

operator or (const a, b : TBits) : TBits;
begin
  result.flag  := a.flag or b.flag;
  result.value := a.value or b.value;
end;

var
  x, y, z : TBits;
begin
  x.flag  := false; x.value := qword($1000);
  y.flag  := true;  y.value := qword($2000);
  z := x or y;
  WriteLn('flag=', z.flag, ' value=', z.value);
end.
