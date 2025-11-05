program array_record_real_coercion;

type
  rec = record
    r: real;
    i: integer;
  end;

const
  arr: array[1..2] of rec = (
    (r: 1; i: 2),
    (r: 3.0; i: 4)
  );

var
  value: real;

begin
  value := arr[1].r;
  writeln(value);
  writeln(arr[2].r);
end.
