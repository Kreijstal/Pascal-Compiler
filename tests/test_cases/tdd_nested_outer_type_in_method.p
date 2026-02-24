program tdd_nested_outer_type_in_method;

type
  TOuter = object
    type
      TThing = record
        x: Integer;
      end;
      PThing = ^TThing;
      TInner = object
        procedure Fix;
      end;
  end;

procedure TOuter.TInner.Fix;
var
  p: PThing;
  v: TThing;
begin
  v.x := 1;
  p := @v;
  p^.x := 2;
end;

var
  i: TOuter.TInner;

begin
  i.Fix;
  writeln('ok');
end.
