program tdd_nested_return_type;

type
  TOuter = object
  type
    PFoo = ^TFoo;
    TFoo = record
      x: integer;
    end;

    TInner = object
      function Find: PFoo;
    end;
  end;

function TOuter.TInner.Find: PFoo;
var
  bins: array[0..0] of PFoo;
begin
  Find := bins[0];
end;

var
  inner: TOuter.TInner;

begin
  writeln('ok');
end.
