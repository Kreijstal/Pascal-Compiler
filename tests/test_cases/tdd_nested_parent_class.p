program tdd_nested_parent_class;
{$mode objfpc}

type
  TOuter = object
    type
      TBase = object
        x: longint;
      end;
      TChild = object(TBase)
        y: longint;
      end;
  end;

var
  c: TOuter.TChild;

begin
  c.x := 1;
  c.y := 2;
  writeln(c.x + c.y);
end.
