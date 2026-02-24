program tdd_nested_parent_class_min;

type
  TOuter = object
    type
      TBase = object
        x: Integer;
      end;
      TChild = object(TBase)
      end;
  end;

var
  c: TOuter.TChild;

begin
  c.x := 3;
  writeln(c.x);
end.
