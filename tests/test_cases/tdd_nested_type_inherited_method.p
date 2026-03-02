program tdd_nested_type_inherited_method;

type
  TOuter = object
    type
      TInner = object
      private
        x: Integer;
      public
        procedure Fix;
      end;
  end;

procedure TOuter.TInner.Fix;
begin
  x := 1;
end;

var
  v: TOuter.TInner;

begin
  v.Fix;
  writeln('ok');
end.
