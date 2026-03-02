program tdd_nested_type_param;
{$mode objfpc}

type
  TOuter = class
    type
      TNested = record
        v: longint;
      end;
    class function Make(const x: TNested): longint; static;
  end;

class function TOuter.Make(const x: TNested): longint;
begin
  Result := x.v + 1;
end;

var
  r: TOuter.TNested;

begin
  r.v := 41;
  writeln(TOuter.Make(r));
end.
