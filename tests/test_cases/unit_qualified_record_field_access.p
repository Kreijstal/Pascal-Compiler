program unit_qualified_record_field_access;

{$mode objfpc}

uses unit_inner_type;

type
  TOuter = record
    Data: unit_inner_type.TInner;
  end;

var
  O: TOuter;

begin
  O.Data.Value := 42;
  writeln(O.Data.Value);
end.
