unit unit_nested_record;

{$mode objfpc}

interface

type
  TInner = record
    Value: Integer;
  end;

  TOuter = record
    Inner: TInner;
  end;

var
  Outer: TOuter;

implementation

end.
