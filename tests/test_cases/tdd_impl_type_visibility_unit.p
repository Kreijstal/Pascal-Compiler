unit tdd_impl_type_visibility_unit;

{$mode objfpc}

interface

procedure TouchSplit;

implementation

type
  TSplitLocal = packed record
    cards: array[0..1] of cardinal;
    w: word;
  end;

procedure TouchSplit;
var
  x: Extended;
  y: Extended;
begin
  x := 0.0;
  y := 1.0;
  TSplitLocal(x).w := (TSplitLocal(x).w and $7fff) or (TSplitLocal(y).w and $8000);
end;

end.
