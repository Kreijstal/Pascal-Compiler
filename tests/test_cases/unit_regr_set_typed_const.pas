unit unit_regr_set_typed_const;

{$mode objfpc}

interface

type
  TColor = (red, green, blue);

const
  Primary : set of TColor = [red, green, blue];

function HasRed : boolean;

implementation

function HasRed : boolean;
begin
  HasRed := red in Primary;
end;

end.
