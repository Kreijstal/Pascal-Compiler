unit fpc_pas_extension_unit;

interface

type
  TColor = (Red, Green, Blue);

const
  MaxValue = 100;

var
  GlobalCounter: Integer;

function ColorToString(c: TColor): String;

implementation

function ColorToString(c: TColor): String;
begin
  case c of
    Red:   ColorToString := 'Red';
    Green: ColorToString := 'Green';
    Blue:  ColorToString := 'Blue';
  end;
end;

end.
