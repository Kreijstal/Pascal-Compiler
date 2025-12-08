unit fpc_unit_exports;

interface

type
  TPoint = record
    x, y: Integer;
  end;

const
  PI = 3.14;

var
  Counter: Integer;

function GetX(const p: TPoint): Integer;

implementation

function GetX(const p: TPoint): Integer;
begin
  GetX := p.x;
end;

end.
