program tdd_types_self_field_shadow_import;

{$mode objfpc}
{$modeswitch advancedrecords}

uses
  Math,
  tdd_types_self_field_shadow_import_unit;

type
  TSize = record
    cx, cy: LongInt;
    function Distance(const asz: TSize): Double;
    function Add(const asz: TSize): TSize;
  end;

function TSize.Distance(const asz: TSize): Double;
begin
  Result := sqrt(sqr(cx - asz.cx) + sqr(cy - asz.cy));
end;

function TSize.Add(const asz: TSize): TSize;
begin
  Result.cx := cx + asz.cx;
  Result.cy := cy + asz.cy;
end;

var
  a, b, c: TSize;
begin
  a.cx := 3;
  a.cy := 4;
  b.cx := 1;
  b.cy := 2;
  c := a.Add(b);
  writeln(a.Distance(b):0:4);
  writeln(c.cx);
  writeln(c.cy);
end.
