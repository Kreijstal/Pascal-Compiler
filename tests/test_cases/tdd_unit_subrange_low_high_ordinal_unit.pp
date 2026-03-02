{$mode objfpc}
unit tdd_unit_subrange_low_high_ordinal_unit;

interface

type
  TShortCut = Low(Word)..High(Word);

function GetShortcut: TShortCut;

implementation

function GetShortcut: TShortCut;
begin
  Result := 123;
end;

end.
