unit reg_imported_anonymous_enum_set_unit;

{$mode objfpc}

interface

type
  TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);

function HasIgnoreCase(Flags: TReplaceFlags): Boolean;

implementation

function HasIgnoreCase(Flags: TReplaceFlags): Boolean;
begin
  Result := rfIgnoreCase in Flags;
end;

end.
