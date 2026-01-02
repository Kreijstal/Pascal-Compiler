program type_helper_basic;

{$mode objfpc}
{$modeswitch typehelpers}

type
  TLongintHelper = type helper for Longint
    function AddTwo: Longint;
  end;

function TLongintHelper.AddTwo: Longint;
begin
  Result := Self + 2;
end;

var
  x: Longint;

begin
  x := 5;
  writeln(x.AddTwo);
end.
