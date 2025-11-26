program TestIf;

{$define MY_VERSION := 1}

begin
{$if MY_VERSION > 0}
  WriteLn('Version is greater than 0');
{$else}
  WriteLn('Version is not greater than 0');
{$endif}
end.
