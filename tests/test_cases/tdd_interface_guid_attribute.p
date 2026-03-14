program tdd_interface_guid_attribute;

{$mode objfpc}
{$interfaces com}

uses
  sysutils;

type
  ITest = interface
    ['{D0395F17-52AA-4515-93A5-5B292F03AA7B}']
    function GetValue: Integer;
  end;

begin
  Writeln('ok');
end.
