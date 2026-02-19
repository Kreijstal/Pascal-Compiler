{$mode objfpc}
program tdd_interface_guid_decl_section;

type
  IFoo = interface ['{12345678-1234-1234-1234-1234567890AB}']
    function GetValue: Integer;
    property Value: Integer read GetValue;
  end;

  IBar = interface ['{87654321-4321-4321-4321-BA0987654321}']
    procedure Ping;
  end;

begin
  WriteLn(1);
end.
