{$mode objfpc}
program tdd_interface_inherit_guid_decl_section;

type
  IInterfaceList = interface ['{285DEA8A-B865-11D1-AAA7-00C04FB17A72}']
    function Get(i: Integer): Integer;
    procedure Put(i: Integer; Item: Integer);
    property Items[i: Integer]: Integer read Get write Put; default;
  end;

  IInterfaceListEx = interface(IInterfaceList) ['{FDB39D70-65B9-4995-9436-6084ACA05DB3}']
    function GetCount: Integer;
  end;

begin
  WriteLn(2);
end.
