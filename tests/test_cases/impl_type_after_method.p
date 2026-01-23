{ Test: Type declaration after method in implementation section }
{ Verifies parser handles type block after record helper method }
program impl_type_after_method;

type
  TGUID = record
    D1: LongWord;
  end;

  TGUIDHelper = record helper for TGUID
    function ToString: string;
  end;

function TGUIDHelper.ToString: string;
begin
  Result := 'GUID';
end;

type
  TTrimMode = (Left, Right, Both);

function AfterMethod: Integer;
begin
  Result := 42;
end;

var
  G: TGUID;
  T: TTrimMode;
begin
  G.D1 := 123;
  T := Both;
  WriteLn(G.ToString);
  WriteLn(AfterMethod);
end.
