{$mode objfpc}
program shortstring_field_assign;

type
    TStoredAttribute = object
        FName: ShortString;
        procedure SetName(const aName: ShortString);
        function NameValue: String;
    end;

procedure TStoredAttribute.SetName(const aName: ShortString);
begin
    FName := aName;
end;

function TStoredAttribute.NameValue: String;
begin
    Result := FName;
end;

begin
end.
