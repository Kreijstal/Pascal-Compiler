{$mode objfpc}
program ptr_record_shortstring_result;

type
    TKind = (tkClass, tkInt64);
    TClassTypeInfo = packed record
        case TKind of
            tkClass: (UnitName: ShortString);
            tkInt64: (Dummy: Int64);
    end;
    PClassTypeInfo = ^TClassTypeInfo;

function GetUnitName(p: PClassTypeInfo): String;
begin
    Result := p^.UnitName;
end;

var
    Info: TClassTypeInfo;
begin
    Info.UnitName := 'objpas';
    WriteLn(GetUnitName(@Info));
end.
