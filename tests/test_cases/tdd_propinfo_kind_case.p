program tdd_propinfo_kind_case;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TTypeKind = (tkWString, tkUString);
  PTypeInfo = ^TTypeInfo;
  TTypeInfo = record
    Kind: TTypeKind;
  end;

  TPropInfo = record
  private
    function GetPropType: PTypeInfo;
  public
    property PropType: PTypeInfo read GetPropType;
  end;
  PPropInfo = ^TPropInfo;

var
  GTypeInfo: TTypeInfo;
  P: TPropInfo;

function TPropInfo.GetPropType: PTypeInfo;
begin
  Result := @GTypeInfo;
end;

procedure Test(PropInfo: PPropInfo);
begin
  case PropInfo^.PropType^.Kind of
    tkWString: ;
    tkUString: ;
  end;
end;

begin
  GTypeInfo.Kind := tkUString;
  Test(@P);
  writeln('ok');
end.
