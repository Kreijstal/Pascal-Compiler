{ Regression test for issue #490: enum overload not found when method
  has multiple overloads with different arity and default parameters.
  The compiler was pre-setting the mangled_id to the first overload
  found by semcheck_find_class_method, bypassing proper overload
  resolution for overloaded methods. }
program TestEnumOverloadCreateSection;
{$mode objfpc}

type
  TSectionType = (sec_none, sec_code, sec_data, sec_bss);
  TSectionOrder = (so_default, so_first, so_last);

  TObjSection = class
  end;

  TObjData = class
    function CreateSection(AType: TSectionType; const AName: String = ''; AOrder: TSectionOrder = so_default): TObjSection; overload; virtual;
    function CreateSection(const AName: String; AAlign: LongInt; AExtra: LongInt; AExtra2: LongInt): TObjSection; overload; virtual;
  end;

function TObjData.CreateSection(AType: TSectionType; const AName: String; AOrder: TSectionOrder): TObjSection;
begin
  WriteLn('enum: ', Ord(AType), ' order: ', Ord(AOrder));
  Result := nil;
end;

function TObjData.CreateSection(const AName: String; AAlign: LongInt; AExtra: LongInt; AExtra2: LongInt): TObjSection;
begin
  WriteLn('string: ', AName);
  Result := nil;
end;

var
  ObjData: TObjData;
begin
  ObjData := TObjData.Create;
  { This call should resolve to the enum overload with default params }
  ObjData.CreateSection(sec_code);
  ObjData.CreateSection(sec_data, 'myname');
  ObjData.CreateSection(sec_bss, 'test', so_last);
  ObjData.Free;
end.
