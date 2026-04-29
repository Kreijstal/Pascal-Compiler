program tdd_inherited_classvar_class_constructor_initializes_base_factory;

{$mode objfpc}

type
  TInfo = class
  public
    Value: LongInt;
    constructor Create(AValue: LongInt);
  end;

  TInfoClass = class of TInfo;

  TBaseBuilder = class
  protected class var
    InfoClass: TInfoClass;
  public
    function MakeInfo(AValue: LongInt): TInfo;
  end;

  TConcreteBuilder = class(TBaseBuilder)
  public
    class constructor ClassCreate;
  end;

constructor TInfo.Create(AValue: LongInt);
begin
  Value := AValue;
end;

function TBaseBuilder.MakeInfo(AValue: LongInt): TInfo;
begin
  MakeInfo := InfoClass.Create(AValue);
end;

class constructor TConcreteBuilder.ClassCreate;
begin
  InfoClass := TInfo;
end;

var
  Builder: TConcreteBuilder;
  Info: TInfo;

begin
  Builder := TConcreteBuilder.Create;
  Info := Builder.MakeInfo(37);
  WriteLn(Info.Value);
end.
