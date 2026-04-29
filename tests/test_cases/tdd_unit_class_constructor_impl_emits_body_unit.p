unit tdd_unit_class_constructor_impl_emits_body_unit;

{$mode objfpc}

interface

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

implementation

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

end.
