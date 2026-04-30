program tdd_unit_class_constructor_impl_emits_body;

{$mode objfpc}

uses
  tdd_unit_class_constructor_impl_emits_body_unit;

var
  Builder: TConcreteBuilder;
  Info: TInfo;

begin
  Builder := TConcreteBuilder.Create;
  Info := Builder.MakeInfo(41);
  WriteLn(Info.Value);
end.
