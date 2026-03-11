program tdd_const_array_single_record_element;

type
  TControllerType = (ctNone);
  TControllerData = record
    ControllerTypeStr: string[20];
    ControllerUnitStr: string[20];
    Value: LongInt;
  end;

const
  EmbeddedControllers: array[TControllerType] of TControllerData =
  (
    (ControllerTypeStr:''; ControllerUnitStr:'cpu'; Value:42)
  );

begin
  WriteLn(EmbeddedControllers[ctNone].ControllerUnitStr);
  WriteLn(EmbeddedControllers[ctNone].Value);
end.
