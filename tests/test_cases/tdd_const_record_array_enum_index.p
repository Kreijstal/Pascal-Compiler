{ TDD: Const array of records indexed by enum type
  Pattern from FPC compiler: embedded_controllers : array[tcontrollertype] of tcontrollerdatatype
  Error: "must use tuple syntax for its initializer" }
program tdd_const_record_array_enum_index;

type
  TControllerType = (ct_none, ct_alpha, ct_beta);

  TControllerData = record
    Name: string[20];
    ID: LongInt;
  end;

const
  Controllers: array[TControllerType] of TControllerData = (
    (Name: ''; ID: 0),
    (Name: 'Alpha'; ID: 1),
    (Name: 'Beta'; ID: 2)
  );

begin
  WriteLn(Controllers[ct_none].Name, ':', Controllers[ct_none].ID);
  WriteLn(Controllers[ct_alpha].Name, ':', Controllers[ct_alpha].ID);
  WriteLn(Controllers[ct_beta].Name, ':', Controllers[ct_beta].ID);
end.
