unit tdd_imported_array_bound_field_layout;

interface

uses tdd_imported_array_bound_field_bounds;

type
  TRegisterSet = set of 0..MaxRegister;

  TRegisterState = class
    Count: Word;
    First: TRegisterNumber;
    Registers: array[0..MaxRegister] of TRegisterNumber;
    Seen: TRegisterSet;
  end;

implementation

end.
