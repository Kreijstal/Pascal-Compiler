unit tdd_unit_forward_class_inherited_field_unit;

{$mode objfpc}

interface

type
  TBase = class
    path: string;
    flag: boolean;
  end;

  TChild = class;

  TChild = class(TBase)
    own: LongInt;
  end;

implementation

end.
