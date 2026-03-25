unit tdd_ptr_deref_field_helper_unit;
{$mode objfpc}{$H+}

interface

uses SysUtils;

type
  { Forward-declared pointer: pointer type declared BEFORE the record }
  PCGParaLocation = ^TCGParaLocation;
  TCGParaLocation = record
    Next: PCGParaLocation;
    Size: LongInt;
    case LongInt of
      0: (offset: LongInt);
      1: (shiftval: ShortInt;
          register_num: LongInt);
  end;

  { Object type like FPC's TCGPara }
  TCGPara = object
    Location: PCGParaLocation;
    IntSize: LongInt;
  end;

implementation

end.
