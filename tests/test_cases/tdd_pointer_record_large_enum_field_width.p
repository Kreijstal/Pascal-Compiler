program tdd_pointer_record_large_enum_field_width;
{$mode objfpc}

type
  TRegister = (
    TRegisterLowEnum := Low(LongInt),
    TRegisterHighEnum := High(LongInt)
  );

  TRegisterRec = packed record
    SupReg: Word;
    SubReg: Byte;
    RegType: Byte;
  end;

  PCallLocation = ^TCallLocation;
  TCallLocation = record
    RegisterValue: TRegister;
    Next: PCallLocation;
    Loc: LongInt;
  end;

function NewReg(SupReg: Word; SubReg: Byte; RegType: Byte): TRegister;
var
  R: TRegisterRec;
begin
  R.SupReg := SupReg;
  R.SubReg := SubReg;
  R.RegType := RegType;
  NewReg := TRegister(R);
end;

function GetRegType(R: TRegister): LongInt;
var
  Parts: TRegisterRec absolute R;
begin
  GetRegType := Parts.RegType;
end;

var
  Location: TCallLocation;
  LocationPtr: PCallLocation;

begin
  Location.RegisterValue := NewReg(4, 5, 6);
  LocationPtr := @Location;
  writeln(GetRegType(LocationPtr^.RegisterValue));
end.
