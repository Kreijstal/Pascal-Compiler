program tdd_pointer_deref_field_vs_helper;
{$mode objfpc}{$H+}
{$modeswitch typehelpers}
{ Test case for issue #482:
  When a pointer to a record is accessed through a field of an object type,
  dereferencing the pointer and accessing a field on the result should resolve
  to the actual record type, NOT to a type helper for a primitive type.

  Bug: cgpara.location^.shiftval incorrectly resolves through TLongIntHelper
  instead of the actual pointed-to record type.

  Matches FPC compiler pattern: TCGPara is an object with location: PCGParaLocation. }

type
  TLongIntHelper = type helper for LongInt
    function IsZero: Boolean;
  end;

  TByteHelper = type helper for Byte
    function IsMax: Boolean;
  end;

  { Forward-declared pointer: pointer type declared BEFORE the record }
  PCGParaLocation = ^TCGParaLocation;
  TCGParaLocation = record
    Next: PCGParaLocation;
    Size: LongInt;
    { variant part like FPC's parabase.pas }
    case LongInt of
      0: (offset: LongInt);
      1: (shiftval: ShortInt;
          register_num: LongInt);
  end;

  { Object type (not record) like FPC's TCGPara }
  TCGPara = object
    Location: PCGParaLocation;
    IntSize: LongInt;
    procedure Init;
  end;

function TLongIntHelper.IsZero: Boolean;
begin
  Result := Self = 0;
end;

function TByteHelper.IsMax: Boolean;
begin
  Result := Self = 255;
end;

procedure TCGPara.Init;
begin
  Location := nil;
  IntSize := 0;
end;

var
  cgpara: TCGPara;
  loc: TCGParaLocation;
  loc2: TCGParaLocation;
begin
  cgpara.Init;

  { Set up linked list of locations }
  loc2.shiftval := 7;
  loc2.register_num := 2;
  loc2.Size := 4;
  loc2.Next := nil;

  loc.shiftval := 42;
  loc.register_num := 1;
  loc.Size := 8;
  loc.Next := @loc2;
  cgpara.Location := @loc;
  cgpara.IntSize := 12;

  { Test 1: Object field pointer dereference then record field access }
  WriteLn(cgpara.Location^.shiftval);
  WriteLn(cgpara.Location^.Size);
  WriteLn(cgpara.Location^.register_num);

  { Test 2: Chained dereference (linked list Next pointer) }
  WriteLn(cgpara.Location^.Next^.shiftval);

  { Test 3: Modify through pointer dereference }
  cgpara.Location^.shiftval := 99;
  WriteLn(loc.shiftval);

  { Test 4: Boolean condition on dereferenced field (common pattern in cgobj.pas) }
  if cgpara.Location^.shiftval <> 0 then
    WriteLn('shiftval nonzero')
  else
    WriteLn('shiftval zero');

  { Test 5: Type helpers still work on actual primitive values }
  WriteLn(cgpara.IntSize.IsZero);

  WriteLn('OK');
end.
