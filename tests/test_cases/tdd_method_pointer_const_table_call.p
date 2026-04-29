program tdd_method_pointer_const_table_call;
{$mode objfpc}

type
  TKind = (kNone, kRun);

  TBox = class
    function Value: Integer;
    function Call(Kind: TKind): Integer;
  end;

function TBox.Value: Integer;
begin
  Result := 23;
end;

function TBox.Call(Kind: TKind): Integer;
const
  Table: array[TKind] of Pointer = (
    nil,
    @TBox.Value
  );
type
  TGetter = function: Integer of object;
var
  Method: TMethod;
begin
  Method.Code := Table[Kind];
  Method.Data := Self;
  Result := TGetter(Method)();
end;

var
  Box: TBox;

begin
  Box := TBox.Create;
  WriteLn(Box.Call(kRun));
end.
