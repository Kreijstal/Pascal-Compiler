{ Test nested type declarations inside records and classes }
{ This is Delphi syntax: public type inside record/class }
program nested_type_declarations;

{$mode delphi}

type
  { Record with nested type declarations }
  TRecordWithNestedTypes = record
  public type
    TNestedInteger = Integer;
  public
    Value: TNestedInteger;
  end;

  { Record with nested procedural type }
  TCallbackRecord = record
  public type
    TCallback = procedure(x: Integer);
  public
    Handler: TCallback;
  end;

  { Class with nested type declarations }
  TClassWithNestedTypes = class
  public type
    TInnerRecord = record
      X: Integer;
      Y: Integer;
    end;
  private
    FData: TInnerRecord;
  public
    procedure SetData(aX, aY: Integer);
    function GetSum: Integer;
  end;

  { Nested enum type in record }
  TStateRecord = record
  public type
    TState = (stIdle, stRunning, stPaused, stStopped);
  public
    CurrentState: TState;
  end;

procedure TClassWithNestedTypes.SetData(aX, aY: Integer);
begin
  FData.X := aX;
  FData.Y := aY;
end;

function TClassWithNestedTypes.GetSum: Integer;
begin
  Result := FData.X + FData.Y;
end;

var
  Rec: TRecordWithNestedTypes;
  State: TStateRecord;
  Obj: TClassWithNestedTypes;

begin
  { Test nested type alias in record }
  Rec.Value := 42;
  WriteLn('Record nested type value: ', Rec.Value);

  { Test nested enum type }
  State.CurrentState := stRunning;
  WriteLn('State ordinal: ', Ord(State.CurrentState));

  { Test class with nested record }
  Obj := TClassWithNestedTypes.Create;
  Obj.SetData(10, 20);
  WriteLn('Class GetSum: ', Obj.GetSum);
  { Note: Obj.Free not called - TObject.Free not yet fully supported }

  WriteLn('Nested type declarations test completed');
end.
