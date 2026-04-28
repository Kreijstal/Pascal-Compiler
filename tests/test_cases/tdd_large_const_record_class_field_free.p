program tdd_large_const_record_class_field_free;
{$mode objfpc}

type
  TMode = (m0, m1, m2, m3, m4);
  TModes = set of TMode;

  TPos = record
    A: LongInt;
    B: LongInt;
    C: LongInt;
  end;

  TItem = class
    Value: LongInt;
    destructor Destroy; override;
  end;

  TState = record
    NewItem: TItem;
    OldItem: TItem;
    Pos: TPos;
    Modes: TModes;
    Valid: Boolean;
  end;

destructor TItem.Destroy;
begin
  WriteLn(Value);
  inherited Destroy;
end;

procedure Replace(out State: TState);
begin
  State.OldItem := nil;
  State.Pos.A := 1;
  State.Pos.B := 2;
  State.Pos.C := 3;
  State.Modes := [m1, m3];
  State.Valid := True;
  State.NewItem := TItem.Create;
  State.NewItem.Value := State.Pos.A + State.Pos.B + State.Pos.C;
end;

procedure Restore(const State: TState);
begin
  if State.Valid and (m3 in State.Modes) then
    State.NewItem.Free;
end;

var
  State: TState;

begin
  Replace(State);
  Restore(State);
end.
