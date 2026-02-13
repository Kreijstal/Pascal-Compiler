program test_record_prop_setter;
{$mode delphi}

type
  TMyRec = record
  private
    FHigh: Integer;
    FLow: Integer;
    function GetHigh: Integer;
    procedure SetHigh(AValue: Integer);
    function GetLow: Integer;
    procedure SetLow(AValue: Integer);
  public
    property HighVal: Integer read GetHigh write SetHigh;
    property LowVal: Integer read GetLow write SetLow;
  end;

function TMyRec.GetHigh: Integer;
begin
  Result := FHigh;
end;

procedure TMyRec.SetHigh(AValue: Integer);
begin
  FHigh := AValue;
end;

function TMyRec.GetLow: Integer;
begin
  Result := FLow;
end;

procedure TMyRec.SetLow(AValue: Integer);
begin
  FLow := AValue;
end;

type
  TIntHelper = record helper for Integer
    function GetDoubled: Integer;
    procedure SetFromHelper(AValue: Integer);
  end;

function TIntHelper.GetDoubled: Integer;
begin
  Result := Self * 2;
end;

procedure TIntHelper.SetFromHelper(AValue: Integer);
begin
  Self := AValue;
end;

var
  r: TMyRec;
begin
  r.HighVal := 10;
  r.LowVal := 20;
  WriteLn(r.HighVal);
  WriteLn(r.LowVal);
  r.HighVal := r.LowVal + 5;
  WriteLn(r.HighVal);
end.
