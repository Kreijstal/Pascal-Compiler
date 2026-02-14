program test_record_prop_setter;
{$mode delphi}

type
  TMyRec = record
  private
    FData: LongWord;
    function GetHigh: Word;
    procedure SetHigh(AValue: Word);
    function GetLow: Word;
    procedure SetLow(AValue: Word);
  public
    property HighWord: Word read GetHigh write SetHigh;
    property LowWord: Word read GetLow write SetLow;
  end;

function TMyRec.GetHigh: Word;
begin
  Result := Word(FData shr 16);
end;

procedure TMyRec.SetHigh(AValue: Word);
begin
  FData := (FData and $0000FFFF) or (LongWord(AValue) shl 16);
end;

function TMyRec.GetLow: Word;
begin
  Result := Word(FData and $FFFF);
end;

procedure TMyRec.SetLow(AValue: Word);
begin
  FData := (FData and $FFFF0000) or LongWord(AValue);
end;

type
  TLongWordHelper = record helper for LongWord
    function GetHigh: Word;
    procedure SetHigh(AValue: Word);
    function GetLow: Word;
    procedure SetLow(AValue: Word);
  end;

function TLongWordHelper.GetHigh: Word;
begin
  Result := TMyRec(Self).HighWord;
end;

procedure TLongWordHelper.SetHigh(AValue: Word);
begin
  TMyRec(Self).HighWord := AValue;
end;

function TLongWordHelper.GetLow: Word;
begin
  Result := TMyRec(Self).LowWord;
end;

procedure TLongWordHelper.SetLow(AValue: Word);
begin
  TMyRec(Self).LowWord := AValue;
end;

var
  r: TMyRec;
  v: LongWord;
begin
  r.HighWord := 1;
  r.LowWord := 2;
  WriteLn(r.HighWord);
  WriteLn(r.LowWord);
  v := $00030004;
  WriteLn(v.GetHigh);
  WriteLn(v.GetLow);
end.
