program tdd_implicit_overload_target_selection;
{$mode objfpc}
{$modeswitch advancedrecords}

type
  TUInt24Rec = record
    V: LongWord;
    class operator :=(const R: TUInt24Rec): LongWord;
    class operator :=(const R: TUInt24Rec): Integer;
    class operator :=(const R: TUInt24Rec): Word;
    class operator :=(A: LongWord): TUInt24Rec;
  end;

class operator TUInt24Rec.:=(const R: TUInt24Rec): LongWord;
begin
  Result := R.V + 1000;
end;

class operator TUInt24Rec.:=(const R: TUInt24Rec): Integer;
begin
  Result := Integer(R.V) + 2000;
end;

class operator TUInt24Rec.:=(const R: TUInt24Rec): Word;
begin
  Result := Word(R.V + 3000);
end;

class operator TUInt24Rec.:=(A: LongWord): TUInt24Rec;
begin
  Result.V := A;
end;

var
  R: TUInt24Rec;
  L: LongWord;
  I: Integer;
  W: Word;
begin
  R := 7;
  L := R;
  I := R;
  W := R;
  WriteLn(L, ' ', I, ' ', W);
end.
