var
  ChPos, OldPos, ArgPos, DoArg, Len: SizeInt;
  Hs, ToAdd: TFormatString;
  Index: SizeInt;
  Width, Prec: Longint;
  Left: Boolean;
  Fchar: TFormatChar;
  vq: QWord;

  function ReadFormat: TFormatChar;
  var
    Value: Longint;

    procedure ReadInteger;
    var
      ArgN: SizeInt;
    begin
      if Value <> -1 then exit;
      OldPos := ChPos;
      while (ChPos <= Len) and (Fmt[ChPos] <= '9') and (Fmt[ChPos] >= '0') do
        Inc(ChPos);
      if ChPos > Len then
        DoFormatError(feInvalidFormat, Fmt);
      if Fmt[ChPos] = '*' then
      begin
        if Index = -1 then
          ArgN := ArgPos
        else
        begin
          ArgN := Index;
          Inc(Index);
        end;
        if (ChPos > OldPos) or (ArgN > High(Args)) then
          DoFormatError(feInvalidFormat, Fmt);
        ArgPos := ArgN + 1;
        case Args[ArgN].VType of
          vtInteger: Value := Args[ArgN].VInteger;
          vtInt64: Value := Args[ArgN].VInt64^;
          vtQWord: Value := Args[ArgN].VQWord^;
        else
          DoFormatError(feInvalidFormat, Fmt);
        end;
        Inc(ChPos);
      end
      else
        Value := -1;
    end;

  begin
    Value := -1;
    ReadInteger;
    ReadFormat := 'A';
  end;

begin
  ChPos := 1;
  Len := Length(Fmt);
  Result := ReadFormat;
end;
