unit impl_try_except_concat_unit;

interface

type
  TFormatSettings = record end;
  Exception = class
    ClassName: string;
    Message: string;
  end;

function SafeFormat(const Fmt: AnsiString; const Args: array of const; const FormatSettings: TFormatSettings): UTF8String;

implementation

function SafeFormat(const Fmt: AnsiString; const Args: array of const; const FormatSettings: TFormatSettings): UTF8String;
begin
  try
    Result:=Format(Fmt,Args,FormatSettings);
  except
    On E : Exception do
      Result:='Error "'+E.ClassName+'" during format('''+Fmt+''',['+ArrayOfConstToStr(Args,',','{','}')+']) : '+E.Message;
  end;
end;

function AfterSafe: Integer;
begin
  Result := 0;
end;

end.
