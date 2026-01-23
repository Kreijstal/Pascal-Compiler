{ Test: Try-except with string concatenation }
{ Verifies parser handles complex expressions in except block }
program impl_try_except_concat;

type
  Exception = class
  public
    ClassName: string;
    Message: string;
    constructor Create(const Msg: string);
  end;

constructor Exception.Create(const Msg: string);
begin
  ClassName := 'Exception';
  Message := Msg;
end;

function SafeOperation: string;
begin
  try
    raise Exception.Create('Test error');
  except
    on E: Exception do
      Result := 'Error "' + E.ClassName + '": ' + E.Message;
  end;
end;

function AfterSafe: Integer;
begin
  Result := 99;
end;

begin
  WriteLn(SafeOperation);
  WriteLn(AfterSafe);
end.
