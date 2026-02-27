program class_ctor_assign;

{$mode objfpc}

type
  Exception = class
  end;

  EInOutError = class(Exception)
  public
    ErrorCode: Integer;
  end;

var
  E: EInOutError;
begin
  E := EInOutError.Create;
  E.ErrorCode := 3;
  writeln('ok ', E.ErrorCode);
end.
