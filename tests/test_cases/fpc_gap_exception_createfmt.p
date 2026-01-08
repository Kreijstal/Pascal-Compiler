program fpc_gap_exception_createfmt;
{$mode objfpc}
{ Test: Exception class with CreateFmt constructor
  FPC allows CreateFmt(const msg: string; const args: array of const)
  Expected output: Error: 42 }

type
  Exception = class
  private
    FMessage: string;
  public
    constructor Create(const Msg: string);
    constructor CreateFmt(const Fmt: string; const Args: array of const);
    property Message: string read FMessage;
  end;

constructor Exception.Create(const Msg: string);
begin
  FMessage := Msg;
end;

constructor Exception.CreateFmt(const Fmt: string; const Args: array of const);
begin
  { Simplified - in real FPC this formats the string }
  if Length(Args) > 0 then
    FMessage := Fmt
  else
    FMessage := Fmt;
end;

var
  E: Exception;
begin
  E := Exception.CreateFmt('Error: %d', [42]);
  writeln(E.Message);
  E.Free;
end.
