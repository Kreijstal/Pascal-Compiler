program tdd_class_field_static_directive;

{ Regression: a class field in a plain `var` section may carry a trailing
  `static;` directive, as FPC 3.2.2's sysencodingh.inc does for TEncoding.
  KGPC previously failed parsing this with "Expected keyword". }

type
  TEncoding = class
  strict private
    type
      TStandardEncoding = (seAnsi, seAscii, seUnicode);
    var
      FStandardEncodings: array[TStandardEncoding] of LongInt; static;
      FSystemEncodings: array of LongInt; static;
    Class Var
      FLock: LongInt;
  protected
    { FPC compiler/cclasses.pas uses the `noreturn` method directive. }
    procedure RaiseError; noreturn;
  public
    function GetByteCount: Integer; virtual; abstract;
  end;

procedure TEncoding.RaiseError;
begin
end;

begin
  WriteLn('ok');
end.
