{$mode objfpc}{$H+}
{$modeswitch typehelpers}
{ Regression test: nested function calling helper method overload in unit context }
{ FPC compiles this. KGPC should too. }
unit reg_unit_nested_overload;

interface

type
  TMyHelper = type helper for AnsiString
    function Foo(const A: array of AnsiChar; B: Integer): Integer;
  end;

procedure Test(const Seps: array of AnsiChar);

implementation

function TMyHelper.Foo(const A: array of AnsiChar; B: Integer): Integer;
begin
  Result := B + High(A) - Low(A) + 1;
end;

procedure Test(const Seps: array of AnsiChar);
var
  S: AnsiString;
  
  function Inner: Integer;
  begin
    { This call should find TMyHelper.Foo but in unit context it fails }
    Result := S.Foo(Seps, 0);
  end;

begin
  S := 'test';
  WriteLn(Inner);
end;

end.
