{$mode objfpc}{$H+}
{$modeswitch typehelpers}
{ Regression: nested function INSIDE helper method calling another helper method }
unit reg_helper_nested_inner_call;

interface

type
  TMyStringArray = array of AnsiString;
  
  TMyHelper = type helper for AnsiString
    function IndexOfAnyUnquoted(const A: array of AnsiChar; B, C: AnsiChar; D: Integer): Integer;
    function Split(const Seps: array of AnsiChar; QS, QE: AnsiChar; Count: Integer): TMyStringArray;
  end;

implementation

function TMyHelper.IndexOfAnyUnquoted(const A: array of AnsiChar; B, C: AnsiChar; D: Integer): Integer;
begin
  Result := D + High(A) - Low(A);
end;

function TMyHelper.Split(const Seps: array of AnsiChar; QS, QE: AnsiChar; Count: Integer): TMyStringArray;

  function NextSep(StartIdx: Integer): Integer;
  begin
    if QS <> #0 then
      { Calling Self.IndexOfAnyUnquoted from nested function INSIDE helper method }
      Result := Self.IndexOfAnyUnquoted(Seps, QS, QE, StartIdx)
    else
      Result := -1;
  end;

begin
  SetLength(Result, 0);
  WriteLn(NextSep(0));
end;

end.
