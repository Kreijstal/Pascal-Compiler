{$mode objfpc}{$H+}
{$modeswitch typehelpers}
{ Regression: overload resolution for array of AnsiChar parameter in nested function }
unit reg_helper_overload_array;

interface

type
  TMyStringArray = array of AnsiString;
  SizeInt = Int64;
  
  TMyHelper = type helper for AnsiString
    { Multiple overloads with array of AnsiChar parameter }
    function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar): SizeInt; overload;
    function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt): SizeInt; overload;
    function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    { Also overload with array of AnsiString }
    function IndexOfAnyUnquoted(const AnyOf: array of AnsiString; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt; out Matched: SizeInt): SizeInt; overload;
    function IndexOfAny(const AnyOf: array of AnsiChar; StartIndex: SizeInt): SizeInt;
    function Split(const Seps: array of AnsiChar; QS, QE: AnsiChar; Count: SizeInt): TMyStringArray;
  end;

implementation

function TMyHelper.IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar): SizeInt;
begin
  Result := IndexOfAnyUnquoted(AnyOf, StartQuote, EndQuote, 0, Length(Self));
end;

function TMyHelper.IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt): SizeInt;
begin
  Result := IndexOfAnyUnquoted(AnyOf, StartQuote, EndQuote, StartIndex, Length(Self) - StartIndex);
end;

function TMyHelper.IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt;
begin
  Result := StartIndex + High(AnyOf) - Low(AnyOf);
end;

function TMyHelper.IndexOfAnyUnquoted(const AnyOf: array of AnsiString; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt; out Matched: SizeInt): SizeInt;
begin
  Matched := 0;
  Result := -1;
end;

function TMyHelper.IndexOfAny(const AnyOf: array of AnsiChar; StartIndex: SizeInt): SizeInt;
begin
  Result := IndexOfAnyUnquoted(AnyOf, #0, #0, StartIndex);
end;

function TMyHelper.Split(const Seps: array of AnsiChar; QS, QE: AnsiChar; Count: SizeInt): TMyStringArray;

  function NextSep(StartIdx: SizeInt): SizeInt;
  begin
    if QS <> #0 then
      { Calling 4-parameter overload of IndexOfAnyUnquoted from nested function }
      Result := Self.IndexOfAnyUnquoted(Seps, QS, QE, StartIdx)
    else
      Result := Self.IndexOfAny(Seps, StartIdx);
  end;

begin
  SetLength(Result, 0);
  WriteLn(NextSep(0));
end;

end.
