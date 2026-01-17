{$mode objfpc}{$H+}
{$modeswitch typehelpers}
{ Regression test for type helper method call from nested function }
{ This tests the pattern from FPC sysutils Split function that calls }
{ IndexOfAnyUnquoted from a nested function NextSep }
unit reg_sysutils_indexof_nested;

interface

type
  TAnsiStringArray = array of AnsiString;
  TStringSplitOptions = (ssNone, ssRemoveEmpty);
  SizeInt = Int64;
  
  TAnsiStringHelper = type helper for AnsiString
    function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; 
      StartQuote, EndQuote: AnsiChar): SizeInt; overload;
    function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; 
      StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt): SizeInt; overload;
    function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; 
      StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    function IndexOfAny(const AnyOf: array of AnsiChar; StartIndex: SizeInt): SizeInt;
    function Split(const Separators: array of AnsiChar; AQuoteStart, 
      AQuoteEnd: AnsiChar; ACount: SizeInt; Options: TStringSplitOptions): TAnsiStringArray;
  end;
  
  TStringHelper = type helper(TAnsiStringHelper) for AnsiString
    function InheritedSplit(const Separators: array of AnsiChar; AQuoteStart, 
      AQuoteEnd: AnsiChar; ACount: SizeInt; Options: TStringSplitOptions): TAnsiStringArray;
  end;

implementation

function TAnsiStringHelper.IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; 
  StartQuote, EndQuote: AnsiChar): SizeInt;
begin
  Result := IndexOfAnyUnquoted(AnyOf, StartQuote, EndQuote, 0);
end;

function TAnsiStringHelper.IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; 
  StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt): SizeInt;
begin
  Result := IndexOfAnyUnquoted(AnyOf, StartQuote, EndQuote, StartIndex, Length(Self) - StartIndex);
end;

function TAnsiStringHelper.IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; 
  StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt;
var
  I, J: SizeInt;
begin
  Result := -1;
  for I := StartIndex + 1 to StartIndex + ACount do
  begin
    for J := Low(AnyOf) to High(AnyOf) do
      if Self[I] = AnyOf[J] then
      begin
        Result := I - 1;
        Exit;
      end;
  end;
end;

function TAnsiStringHelper.IndexOfAny(const AnyOf: array of AnsiChar; StartIndex: SizeInt): SizeInt;
begin
  Result := IndexOfAnyUnquoted(AnyOf, #0, #0, StartIndex);
end;

function TAnsiStringHelper.Split(const Separators: array of AnsiChar; AQuoteStart, 
  AQuoteEnd: AnsiChar; ACount: SizeInt; Options: TStringSplitOptions): TAnsiStringArray;

  function NextSep(StartIndex: SizeInt): SizeInt;
  begin
    if (AQuoteStart <> #0) then
      { Key test: calling overloaded method from nested function with array param }
      Result := Self.IndexOfAnyUnQuoted(Separators, AQuoteStart, AQuoteEnd, StartIndex)
    else
      Result := Self.IndexOfAny(Separators, StartIndex);
  end;

begin
  SetLength(Result, 0);
  if NextSep(0) < 0 then
    begin
      SetLength(Result, 1);
      Result[0] := Self;
    end;
end;

function TStringHelper.InheritedSplit(const Separators: array of AnsiChar; AQuoteStart, 
  AQuoteEnd: AnsiChar; ACount: SizeInt; Options: TStringSplitOptions): TAnsiStringArray;

  function NextSep(StartIndex: SizeInt): SizeInt;
  begin
    { Calling inherited method via Self }
    if (AQuoteStart <> #0) then
      Result := Self.IndexOfAnyUnQuoted(Separators, AQuoteStart, AQuoteEnd, StartIndex)
    else
      Result := Self.IndexOfAny(Separators, StartIndex);
  end;

begin
  SetLength(Result, 0);
  if NextSep(0) < 0 then
    begin
      SetLength(Result, 1);
      Result[0] := Self;
    end;
end;

end.
