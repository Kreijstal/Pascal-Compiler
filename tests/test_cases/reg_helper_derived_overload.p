{$mode objfpc}{$H+}
{$modeswitch typehelpers}
unit test_derived_split;

interface

type
  SizeInt = Int64;
  TAnsiStringArray = array of AnsiString;
  
  TBaseHelper = type helper for AnsiString
    function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar): SizeInt; overload;
    function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt): SizeInt; overload;
    function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    function IndexOfAny(const AnyOf: array of AnsiChar; StartIndex: SizeInt): SizeInt;
    function Split(const Separators: array of AnsiChar; AQuoteStart, AQuoteEnd: AnsiChar; ACount: SizeInt): TAnsiStringArray;
  end;
  
  TDerivedHelper = type helper(TBaseHelper) for AnsiString
    function InheritedSplit(const Separators: array of AnsiChar; AQuoteStart, AQuoteEnd: AnsiChar; ACount: SizeInt): TAnsiStringArray;
  end;

implementation

function TBaseHelper.IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar): SizeInt;
begin
  Result := IndexOfAnyUnquoted(AnyOf, StartQuote, EndQuote, 0);
end;

function TBaseHelper.IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt): SizeInt;
begin
  Result := IndexOfAnyUnquoted(AnyOf, StartQuote, EndQuote, StartIndex, Length(Self) - StartIndex);
end;

function TBaseHelper.IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar; StartIndex, ACount: SizeInt): SizeInt;
var
  I, J: SizeInt;
begin
  Result := -1;
  for I := StartIndex + 1 to StartIndex + ACount do
    for J := Low(AnyOf) to High(AnyOf) do
      if Self[I] = AnyOf[J] then
      begin
        Result := I - 1;
        Exit;
      end;
end;

function TBaseHelper.IndexOfAny(const AnyOf: array of AnsiChar; StartIndex: SizeInt): SizeInt;
begin
  Result := IndexOfAnyUnquoted(AnyOf, #0, #0, StartIndex);
end;

function TBaseHelper.Split(const Separators: array of AnsiChar; AQuoteStart, AQuoteEnd: AnsiChar; ACount: SizeInt): TAnsiStringArray;

  function NextSep(StartIndex: SizeInt): SizeInt;
  begin
    if (AQuoteStart <> #0) then
      Result := Self.IndexOfAnyUnquoted(Separators, AQuoteStart, AQuoteEnd, StartIndex)
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

function TDerivedHelper.InheritedSplit(const Separators: array of AnsiChar; AQuoteStart, AQuoteEnd: AnsiChar; ACount: SizeInt): TAnsiStringArray;

  function NextSep(StartIndex: SizeInt): SizeInt;
  begin
    if (AQuoteStart <> #0) then
      Result := Self.IndexOfAnyUnquoted(Separators, AQuoteStart, AQuoteEnd, StartIndex)
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
