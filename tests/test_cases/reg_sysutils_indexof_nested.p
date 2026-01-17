{$mode objfpc}{$H+}
{$modeswitch typehelpers}
{ Regression test for type helper method call from nested function }
{ This tests the pattern from FPC sysutils Split function that calls }
{ IndexOfAnyUnquoted from a nested function NextSep }
program reg_sysutils_indexof_nested;

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
    function TestCall: SizeInt;
  end;
  
  TStringHelper = type helper(TAnsiStringHelper) for AnsiString
    function InheritedTestCall: SizeInt;
  end;

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

function TAnsiStringHelper.TestCall: SizeInt;

  function BaseNestedCall: SizeInt;
  begin
    { Key test: calling overloaded Self.method from nested function }
    Result := Self.IndexOfAnyUnQuoted(['|'], #0, #0, 0);
  end;

begin
  Result := BaseNestedCall;
end;

function TStringHelper.InheritedTestCall: SizeInt;

  function DerivedNestedCall: SizeInt;
  begin
    { Calling inherited method via Self from nested function }
    Result := Self.IndexOfAny(['|'], 0);
  end;

begin
  Result := DerivedNestedCall;
end;

var
  S: AnsiString;
  R1, R2: SizeInt;
begin
  { Test that the methods compile and work correctly }
  S := 'test|value';
  R1 := S.TestCall;
  R2 := S.InheritedTestCall;
  WriteLn(R1);
  WriteLn(R2);
end.
