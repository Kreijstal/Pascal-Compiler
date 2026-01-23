{$mode objfpc}{$H+}
{$modeswitch typehelpers}
{ Regression test for type helper inheritance with nested function calls }
{ Tests that Self.Method() from a nested function inside a derived helper }
{ correctly resolves to the parent helper's overloaded methods. }
program reg_helper_derived_overload;

type
  SizeInt = Int64;
  TAnsiStringArray = array of AnsiString;
  
  TBaseHelper = type helper for AnsiString
    function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar): SizeInt; overload;
    function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt): SizeInt; overload;
    function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    function IndexOfAny(const AnyOf: array of AnsiChar; StartIndex: SizeInt): SizeInt;
    function TestCall: SizeInt;
  end;
  
  TDerivedHelper = type helper(TBaseHelper) for AnsiString
    function InheritedTestCall: SizeInt;
  end;

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

function TBaseHelper.TestCall: SizeInt;

  function BaseNestedCall: SizeInt;
  begin
    { Key test: calling overloaded Self.method from nested function }
    Result := Self.IndexOfAnyUnquoted([','], #0, #0, 0);
  end;

begin
  Result := BaseNestedCall;
end;

function TDerivedHelper.InheritedTestCall: SizeInt;

  function DerivedNestedCall: SizeInt;
  begin
    { Key test: calling inherited method via Self from nested function in derived helper }
    Result := Self.IndexOfAny([','], 0);
  end;

begin
  Result := DerivedNestedCall;
end;

var
  S: AnsiString;
  R1, R2: SizeInt;
begin
  S := 'hello,world';
  R1 := S.TestCall;
  R2 := S.InheritedTestCall;
  WriteLn(R1);
  WriteLn(R2);
end.
