program LambdaExamples;
{
  Examples from the problem statement.
  These demonstrate the lambda syntax supported by the compiler.
}

type
  TFuncOfIntToString = reference to function(x: Integer): string;

var
  myFunc: TFuncOfIntToString;
  x: Integer;
  
procedure AnalyzeFunction(proc: TFuncOfIntToString);
begin
  WriteLn('Analyzing function (not yet callable)')
end;

begin
  { Anonymous function assigned to variable }
  myFunc := function(x: Integer): string
  begin
    Result := IntToStr(x)
  end;

  { Call procedure with anonymous method as parameter }
  { Using variable: }
  AnalyzeFunction(myFunc);

  { Use anonymous method directly: }
  AnalyzeFunction(function(x: Integer): string
  begin
    Result := IntToStr(x)
  end);
  
  WriteLn('Lambda examples type-checked successfully!');
end.
