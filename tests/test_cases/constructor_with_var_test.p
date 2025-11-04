program ConstructorWithVarTest;

type
  TTest = class
    X: Integer;
  end;

constructor TTest.Create;
var
  InitValue: Integer;
begin
  InitValue := 42;
  X := InitValue;
end;

destructor TTest.Destroy;
var
  FinalValue: Integer;
begin
  FinalValue := X;
  X := 0;
end;

begin
  WriteLn('Constructor and destructor with var sections parsed successfully!');
end.
