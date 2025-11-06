program GenericParsingTest;

// Test generic type declarations
type
  // Simple generic class
  TFoo<T> = class
    FData: T;
  end;

  // Generic with multiple type parameters
  TBar<T, U> = record
    First: T;
    Second: U;
  end;

  // Generic array type
  TArray<T> = array of T;

  // Generic pointer type
  TPointer<T> = ^T;

// Test variable declarations with constructed types
var
  IntFoo: TFoo<Integer>;
  StringIntBar: TBar<String, Integer>;
  IntArray: TArray<Integer>;
  IntPointer: TPointer<Integer>;

// Test type aliases with constructed types
type
  TIntFoo = TFoo<Integer>;
  TStringArray = TArray<String>;

begin
  // The parser can now handle generic type declarations and constructed types
  // Semantic analysis for generic expressions is not yet implemented
  writeln('Generic parsing test completed successfully!');
end.