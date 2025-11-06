program GenericsComprehensive;

// Test 1: Simple generic class
type
  TFoo<T> = class
    FData: T;
  end;

// Test 2: Multiple type parameters
type
  TBar<T, U> = class
    First: T;
    Second: U;
  end;

// Test 3: Generic record
type
  TRecord<T> = record
    FData: T;
  end;

// Test 4: Generic interface
type
  IAncestor<T> = interface
    function GetRecord: TRecord<T>;
  end;

  IFoo<T> = interface(IAncestor<T>)
    procedure AMethod(Param: T);
  end;

// Test 5: Generic class with interface
type
  TFooImpl<T> = class(TObject, IFoo<T>)
    FField: TRecord<T>;
    procedure AMethod(Param: T);
    function GetRecord: TRecord<T>;
  end;

// Test 6: Generic array type
type
  anArray<T> = array of T;
  IntArray = anArray<Integer>;

// Test 7: Variable declarations with constructed types
var
  F: TFoo<Integer>;
  StringFoo: TFoo<String>;
  IntStringBar: TBar<Integer, String>;
  IntRecord: TRecord<Integer>;
  IntArray1: anArray<Integer>;

// Test 8: Type aliases with constructed types
type
  TIntFoo = TFoo<Integer>;
  TStringArray = anArray<String>;

// Test 9: Generic base types (open and closed constructed)
type
  TBase1<T> = class(TObject)
  end;

  TDerived1<T> = class(TBase1<T>)  // Open constructed base
  end;

  TDerived2<T> = class(TBase1<Integer>)  // Closed constructed base
  end;

// Test 10: Generic procedural types
type
  TMyProc<T> = procedure(Param: T);
  TMyProc2<Y> = procedure(Param1, Param2: Y);

// Test 11: Generic function type
type
  TMyFunc<T> = function(Param: T): T;

begin
  writeln('Generics comprehensive test');
end.
