program TestGenericMethods;

// Test generic methods with type parameters
type
  TFoo = class
    // Generic method declarations
    procedure Test<T>(X: T);
    function Compare<T>(X, Y: T): Boolean;
    
    // Generic method with constraint
    procedure TestConstrained<T: class>(Obj: T);
    
    // Generic method with multiple type parameters
    function Convert<T, U>(Input: T): U;
  end;

  // Generic method in generic class
  TBar<T> = class
    procedure GenericInGeneric<U>(X: T; Y: U);
  end;

  // Generic methods in interfaces
  IComparable<T> = interface
    function CompareGeneric<U>(X: T; Y: U): Integer;
  end;

begin
  writeln('Generic methods test passed');
end.
