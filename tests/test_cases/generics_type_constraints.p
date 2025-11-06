program TestTypeConstraints;

// Test type constraints on generic type parameters
type
  // Class constraint
  TClassConstrained<T: class> = class
    FData: T;
  end;

  // Record constraint
  TRecordConstrained<T: record> = class
    FData: T;
  end;

  // Constructor constraint
  TConstructorConstrained<T: constructor> = class
    FData: T;
  end;

  // Interface constraint
  TInterfaceConstrained<T: interface> = class
    FData: T;
  end;

  // Multiple parameters with different constraints
  TMultiConstrained<T: class, U: record> = class
    FFirst: T;
    FSecond: U;
  end;

begin
  writeln('Type constraints test passed');
end.
