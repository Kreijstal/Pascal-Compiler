program TestNestedTypes;

// Test nested type declarations within generic classes
type
  // Generic class with nested non-generic type
  TFoo<T> = class
  type
    TBar = class
      X: Integer;
      Y: String;
    end;
  end;

  // Generic class with nested generic type
  TOuter<T> = class
  type
    TInner<U> = class
      First: T;
      Second: U;
    end;
  end;

  // Non-generic class with nested generic type
  TBaz = class
  type
    TQux<T> = class
      Value: T;
    end;
  end;

  // Multiple nested types
  TMultiNested<T> = class
  type
    TData = class
      Info: T;
    end;
    
    THelper<U> = class
      Item1: T;
      Item2: U;
    end;
  end;

begin
  writeln('Nested types test passed');
end.
