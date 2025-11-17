{ Free Pascal Generic Library (FGL) - stub implementation
  
  This unit provides declarations for FGL generic types. The actual
  implementations must be specialized in user code.
}

unit FGL;

interface

type
  { Generic list container - declaration only
    Users must provide specialized implementations }
  TFPGList<T> = class
    procedure Add(const Item: T);
    function Get(Index: Integer): T;
    property Items[Index: Integer]: T read Get; default;
    property Count: Integer;
  end;

implementation

{ TFPGList methods are not implemented in the stub.
  Specialized versions must be provided by the compiler's generic
  instantiation system or in user code. }

end.
