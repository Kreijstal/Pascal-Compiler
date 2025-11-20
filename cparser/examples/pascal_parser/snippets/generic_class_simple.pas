unit GenericClassSimple;

interface

type
  generic TFoo<T> = class
  private
    FValue: T;
  public
    constructor Create;
    procedure Add(const Value: T);
    function GetEnumerator: specialize TFoo<T>;
    property Value: T read FValue write FValue; default;
  end;

implementation

end.
