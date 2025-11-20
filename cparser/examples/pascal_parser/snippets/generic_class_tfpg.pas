unit GenericClassTFPG;

interface

type
  generic TFPGList<T> = class
  private
    FItems: array of T;
    FCount: NativeInt;
    function GetItem(Index: NativeInt): T;
    procedure SetItem(Index: NativeInt; const Value: T);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Value: T);
    function Count: NativeInt;
    function GetEnumerator: specialize TFPGListEnumerator<T>;
    property Items[Index: NativeInt]: T read GetItem write SetItem; default;
  end;

implementation

end.
