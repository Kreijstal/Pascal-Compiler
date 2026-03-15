unit GenericClassPublicTypeSpecialize;

interface

type
  generic TEnum<T> = class(TObject)
  protected
    function GetCurrent: T;
  public
    property Current: T read GetCurrent;
  end;

  generic TList<T> = class(TObject)
  public
    type
      TEnumSpec = specialize TEnum<T>;
    function GetEnumerator: TEnumSpec;
  end;

implementation

end.
