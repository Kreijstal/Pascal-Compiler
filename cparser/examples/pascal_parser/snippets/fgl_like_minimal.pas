unit FGLMinimal;

interface

type
  generic TFGLListEnumerator<T> = record
  private
    FCount: NativeInt;
  public
    function MoveNext: Boolean;
  end;

implementation

end.

