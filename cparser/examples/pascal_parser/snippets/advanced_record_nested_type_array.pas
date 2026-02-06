unit advanced_record_nested_type_array;

interface

type
  TPoint3D = packed record
  public
    Type TSingle3Array = array[0..2] of single;
    constructor Create(const ax,ay,az:single);
  end;

implementation

constructor TPoint3D.Create(const ax,ay,az:single);
begin
end;

end.
