unit property_indexed_unit;

interface

type
  TBoolArray = array[boolean] of LongInt;

var
  G: TBoolArray;

function GetIndexVal(b: Boolean): LongInt;
property IndexVal[b: Boolean]: LongInt read GetIndexVal;

implementation

function GetIndexVal(b: Boolean): LongInt;
begin
  GetIndexVal := G[b];
end;

end.
