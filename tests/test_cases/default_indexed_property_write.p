{ Regression: writing through a class default indexed property (obj[i] := v).

  semcheck_arrayaccess synthesizes an `obj.DefaultProperty` record-access
  wrapper node for the LHS, then defers the setter rewrite to
  semcheck_try_indexed_property_assignment.  That rewrite detached the wrapper
  from the array-access node (nulling the array_expr slot) and destroyed the
  array-access node -- but never freed the detached wrapper itself, leaking the
  Expression node plus its strdup'd property-name string on every default
  indexed property assignment.

  This exercises the setter path so the store must land in the backing array
  (verifying no use-after-free from the added free), and the read-back path. }
program default_indexed_property_write;

type
  TColl = class
    FData: array[0..9] of Integer;
    procedure SetItem(i: Integer; v: Integer);
    function GetItem(i: Integer): Integer;
    property Items[i: Integer]: Integer read GetItem write SetItem; default;
  end;

procedure TColl.SetItem(i: Integer; v: Integer);
begin
  FData[i] := v;
end;

function TColl.GetItem(i: Integer): Integer;
begin
  GetItem := FData[i];
end;

var
  c: TColl;
  i: Integer;
begin
  c := TColl.Create;
  for i := 0 to 4 do
    c[i] := i * 10;
  for i := 0 to 4 do
    writeln(c[i]);
  c := nil;
end.
