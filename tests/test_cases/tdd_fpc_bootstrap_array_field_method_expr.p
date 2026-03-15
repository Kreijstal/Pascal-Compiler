{ TDD: calling a method on record.array_field[i] in expression context.
  Bug: KGPC loses class type through record.array[i] chain when result is used
  as an expression. Statement calls (void) work, but function returns fail with
  "pointer does not reference a record type".
  This also affects WITH on record.array_field[i].sub_field.
  Impact: ~1860 errors in FPC compiler bootstrap (97 units). }
program tdd_fpc_bootstrap_array_field_method_expr;

type
  TKind = (kA, kB);

  TFoo = class
    FCount: Integer;
    procedure Add;
    function Count: Integer;
    function GetName: String;
  end;

  TBar = record
    items: array[0..1] of TFoo;
    named: array[TKind] of TFoo;
  end;

procedure TFoo.Add;
begin
  Inc(FCount);
end;

function TFoo.Count: Integer;
begin
  Result := FCount;
end;

function TFoo.GetName: String;
begin
  Result := 'foo' + Chr(Ord('0') + FCount);
end;

var
  b: TBar;
  n: Integer;
  s: String;
begin
  { Setup }
  b.items[0] := TFoo.Create;
  b.named[kA] := TFoo.Create;

  { Statement context — these already work }
  b.items[0].Add;
  b.items[0].Add;
  b.named[kA].Add;

  { Expression context — these are the bugs }
  n := b.items[0].Count;
  WriteLn(n);

  s := b.items[0].GetName;
  WriteLn(s);

  WriteLn(b.named[kA].Count);
  WriteLn(b.named[kA].GetName);

  { Chained field access through array element }
  WriteLn(b.items[0].FCount);
end.
