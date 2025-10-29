program record_member_access;

type
  TVector = record
    x, y: integer;
  end;

  TBox = record
    left, right: TVector;
  end;

var
  box: TBox;
  total, product: integer;
begin
  box.left.x := 3;
  box.left.y := 4;
  box.right.x := 5;
  box.right.y := 6;

  total := box.left.x + box.left.y + box.right.x + box.right.y;
  product := box.left.y * box.right.x;

  writeln(total);
  writeln(product);
end.
