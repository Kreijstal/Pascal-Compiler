program tdd_typedconst_ptr_to_class_deref;
{$mode objfpc}
type
  TItem = class
    payload: longint;
    constructor Create(v: longint);
  end;

constructor TItem.Create(v: longint);
begin
  payload := v;
end;

var
  g: TItem;
const
  pbest: ^TItem = @g;     { typed constant pointer to a class-typed global }

procedure Take(it: TItem);
begin
  writeln('payload=', it.payload);
end;

begin
  g := TItem.Create(77);
  Take(pbest^);            { deref typed-const ptr-to-class, pass by value }
end.
