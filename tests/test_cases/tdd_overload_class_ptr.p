program test_overload_class_ptr;
{$MODE OBJFPC}
{$MODESWITCH class}

type
  TAnimal = class
    Name: String;
  end;

  TCat = class(TAnimal)
  end;

function Describe(const A: TAnimal): Integer; overload;
begin
  Result := 1;
end;

function Describe(const C: TCat): Integer; overload;
begin
  Result := 2;
end;

var
  a: TAnimal;
  c: TCat;
begin
  a := TAnimal.Create;
  c := TCat.Create;
  WriteLn(Describe(a));
  WriteLn(Describe(c));
  c.Free;
  a.Free;
end.
