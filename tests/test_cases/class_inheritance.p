program ClassInheritanceTest;

type
  TAnimal = class
    Name: Integer;
    Age: Integer;
    procedure Speak;
  end;

  TDog = class(TAnimal)
    Breed: Integer;
    procedure WagTail;
  end;

procedure TAnimal.Speak;
begin
  WriteLn('Animal age: ', Age);
end;

procedure TDog.WagTail;
begin
  WriteLn('Dog wagging tail, age: ', Age, ', breed: ', Breed);
end;

var
  MyDog: TDog;
begin
  MyDog := TDog.Create;
  MyDog.Name := 1;
  MyDog.Age := 5;
  MyDog.Breed := 42;
  
  MyDog.Speak;      { Inherited from TAnimal }
  MyDog.WagTail;    { Defined in TDog }
end.
