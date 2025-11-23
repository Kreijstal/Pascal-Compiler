program DynamicDispatchUnitTest;

{ Unit test for dynamic dispatch with virtual/override methods
  This test verifies that:
  1. Virtual methods can be declared in base classes
  2. Override methods can be declared in derived classes  
  3. The correct method is called based on the actual object type (dynamic dispatch)
}

type
  TAnimal = class
    Name: Integer;
    procedure Speak; virtual;
    procedure Move; virtual;
  end;

  TDog = class(TAnimal)
    Breed: Integer;
    procedure Speak; override;  { Override Speak }
    procedure Bark;             { New method }
  end;
  
  TCat = class(TAnimal)
    Color: Integer;
    procedure Speak; override;  { Override Speak }
    procedure Move; override;   { Override Move }
  end;

{ TAnimal methods }
procedure TAnimal.Speak;
begin
  WriteLn('Animal speaks');
end;

procedure TAnimal.Move;
begin
  WriteLn('Animal moves');
end;

{ TDog methods }
procedure TDog.Speak;
begin
  WriteLn('Dog barks: Woof!');
end;

procedure TDog.Bark;
begin
  WriteLn('Extra bark!');
end;

{ TCat methods }
procedure TCat.Speak;
begin
  WriteLn('Cat meows: Meow!');
end;

procedure TCat.Move;
begin
  WriteLn('Cat sneaks quietly');
end;

var
  Dog: TDog;
  Cat: TCat;
begin
  WriteLn('=== Dynamic Dispatch Unit Test ===');
  WriteLn('');
  
  { Test Dog }
  WriteLn('Testing TDog:');
  Dog := TDog.Create;
  Dog.Name := 1;
  Dog.Breed := 2;
  Dog.Speak;  { Should print "Dog barks: Woof!" }
  Dog.Bark;   { Should print "Extra bark!" }
  WriteLn('');
  
  { Test Cat }
  WriteLn('Testing TCat:');
  Cat := TCat.Create;
  Cat.Name := 3;
  Cat.Color := 4;
  Cat.Speak;  { Should print "Cat meows: Meow!" }
  Cat.Move;   { Should print "Cat sneaks quietly" }
  WriteLn('');
  
  WriteLn('=== Test Complete ===');
end.
