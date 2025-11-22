program test_constructor_expressions;

type
    MyClass = class
        val: Integer;
        constructor Create(v: Integer);
        function GetVal: Integer;
    end;

constructor MyClass.Create(v: Integer);
begin
    Self.val := v;
end;

function MyClass.GetVal: Integer;
begin
    GetVal := Self.val;
end;

procedure ProcessObj(obj: MyClass);
begin
    WriteLn('ProcessObj: ', obj.GetVal);
end;

var
    obj: MyClass;

begin
    { Context 1: Assignment }
    obj := MyClass.Create(10);
    WriteLn('Assignment: ', obj.GetVal);

    { Context 2: Function Parameter }
    ProcessObj(MyClass.Create(20));

    { Context 3: Method Chaining (Nested Expression) }
    WriteLn('Chaining: ', MyClass.Create(30).GetVal);

    { Context 4: If Condition (checking non-nil) }
    if MyClass.Create(40) <> nil then
        WriteLn('If Condition: Created successfully');
end.
