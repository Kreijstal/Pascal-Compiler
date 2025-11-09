program nested_proc_multiple_args;
{ Test for nested procedures with multiple arguments at different nesting levels.
  Tests that static link handling works correctly when there are multiple arguments. }
var
  GlobalVar1, GlobalVar2: Integer;
  Result: Integer;

procedure Level1Proc(x: Integer; y: Integer);
begin
  Result := GlobalVar1 + GlobalVar2 + x + y;
end;

procedure OuterProc;
var
  OuterVar1, OuterVar2: Integer;
  
  procedure MiddleProc;
  var
    MiddleVar1, MiddleVar2: Integer;
    
    procedure InnerProc;
    begin
      { Call Level1Proc from depth 3, passing variables from depth 2 }
      Level1Proc(MiddleVar1, MiddleVar2);
    end;
    
  begin
    MiddleVar1 := 100;
    MiddleVar2 := 200;
    InnerProc;
  end;
  
begin
  OuterVar1 := 10;
  OuterVar2 := 20;
  GlobalVar1 := 1;
  GlobalVar2 := 2;
  MiddleProc;
end;

begin
  OuterProc;
  writeln('Result: ', Result);
end.
