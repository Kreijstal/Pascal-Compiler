{$mode objfpc}
{$modeswitch advancedrecords}
program tdd_nullptr_tobject_eq;

type
  TNullPtr = record
    class operator =(LHS: TNullPtr; RHS: TObject): Boolean; inline;
    class operator =(LHS: TObject; RHS: TNullPtr): Boolean; inline;
    class operator <>(LHS: TNullPtr; RHS: TObject): Boolean; inline;
    class operator <>(LHS: TObject; RHS: TNullPtr): Boolean; inline;
  end;

class operator TNullPtr.=(LHS: TNullPtr; RHS: TObject): Boolean;
begin
  Result := not Assigned(RHS);
end;

class operator TNullPtr.=(LHS: TObject; RHS: TNullPtr): Boolean;
begin
  Result := not Assigned(LHS);
end;

class operator TNullPtr.<>(LHS: TNullPtr; RHS: TObject): Boolean;
begin
  Result := Assigned(RHS);
end;

class operator TNullPtr.<>(LHS: TObject; RHS: TNullPtr): Boolean;
begin
  Result := Assigned(LHS);
end;

var
  N: TNullPtr;
  O: TObject;
begin
  O := nil;
  if (N = O) and not (N <> O) then
    WriteLn('ok')
  else
    WriteLn('bad');
end.
