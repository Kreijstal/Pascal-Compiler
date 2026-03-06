program tdd_nullptr_operator;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TNullPtr = record
    class operator :=(None: TNullPtr): Pointer; inline;
    class operator :=(None: TNullPtr): TObject; inline;
    class operator <>(LHS: TObject; RHS: TNullPtr): Boolean; inline;
  end;

class operator TNullPtr.:=(None: TNullPtr): Pointer;
begin
  Result := nil;
end;

class operator TNullPtr.:=(None: TNullPtr): TObject;
begin
  Result := nil;
end;

class operator TNullPtr.<>(LHS: TObject; RHS: TNullPtr): Boolean;
begin
  Result := Assigned(LHS);
end;

var
  o: TObject;
  n: TNullPtr;

begin
  o := nil;
  if o <> n then
    writeln('neq')
  else
    writeln('eq');
end.
