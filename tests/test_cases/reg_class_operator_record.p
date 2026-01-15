program reg_class_operator_record;
{$mode objfpc}
{$modeswitch advancedrecords}

type
  TNullPtr = record
    class operator :=(None: TNullPtr): Pointer; inline;
    class operator =(LHS: TNullPtr; RHS: Pointer): Boolean; inline;
    class operator <>(LHS: TNullPtr; RHS: Pointer): Boolean; inline;
  end;

const
  NullPtr: TNullPtr = ();

type
  TShortIntDynArray = array of ShortInt;

class operator TNullPtr.:=(None: TNullPtr): Pointer;
begin
  Result := nil;
end;

class operator TNullPtr.=(LHS: TNullPtr; RHS: Pointer): Boolean;
begin
  Result := RHS = nil;
end;

class operator TNullPtr.<>(LHS: TNullPtr; RHS: Pointer): Boolean;
begin
  Result := RHS <> nil;
end;

begin
end.
