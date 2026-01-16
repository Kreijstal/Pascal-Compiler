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

var
  p: Pointer;
  q: Pointer;

begin
  p := NullPtr;
  q := @p;
  if p = nil then
    WriteLn('PASS: NullPtr converts to nil Pointer')
  else
    WriteLn('FAIL: NullPtr converts to non-nil Pointer');

  if q <> nil then
    WriteLn('PASS: Non-nil pointer setup')
  else
    WriteLn('FAIL: Non-nil pointer setup');

  if (NullPtr = nil) then
    WriteLn('PASS: NullPtr = nil is true')
  else
    WriteLn('FAIL: NullPtr = nil is false');

  if not (NullPtr <> nil) then
    WriteLn('PASS: NullPtr <> nil is false')
  else
    WriteLn('FAIL: NullPtr <> nil is true');

  if not (NullPtr = q) then
    WriteLn('PASS: NullPtr = non-nil pointer is false')
  else
    WriteLn('FAIL: NullPtr = non-nil pointer is true');

  if (NullPtr <> q) then
    WriteLn('PASS: NullPtr <> non-nil pointer is true')
  else
    WriteLn('FAIL: NullPtr <> non-nil pointer is false');
end.
