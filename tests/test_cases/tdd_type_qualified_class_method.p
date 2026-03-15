program tdd_type_qualified_class_method;

type
  TMyClass = class
    class procedure PrintValue(const V: Integer);
    class function NextValue(const V: Integer): Integer;
  end;

class procedure TMyClass.PrintValue(const V: Integer);
begin
  Writeln(V);
end;

class function TMyClass.NextValue(const V: Integer): Integer;
begin
  Result := V + 1;
end;

begin
  TMyClass.PrintValue(41);
  Writeln(TMyClass.NextValue(41));
end.
