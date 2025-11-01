program ExceptionReraise;

var
  value: longint;
begin
  value := 0;
  try
    try
      raise 42;
    except
      raise;
    end;
  except
    value := value + 1;
  end;
  writeln(value);
end.
