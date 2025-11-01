program ExceptionFlow;

var
  steps: integer;

begin
  steps := 0;

  try
    writeln('outer-try');
    try
      writeln('inner-try');
      steps := steps + 1;
      raise 42;
    finally
      writeln('inner-finally');
      steps := steps + 10;
    end;
  except
    writeln('outer-except');
    steps := steps + 100;
  end;

  try
    writeln('rethrow-setup');
    try
      raise 7;
    except
      writeln('inner-except');
      raise;
    end;
  except
    writeln('outer-reraise');
  end;

  try
    try
      raise 5;
    except
      writeln('convert-exception');
      raise 99;
    end;
  except
    writeln('final-handler');
  end;

  writeln(steps);
end.
