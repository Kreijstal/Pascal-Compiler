program test_nested_finally;

uses SysUtils;

var
  i: integer;

begin
  writeln('Start');
  i := 0;
  try
    writeln('Outer try');
    i := i + 1;
    try
      writeln('Inner try');
      i := i + 10;
    finally
      writeln('Inner finally, i = ', i);
    end;
    writeln('After inner, i = ', i);
  finally
    writeln('Outer finally, i = ', i);
  end;
  writeln('End, i = ', i);
end.
