{$mode objfpc}
{$H+}

program test_nested_exception_blocks;

uses
  SysUtils;

var
  x: Integer = -1;
  y: Integer = 0;

begin
  Writeln('Before outer try');
  try
    try
      Writeln('Inside inner try');
      x := 42;
      y := 100;
      Writeln('x=', x, ', y=', y);
    except
      on E: Exception do
        Writeln('Caught in inner except: ', E.Message);
    end;
    Writeln('After inner except');
  finally
    Writeln('In finally block, x=', x, ', y=', y);
  end;
  Writeln('After outer finally');
  Writeln('Done');
end.
