program tdd_empty_except_handler_swallows;
{ Regression: an except handler with an empty body still catches and swallows
  the matching exception.  KGPC used to collapse a try..except whose handler
  produced no statements (`except end` or `on E do ;`) down to just its try
  body, removing the exception barrier — so the raise escaped and went
  unhandled.  This is exactly FPC's compiler abort idiom
  (compiler.pas: `on ECompilerAbort do begin try Message(...) except
  on ECompilerAbort do ; end end`), which made pp.pas crash with an
  "Unhandled exception" instead of reporting a clean fatal error. }
{$mode objfpc}{$H+}
uses sysutils;

var
  passed, failed: integer;

procedure check(const name: string; ok: boolean);
begin
  if ok then
  begin
    writeln('[PASS] ', name);
    inc(passed);
  end
  else
  begin
    writeln('[FAIL] ', name);
    inc(failed);
  end;
end;

procedure boom;
begin
  raise Exception.Create('boom');
end;

var
  reached: boolean;

begin
  passed := 0;
  failed := 0;

  { 1: a bare `except end` with no statements must still swallow. }
  reached := false;
  try
    boom;
  except
  end;
  reached := true;
  check('bare empty except swallows', reached);

  { 2: an empty typed handler `on E do ;` must still swallow. }
  reached := false;
  try
    boom;
  except
    on Exception do ;
  end;
  reached := true;
  check('empty on-handler swallows', reached);

  { 3: empty nested handler inside an outer handler — the pp.pas abort
       pattern.  `reached` is set only if the nested empty handler caught
       the second raise instead of letting it escape. }
  reached := false;
  try
    boom;
  except
    on Exception do
      begin
        try
          boom;
        except
          on Exception do ;
        end;
        reached := true;
      end;
  end;
  check('nested empty handler swallows', reached);

  writeln('Passed: ', passed);
  writeln('Failed: ', failed);
  if failed = 0 then
    writeln('All tests passed!')
  else
    writeln('Some tests failed!');
end.
