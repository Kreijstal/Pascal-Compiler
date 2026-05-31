{$mode objfpc}
{$H+}
program tdd_string_compare_value_context;
var
  a, s : ansistring;
  ss : shortstring;
  b : boolean;
  cond : boolean;
begin
  a := 'rcx';
  s := 'rcx';
  ss := 'rcx';
  cond := true;

  { ansistring = ansistring, value context (right operand was passed by
    address instead of value before the fix) }
  b := (a = s);
  writeln('ansi=ansi equal -> ', b);
  s := 'rdx';
  b := (a = s);
  writeln('ansi=ansi diff  -> ', b);

  { shortstring = ansistring, value context }
  s := 'rcx';
  b := (ss = s);
  writeln('short=ansi equal -> ', b);
  s := 'rdx';
  b := (ss = s);
  writeln('short=ansi diff  -> ', b);

  { ansistring = shortstring, value context }
  s := 'rcx';
  b := (s = ss);
  writeln('ansi=short equal -> ', b);
  s := 'rdx';
  b := (s = ss);
  writeln('ansi=short diff  -> ', b);

  { (cond) and (x = y) shape — short-circuit AND with string compare }
  s := 'rcx';
  b := cond and (a = s);
  writeln('cond and ansi=ansi equal -> ', b);
  s := 'rdx';
  b := cond and (a = s);
  writeln('cond and ansi=ansi diff  -> ', b);
end.
