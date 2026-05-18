{ Regression test: Exit(string_literal) in a shortstring-returning
  function must write the literal into the caller's hidden SRET
  buffer, not just park its address in %rax.

  Without the fix, codegen's scalar Exit path only updated %rax,
  leaving the SRET buffer uninitialised — callers saw an empty
  string.

  This mirrors FPC's pattern in ninl.pas get_str_int_func:
    function get_str_int_func(def: tdef): string;
    begin
      ...
      if is_nativeuint(def) then exit('uint') else exit('sint');
    end;
  Without the fix, pp_bootstrap built the procname 'fpc_shortstr_'
  with no suffix, and the system unit's compilerproc lookup failed
  with "Unknown compilerproc fpc_shortstr_". }
{$mode objfpc}
{$H-}
program tdd_exit_with_string_literal;

function getname(kind: integer): string;
begin
  result := 'preset_';
  case kind of
    1: exit('alpha');
    2: exit('beta');
    3: exit('gamma');
  end;
  result := result + 'fallthrough';
end;

begin
  writeln('[', getname(1), ']');
  writeln('[', getname(2), ']');
  writeln('[', getname(3), ']');
  writeln('[', getname(0), ']');
end.
