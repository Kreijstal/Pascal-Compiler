{$mode objfpc}
{$H+}
program fpc_bootstrap_const_string_param_shadow;

{ Regression for the FPC RTL FNMatch miscompile that broke directory
  enumeration during the FPC 3.2.2 bootstrap.

  `Pattern` is also declared as a GLOBAL shortstring.  Inside FNM the
  parameter `const Pattern: string` is a managed AnsiString and must be
  read by value (movq -> kgpc_string_length).  Codegen used to promote the
  parameter to shortstring because an unrelated same-named global shortstring
  was visible in an outer scope, so Length(Pattern) loaded the variable's
  address instead of its value and returned garbage, making FNM return False. }

var
  Pattern: string[20];   { global shortstring, same name as the param }

function FNM(const Pattern, Name: string): Boolean;
begin
  FNM := (Length(Pattern) = 1) and (Length(Name) = 3);
end;

begin
  Pattern := 'global-shortstring';
  if FNM('*', 'foo') then
    WriteLn('match')
  else
    WriteLn('nomatch');
end.
