{ Regression test: strlen function overload resolution }
{ FPC RTL baseunix.pp requires strlen() to work with @array[0] }
{ This test documents a regression introduced in commit 5687905 }
{ At the good commit (9d96c77), baseunix.pp compiled without errors }
unit regression_fpc_strlen_overload;

interface

type
  SizeInt = Int64;
  
  utsname_t = record
    domain: array[0..64] of AnsiChar;
  end;

{ strlen should accept PAnsiChar }
function strlen(p: PAnsiChar): SizeInt; external name 'FPC_PCHAR_LENGTH';

function GetDomainLength(var u: utsname_t): SizeInt;

implementation

function GetDomainLength(var u: utsname_t): SizeInt;
begin
  { This pattern from FPC's bunxsysc.inc fails }
  Result := strlen(@u.domain[0]);
end;

end.
