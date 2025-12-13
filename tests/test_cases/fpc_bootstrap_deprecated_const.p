{ Test for FPC bootstrap compatibility: deprecated directive on constants }
{ This feature is required by FPC's rtlconsts.pp unit }
program fpc_bootstrap_deprecated_const;

const
  { Standard constant }
  NormalConst = 'normal value';
  
  { FPC allows 'deprecated' directive after constant values }
  DeprecatedConst = 'deprecated value' deprecated;
  
  { FPC also allows deprecated with a message }
  DeprecatedConstWithMsg = 'old value' deprecated 'Use NewConst instead';
  
  { Other FPC directives on constants }
  PlatformConst = 'platform value' platform;

var
  s: string;
  
begin
  { Verify constants are accessible and have correct values }
  WriteLn(NormalConst);
  WriteLn(DeprecatedConst);
  WriteLn(DeprecatedConstWithMsg);
  WriteLn(PlatformConst);
end.
