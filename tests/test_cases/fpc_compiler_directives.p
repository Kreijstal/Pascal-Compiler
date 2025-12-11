program FPCCompilerDirectives;
{
  Test FPC compiler directives used in RTL.
  These are preprocessor-like directives that control compilation.
  
  Examples from FPC RTL:
  - ctypes.pp: {$ifdef}, {$ifndef}, {$define}, {$endif}
  - sortbase.pp: {$MODE objfpc}
}

{$IFDEF FPC}
  {$MODE OBJFPC}
  const
    COMPILER_NAME = 'Free Pascal';
{$ELSE}
  const
    COMPILER_NAME = 'Other';
{$ENDIF}

{$IFNDEF TESTING}
  {$DEFINE TESTING}
{$ENDIF}

begin
  WriteLn('Compiler: ', COMPILER_NAME);
  
  {$IFDEF TESTING}
  WriteLn('Testing mode enabled');
  {$ENDIF}
  
  WriteLn('Test completed');
end.
