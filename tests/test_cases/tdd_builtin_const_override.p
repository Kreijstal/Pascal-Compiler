{ Regression: KGPC seeds platform-neutral defaults for path-separator
  constants (DirectorySeparator='/', DriveSeparator=':', PathSeparator=';',
  ExtensionSeparator='.') before unit loading.  Previously a subsequent CONST
  declaration of the same identifier from FPC's syswinh.inc (or any RTL) was
  rejected by the CONST+CONST collision rule, leaving the platform-neutral
  default in place and producing garbled Windows-target paths
  (E:\msys64\tmp -> E<NUL>\msys64\tmp after AddPath/DoDirSeparators ran).
  This test redeclares DirectorySeparator and DriveSeparator at program scope
  to verify that the builtin defaults are overridable rather than silently
  shadowing the new declaration. }
program tdd_builtin_const_override;
const
  DirectorySeparator = #92; { '\' = 92, overrides builtin '/' = 47 }
  DriveSeparator = #58;     { ':' = 58, overrides builtin 0 }
begin
  if Ord(DirectorySeparator) <> 92 then
    begin
      WriteLn('FAIL: DirectorySeparator=', Ord(DirectorySeparator),
              ' expected 92');
      Halt(1);
    end;
  if Ord(DriveSeparator) <> 58 then
    begin
      WriteLn('FAIL: DriveSeparator=', Ord(DriveSeparator), ' expected 58');
      Halt(1);
    end;
  WriteLn('OK DirectorySeparator=', Ord(DirectorySeparator),
          ' DriveSeparator=', Ord(DriveSeparator));
end.
