{
  Test case: Const array of sized ShortString (string[N]) fails.

  BUG: KGPC misinterprets string[N] as 'real' type in const array initializers,
  causing "incompatible types in assignment (lhs: real, rhs: string)".

  This blocks FPC bootstrap because globtype.pas uses:
    modeswitchstr: array[tmodeswitch] of string[30] = (...);

  FPC correctly handles this.
}
program fpc_bootstrap_sized_string_array_const;

const
  Names: array[0..2] of string[10] = ('ONE', 'TWO', 'THREE');

begin
  WriteLn(Names[0]);
  WriteLn(Names[1]);
  WriteLn(Names[2]);
end.
