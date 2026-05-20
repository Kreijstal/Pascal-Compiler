{ Regression: typed-const initialiser of a >4-byte set (32-byte storage for
  set-of-enum with >32 members) used to store only the rodata-literal
  pointer into bytes [0..7] of the variable and leave the remaining 24
  bytes uninitialised.  Pp.pas's pgenutil.pas:91 typed-const
    tgeneric_param_nodes : tnodetypeset = [typen, ordconstn, stringconstn,
                                           realconstn, setconstn, niln];
  was mis-initialised by KGPC, so the subsequent IN test inside
  parse_generic_specialization_types_internal misclassified type-nodes as
  non-type and emitted "Type identifier expected" at every
  `specialize TFoo<Integer>` in user programs. }

program tdd_typed_const_large_enum_set;

{$mode objfpc}

type
  tcolor = (red, green, blue, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13,
            c14, c15, c16, c17, c18, c19, c20, c21, c22, c23, c24, c25,
            c26, c27, c28, c29, c30, c31, c32, c33, c34, c35, c36, c37,
            c38, c39, c40, c41, c42, c43, c44, c45);
  tcolset = set of tcolor;

const
  myset : tcolset = [red, c44];

var
  c : tcolor;

begin
  c := red;
  if c in myset then writeln('red yes') else writeln('red no');
  c := green;
  if c in myset then writeln('green yes') else writeln('green no');
  c := c44;
  if c in myset then writeln('c44 yes') else writeln('c44 no');
  c := c45;
  if c in myset then writeln('c45 yes') else writeln('c45 no');
end.
