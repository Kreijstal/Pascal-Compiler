{ Regression: an anonymous `var s: set of <enum>` whose element type has more
  than 32 values must be sized to its true storage width (32 bytes for a set
  spanning ordinals 0..255), not the 4-byte default.

  KGPC resolved the element-type range (and thus the set storage size) only for
  NAMED set type declarations (`type TS = set of TE`), via inherit_alias_metadata
  in the type-declaration path.  An inline `var s: set of TE` never ran that
  resolution, so kgpc_set_storage_size fell back to `return 4` and the variable
  was allocated only 4 bytes.  Every element with ordinal >= 32 was then stored
  out of bounds / read back as garbage, so set operations (intersection,
  membership) on such variables silently produced wrong results.

  This miscompiled FPC's optimiser: `set of TInsChange` (and similar large
  enum sets) lose their high words, so e.g. RegModifiedByInstruction's
  `[...] * insprop[op].Ch <> []` test (flags Ch_WRSP/Ch_RWRSP live in the high
  word) wrongly reports that a stack-pointer-modifying instruction does not
  touch RSP — which let OptPass1MOV rebase prologue register saves from
  off(%rbp) to off(%rsp), clobbering callee-saved registers across calls when
  bootstrapping the RTL at -O2.

  Elements e70/e79/e84 sit in the high qword (ordinal >= 64), exactly where the
  truncation drops them. }
program set_anon_high_ordinal;

type
  TE = (e0,e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,
        e16,e17,e18,e19,e20,e21,e22,e23,e24,e25,e26,e27,e28,e29,e30,e31,
        e32,e33,e34,e35,e36,e37,e38,e39,e40,e41,e42,e43,e44,e45,e46,e47,
        e48,e49,e50,e51,e52,e53,e54,e55,e56,e57,e58,e59,e60,e61,e62,e63,
        e64,e65,e66,e67,e68,e69,e70,e71,e72,e73,e74,e75,e76,e77,e78,e79,
        e80,e81,e82,e83,e84,e85,e86,e87,e88,e89,e90);

var
  s: set of TE;          { anonymous inline set type }

begin
  s := [e79];
  if ([e70, e79, e84] * s) <> [] then
    writeln('intersect-nonempty OK')
  else
    writeln('intersect-nonempty BUG');

  s := [e84];
  if e84 in ([e70, e79, e84] * s) then
    writeln('member OK')
  else
    writeln('member BUG');
end.
