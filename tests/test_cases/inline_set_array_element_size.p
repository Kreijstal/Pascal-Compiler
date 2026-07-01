program inline_set_array_element_size;
{ Regression: an anonymous inline `set of <named enum>` used as a typed-const
  array element type must be sized as a full 32-byte set (the enum has >32
  values), not collapsed to a 4-byte small set.

  The typed const lives inside a procedure on purpose: a procedure-local
  typed const's parse-time element KgpcType is run through the predeclare
  type-cache clear, which used to destroy the only durable record of the
  inline set's element enum.  The real semantic pass then had no `type_id`
  to rebuild it from and degraded the element to a generic 4-byte small set,
  so the array was allocated 4*4 = 16 bytes (not 4*32 = 128) and indexed with
  a 4-byte stride.  WriteOps[i] therefore read garbage / overran the next
  global -- the root cause of FPC's RegModifiedByInstruction (aoptx86)
  failing to detect operand register modifications, which miscompiled the
  optimized (-O2) FPC self-build. }
type
  TE = (e0,e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,
        e16,e17,e18,e19,e20,e21,e22,e23,e24,e25,e26,e27,e28,e29,e30,e31,
        e32,e33,e34,e35,e36,e37,e38,e39,e40,e41,e42,e43,e44,e45,e46,e47,
        e48,e49,e50,e51,e52,e53,e54,e55,e56,e57,e58,e59,e60,e61,e62,e63,
        e64,e65,e66,e67,e68,e69,e70,e71,e72,e73,e74,e75,e76,e77,e78,e79,
        e80,e81,e82,e83,e84,e85,e86,e87,e88,e89,e90,e91,e92,e93,e94,e95);

procedure P;
const
  WriteOps: array[0..3] of set of TE =
    ([e80,e81,e82],[e84,e85,e86],[e88,e89,e90],[e92,e93,e94]);
var
  i: integer;
  ch: set of TE;
begin
  writeln('sizeof=', sizeof(WriteOps));
  ch := [e85];           { only matches WriteOps[1] }
  for i := 0 to 3 do
    if (WriteOps[i] * ch) <> [] then
      writeln('match ', i)
    else
      writeln('no match ', i);
end;

begin
  P;
end.
