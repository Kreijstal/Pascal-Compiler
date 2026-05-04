{ Regression: function-local real consts must resolve in codegen-cache mode.

  Cache-miss codegen pushes a fresh local scope per subprogram and
  re-registers its const decls.  The pre-fix codegen_register_const_decls
  only handled int/string/set, leaving local real consts unresolved at
  use site:

    ERROR: Unresolved non-local symbol DELTA reached codegen fallback.
    WARNING: Not all registers freed, 1 still remaining!

  Covers two real-const flavors that surface in FPC RTL math.pp / genmath.inc:
    - bare real literal:   DELTA = 0.001
    - typed real literal:  DP1 = double(7.85398125648498535156E-1)
}
program tdd_local_real_const_codegen_cache;

function ComputeNudge(x: Double): Double;
const
  DELTA = 0.001;
  EPS = 1E-9;
  DP1 = double(7.85398125648498535156E-1);
var
  r: Double;
begin
  r := x + DELTA + DP1;
  if abs(r) < EPS then r := 0.0;
  ComputeNudge := r;
end;

begin
  writeln(ComputeNudge(0.5):0:6);
end.
