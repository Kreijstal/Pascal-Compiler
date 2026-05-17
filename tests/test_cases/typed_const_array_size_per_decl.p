{ Regression test: two units declare typed-const arrays of the same
  NAME and same element type but DIFFERENT bounds.  Before the fix,
  codegen pulled total_size from a cross-unit symtab lookup whose
  KgpcType carried whichever declaration's bounds happened to be
  visible — so the .comm allocation for one unit's array was sized
  using the OTHER unit's bounds.

  The companion Python test inspects the emitted asm and asserts each
  unit's .comm has its own correct byte count
  (8 entries * 4 bytes = 32 for unit_a, 4 entries * 4 bytes = 16 for
  unit_b).  Same root cause as the FPC pp.pas charmap miscompile where
  every cp*.reversemap was sized using cpX's bound. }
program typed_const_array_size_per_decl;

uses
  typed_const_array_size_per_decl_unit_a,
  typed_const_array_size_per_decl_unit_b;

begin
end.
