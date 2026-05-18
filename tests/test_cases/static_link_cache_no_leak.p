{ Regression test: codegen used to leak a callee-save register from its
  static-link cache when a procedure body ended while the cache still held
  a register acquired by a nested helper.  The next subprogram's
  reset_reg_stack() then printed "WARNING: Not all registers freed" and,
  worse, ctx->static_link_reg dangled into freed memory — codegen could
  hand the same Register_t back out at a matching depth, miscompiling the
  next function. }
program static_link_cache_no_leak(output);

  var
    total : longint;

  procedure outer;
    var
      base : longint;

    procedure middle;
      var
        offset : longint;

      procedure inner;
      begin
        { Three levels deep — codegen_acquire_static_link caches a
          callee-save reg.  This is the body whose end used to leave the
          cache populated. }
        total := base + offset
      end;

    begin
      offset := 7;
      inner;
      offset := offset + 1;
      inner
    end;

  begin
    base := 10;
    middle;
    base := base + 1;
    middle
  end;

  procedure neighbor;
    var
      local : longint;

    procedure helper;
    begin
      { After outer/inner ran, codegen used to enter `helper` with a stale
        ctx->static_link_reg pointing into freed memory.  This call shape
        is what made bootstrapped pp_stage2 emit wrong bytes for
        tnodetreetypedconstbuilder.parse_arraydef. }
      total := total + local
    end;

  begin
    local := 100;
    helper
  end;

begin
  total := 0;
  outer;
  neighbor;
  writeln(total)
end.
