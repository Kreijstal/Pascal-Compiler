{ Regression: under FPC's pp.pas Windows-target compile, the typed-const
  declarations of the form

      system_powerpc64_linux_info : tsysteminfo = ( system : ...; ... );

  in FPCSource/compiler/systems/i_linux.pas pulled the wrong RecordType
  when Win RTL's sysos.inc had a same-named (case-insensitively) record
  TSystemInfo with a totally different field set.  semcheck_decls'
  imported-decl path used semcheck_find_type_node_with_unit_flag_ref,
  which only filtered on the defined_in_unit flag and returned the first
  match — picking System's TSystemInfo over compiler/systems.pas's
  tsysteminfo even though i_linux only directly uses systems.

  This test exercises the structural fix: when multiple units define a
  type with the same name, a typed-const record init must resolve to the
  RecordType from a directly-used unit, not a transitive one. }
{$mode objfpc}
program TypedConstDirectDepRecord;

uses tdd_pp_bootstrap_typed_const_direct_dep_record_systems;

const
  cfg : tcompilercfg = (id : 7; name : 'KGPC');

begin
  WriteLn(cfg.id);
  WriteLn(cfg.name);
end.
