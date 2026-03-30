program test_pp_readargs;

{$mode objfpc}

uses
  globals, options, systems, verbose, symtable, tokens, comphook,
  i_linux, cputarg;

begin
  WriteLn('[TEST] source_info.system = ', Ord(source_info.system));
  WriteLn('[TEST] source_info.shortname = "', source_info.shortname, '"');
  WriteLn('[TEST] target_info before InitSystems = "', target_info.shortname, '"');
  InitSystems;
  WriteLn('[TEST] target_info.system = ', Ord(target_info.system));
  WriteLn('[TEST] target_info.shortname = "', target_info.shortname, '"');
  InitFileUtils;
  InitGlobals;
  InitVerbose;
  inittokens;
  InitSymtable;
  do_initSymbolInfo;
  set_current_module(nil);
  WriteLn('[TEST] calling read_arguments');
  read_arguments('');
  WriteLn('[TEST] read_arguments returned!');
end.
