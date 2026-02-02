{ Test: include-defined procedure visible in unit initialization }
unit unit_include_init_section;

{$mode objfpc}

interface

implementation

{$i unit_include_init_section.inc}

initialization
  InitExceptions;
end.
