program tdd_named_large_enum_set_include;
type
  TSwitch = (
    s00,s01,s02,s03,s04,s05,s06,s07,s08,s09,
    s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,
    s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,
    s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,
    s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,
    s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,
    s60,s61,s62,s63,s64,s65,s66,s67,s68,s69
  );
  TSwitches = set of TSwitch;
var
  switches: TSwitches;
begin
  switches := [];
  include(switches, s52);
  if s52 in switches then
    writeln('include ok')
  else
    writeln('include bad');
  exclude(switches, s52);
  if s52 in switches then
    writeln('exclude bad')
  else
    writeln('exclude ok');
end.
