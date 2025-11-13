program inrange_function;

uses Math;

begin
  writeln('InRangeTrue=', Ord(InRange(5, 1, 10)));
  writeln('InRangeFalse=', Ord(InRange(0, 1, 10)));
  writeln('InRangeSwap=', Ord(InRange(5, 10, 1)));
  writeln('InRangeLong=', Ord(InRange(5000000000, -1, 6000000000)));
end.
