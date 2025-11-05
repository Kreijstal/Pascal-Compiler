program enum_for_loop;

type Month = (Jan, Feb, Mar);

var m: Month;

begin
  for m := Jan to Mar do
    writeln(ord(m));
end.
