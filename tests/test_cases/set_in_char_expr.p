program test_set;
type charset = set of char;
var s: charset;
begin
s := ['a', 'b'];
if 'a' in s then
writeln('yes');
end.
