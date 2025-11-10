program string_utils_basic;

uses
    StringUtils;

var
    s: string;
    flag: Boolean;
begin
    s := 'Hello';
    AppendStr(s, ' World');
    writeln('Append=', s);
    StrCut(s, 5);
    writeln('Cut=', s);
    writeln('Count=', StrCount('ana', 'bananas'));
    writeln('Replace=', StrReplace('abc abc', 'abc', 'x'));

    if Char2Boolean('y', flag) then
        writeln('Bool=', flag)
    else
        writeln('Bool=?');
    writeln('Digit=', Char2Digit('F'));
end.
