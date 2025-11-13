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

    if AnsiContainsStr('Pascal', 'sca') then
        writeln('AnsiContainsStr=TRUE')
    else
        writeln('AnsiContainsStr=FALSE');

    if AnsiContainsText('Pascal', 'SCAL') then
        writeln('AnsiContainsText=TRUE')
    else
        writeln('AnsiContainsText=FALSE');

    if AnsiStartsStr('Pas', 'Pascal') then
        writeln('AnsiStartsStr=TRUE')
    else
        writeln('AnsiStartsStr=FALSE');

    if AnsiStartsText('pas', 'Pascal') then
        writeln('AnsiStartsText=TRUE')
    else
        writeln('AnsiStartsText=FALSE');

    if AnsiEndsStr('cal', 'Pascal') then
        writeln('AnsiEndsStr=TRUE')
    else
    writeln('AnsiEndsStr=FALSE');

    if AnsiEndsText('CAL', 'Pascal') then
        writeln('AnsiEndsText=TRUE')
    else
        writeln('AnsiEndsText=FALSE');

    if ContainsStr('Pascal', 'pas') then
        writeln('ContainsStr=TRUE')
    else
        writeln('ContainsStr=FALSE');

    if ContainsText('Pascal', 'pas') then
        writeln('ContainsText=TRUE')
    else
        writeln('ContainsText=FALSE');

    if StartsStr('Hello', 'Hello World') then
        writeln('StartsStr=TRUE')
    else
        writeln('StartsStr=FALSE');

    if StartsText('hello', 'Hello World') then
        writeln('StartsText=TRUE')
    else
        writeln('StartsText=FALSE');

    if EndsStr('World', 'Hello World') then
        writeln('EndsStr=TRUE')
    else
        writeln('EndsStr=FALSE');

    if EndsText('world', 'Hello World') then
        writeln('EndsText=TRUE')
    else
        writeln('EndsText=FALSE');

    writeln('Dupe=', DupeString('ab', 3));
    writeln('AnsiReplaceStr=', AnsiReplaceStr('Foo foo', 'Foo', 'Bar'));
    writeln('AnsiReplaceText=', AnsiReplaceText('Foo foo', 'FOO', 'Bar'));
end.
