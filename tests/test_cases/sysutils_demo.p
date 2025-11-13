program sysutils_demo;

uses SysUtils;

var
    startTick: longint;
    afterSleep: longint;
    diff: longint;
    sleptOk: longint;
    sample: AnsiString;

begin
    startTick := GetTickCount64();
    Sleep(1);
    afterSleep := GetTickCount64();
    diff := MillisecondsBetween(42, 10);
    writeln(diff);
    if MillisecondsBetween(startTick, afterSleep) < 0 then
        sleptOk := 0
    else
        sleptOk := 1;
    writeln(sleptOk);
    sample := '  Pascal  ';
    writeln('Trim=', Trim(sample));
    writeln('TrimLeft=', TrimLeft(sample));
    writeln('TrimRight=', TrimRight(sample));
    writeln('AnsiUpper=', AnsiUpperCase('Pascal'));
    writeln('AnsiLower=', AnsiLowerCase('Pascal'));
    writeln('CompareText=', CompareText('AbC', 'abc'));
    writeln('SameText=', SameText('One', 'oNe'));
end.
