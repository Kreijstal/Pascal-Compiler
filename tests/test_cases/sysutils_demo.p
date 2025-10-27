program sysutils_demo;

uses SysUtils;

var
    startTick: longint;
    afterSleep: longint;
    diff: longint;
    sleptOk: longint;
    demoStart: longint;
    demoEnd: longint;

begin
    startTick := GetTickCount64();
    Sleep(1);
    afterSleep := GetTickCount64();
    demoStart := 42;
    demoEnd := 10;
    diff := MillisecondsBetween(demoStart, demoEnd);
    writeln(diff);
    if MillisecondsBetween(startTick, afterSleep) < 0 then
        sleptOk := 0
    else
        sleptOk := 1;
    writeln(sleptOk);
end.
