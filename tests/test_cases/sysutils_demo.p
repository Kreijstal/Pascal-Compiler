program sysutils_demo;

uses SysUtils;

var
    startTick: longint;
    afterSleep: longint;
    diff: longint;
    sleptOk: longint;

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
end.
