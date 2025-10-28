program sysutils_demo;

uses SysUtils;

var
    startTick: longint;
    afterSleep: longint;
    diff: longint;
    sleptOk: longint;
    duration: TDateTime;
    formatted: string;
    digits: string;
    startNow: TDateTime;
    endNow: TDateTime;
    clockOk: longint;
    src: array[0..2] of longint;
    dest: array[0..2] of longint;
    idx: longint;
    bigUint: UInt64;
    nextUint: Uint64;

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

    duration := 12345;
    formatted := FormatDateTime('NN:SS.ZZZ', duration);
    writeln(formatted);

    digits := IntToStr(-4096);
    writeln(digits);

    src[0] := 7;
    src[1] := 8;
    src[2] := 9;
    dest[0] := 0;
    dest[1] := 0;
    dest[2] := 0;
    Move(src[0], dest[0], 3 * SizeOf(src[0]));
    for idx := 0 to 2 do
        writeln(dest[idx]);

    bigUint := 4294967295;
    nextUint := bigUint - 1;
    writeln(bigUint);
    writeln(nextUint);

    startNow := Now();
    Sleep(1);
    endNow := Now();
    if (endNow - startNow) >= 0 then
        clockOk := 1
    else
        clockOk := 0;
    writeln(clockOk);
end.
