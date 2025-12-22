unit DateUtils;

interface

uses SysUtils;

function MillisecondsBetween(startTick, endTick: TDateTime): Int64;

implementation

function MillisecondsBetween(startTick, endTick: TDateTime): Int64;
begin
    if endTick >= startTick then
        MillisecondsBetween := Round((endTick - startTick) * 86400000.0)
    else
        MillisecondsBetween := Round((startTick - endTick) * 86400000.0);
end;

end.
