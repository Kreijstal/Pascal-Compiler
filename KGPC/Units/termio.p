unit termio;

interface

function IsATTY(Handle: LongInt): LongInt;
function IsATTY(var t: text): LongInt;

implementation

function c_isatty(fd: LongInt): LongInt; cdecl; external name 'isatty';

function IsATTY(Handle: LongInt): LongInt;
begin
    IsATTY := c_isatty(Handle);
end;

function IsATTY(var t: text): LongInt;
begin
    { Stub: assume not a TTY }
    IsATTY := 0;
end;

end.
