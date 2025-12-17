unit Keyboard;

interface

type
    TKeyEvent = LongInt;

function PollKeyEvent: TKeyEvent;
function GetKeyEvent: TKeyEvent;

implementation

function kgpc_keyboard_poll: LongInt; external;
function kgpc_keyboard_get: LongInt; external;

function PollKeyEvent: TKeyEvent;
begin
    PollKeyEvent := kgpc_keyboard_poll();
end;

function GetKeyEvent: TKeyEvent;
begin
    GetKeyEvent := kgpc_keyboard_get();
end;

end.
