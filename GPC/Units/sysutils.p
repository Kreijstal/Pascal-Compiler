unit SysUtils;

interface

procedure Sleep(milliseconds: integer);
function GetTickCount64: longint;
function MillisecondsBetween(startTick, endTick: longint): longint;

implementation

procedure Sleep(milliseconds: integer);
begin
    asm
        movl $GPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .LSleep_sysv
        movl %ecx, %ecx
        jmp .LSleep_call
.LSleep_sysv:
        movl %edi, %edi
.LSleep_call:
        call gpc_sleep_ms
    end
end;

function GetTickCount64: longint;
begin
    asm
        call gpc_get_tick_count64
    end
end;

function MillisecondsBetween(startTick, endTick: longint): longint;
begin
    asm
        movl $GPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .Lmb_sysv
        movl %edx, %eax
        subl %ecx, %eax
        jmp .Lmb_check
.Lmb_sysv:
        movl %esi, %eax
        subl %edi, %eax
.Lmb_check:
        testl %eax, %eax
        jge .Lmb_done
        negl %eax
.Lmb_done:
    end
end;

end.
