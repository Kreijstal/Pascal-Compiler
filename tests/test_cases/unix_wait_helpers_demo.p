program unix_wait_helpers_demo;

uses Unix, ctypes;

function sys_fork: cint; external name 'fork';

var
    pid: cint;
    wait_result: cint;
    status: integer;
begin
    pid := sys_fork();
    if pid = 0 then
    begin
        halt(7);
    end
    else if pid > 0 then
    begin
        wait_result := WaitProcess(pid);
        writeln(wait_result);
        status := W_STOPCODE(19);
        if WIFSTOPPED(status) then
            writeln(1)
        else
            writeln(0);
        writeln(W_EXITCODE(12, 0));
    end
    else
    begin
        writeln(-1);
        writeln(0);
        writeln(0);
    end;
end.
