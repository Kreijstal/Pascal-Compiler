unit Crt;

interface

procedure ClrScr;
procedure TextColor(color: integer);
procedure ReadLn;

implementation

procedure ClrScr;
begin
    asm
        call gpc_clrscr
    end;
end;

procedure TextColor(color: integer);
begin
    asm
        movl %edi, %edi
        call gpc_textcolor
    end;
end;

procedure ReadLn;
begin
    asm
        call gpc_readln
    end;
end;

end.
