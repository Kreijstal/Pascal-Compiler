unit Crt;

interface

procedure clrscr;
procedure textcolor(color: integer);

implementation

procedure clrscr;
begin
    asm
        call gpc_clrscr
    end
end;

procedure textcolor(color: integer);
begin
    asm
        movl %edi, %edi
        call gpc_textcolor
    end
end;

end.
