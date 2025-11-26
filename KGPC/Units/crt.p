unit Crt;

interface

procedure clrscr;
procedure textcolor(color: integer);

implementation

procedure clrscr;
begin
    asm
        call kgpc_clrscr
    end
end;

procedure textcolor(color: integer);
begin
    asm
        movl %edi, %edi
        call kgpc_textcolor
    end
end;

end.
