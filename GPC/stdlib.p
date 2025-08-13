program stdlib;

procedure WriteStringLn(s: string);
begin
    assembler;
    asm
        call puts
    end
end;

procedure WriteIntLn(i: integer);
begin
    assembler;
    asm
        call print_integer
    end
end;

begin
end.
