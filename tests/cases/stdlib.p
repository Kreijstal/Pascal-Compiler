program stdlib;

procedure write(s: string);
begin
    assembler;
    asm
        call print_string
    end
end;

procedure write(i: integer);
begin
    assembler;
    asm
        call print_integer_no_newline
    end
end;

procedure writeln(s: string);
begin
    assembler;
    asm
        call print_string
    end
end;

procedure writeln(i: integer);
begin
    assembler;
    asm
        call print_integer
    end
end;

procedure writeln(i: longint);
begin
    assembler;
    asm
        call print_integer
    end
end;

procedure writeln;
begin
    assembler;
    asm
        call print_newline
    end
end;

procedure read(var i: integer);
begin
    assembler;
    asm
        call read_integer
    end
end;

procedure read(var i: longint);
begin
    assembler;
    asm
        call read_integer
    end
end;


begin
    // The data section is now hardcoded in the code generator.
end.
