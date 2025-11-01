program crt_colour_list;

uses Crt;

var
    counter: integer;

begin
    ClrScr;
    for counter := 0 to 15 do
    begin
        TextColor(counter);
        writeln(counter);
    end;
    Crt.ReadLn;
end.
