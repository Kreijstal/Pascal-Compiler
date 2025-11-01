program string_indexing;

var
    s: string;
    c: char;
begin
    s := 'Pascal';
    writeln('Initial string is: ', s);
    c := s[1];
    writeln('Read character at index 1: ', c);
    s[4] := 'K';
    c := s[4];
    writeln('Read character at index 4 after modify: ', c);
    writeln('Final string is: ', s);
end.
