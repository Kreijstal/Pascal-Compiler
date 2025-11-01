program string_indexing;

type
    TStringArray = array[1..2] of string;

    TStringRecord = record
        value: string;
    end;

var
    s: string;
    c: char;
    arr: TStringArray;
    rec: TStringRecord;
begin
    s := 'Pascal';
    writeln('Initial string is: ', s);
    c := s[1];
    writeln('Read character at index 1: ', c);
    s[4] := 'K';
    c := s[4];
    writeln('Read character at index 4 after modify: ', c);
    writeln('Final string is: ', s);

    arr[1] := 'ArrayLit';
    arr[1][5] := 'X';
    writeln('Array element after modify: ', arr[1]);

    rec.value := 'Record';
    rec.value[3] := 'Z';
    writeln('Record field after modify: ', rec.value);
end.
