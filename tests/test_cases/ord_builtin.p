program ord_builtin;

var
    digit: string;
    ascii: longint;
begin
    digit := '7';
    ascii := Ord(digit);
    writeln(ascii);
    writeln(Ord('0'));
    writeln(Chr(Ord('0') + 5));
end.
