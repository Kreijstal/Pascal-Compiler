program Gleichung;
var a,b,c:Integer;
d:Real;
Begin
writeln('Schreib die Koeffizienten von eine ax^2+bx+c=0 gleichung, (3 Zahlen)');
read(a,b,c);
d:=(b*b)-(4*a*c);
if(d<0) then begin writeln('Keine Positiv Lösung gefunden, Diskriminant:',d); halt; end;
writeln ('Erste Lösung:',(-b-sqrt(d))/(2*a));
writeln ('Zweite Lösung:',(-b+sqrt(d))/(2*a));
end.