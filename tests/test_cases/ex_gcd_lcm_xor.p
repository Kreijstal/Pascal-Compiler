Program ex;
var
a,b,c,i:Integer;
d:Boolean;
begin
        d:=True;
        writeln('Tippe 2 zahlen');
        read(a);
        read(b);
        if((a=0) or (b=0)) then begin writeln('Kein division durch null Auszuführen'); halt;end;
        if a>b then begin a:=a xor b;b:=a xor b;a:=a xor b; end;
        i:=b+1;
        if (a<0) then begin writeln('Keine negative Zahlen, bitte');halt; end;
        while ((d) or (i<0)) do begin
                i:=i-1;
                if ((a mod i=0) and (b mod i=0)) then d:=false;
        end;
        if d then writeln('GGT nicht gefunden') else writeln('GGT ist: ',i);
        if b mod a=0 then writeln('KGV ist: ',b) else  writeln('KGV ist: ',b*a);

end.