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
        if a>b then begin 
          c:=a;
          a:=b;
          b:=c;
        end;
        i:=b;
        if (a<0) then begin writeln('Keine negative Zahlen, bitte');halt; end;
        while (d and (i>0)) do begin
                if ((a mod i=0) and (b mod i=0)) then begin
                  d:=false;
                end else begin
                  i:=i-1;
                end;
        end;
        if d then writeln('GGT nicht gefunden') else writeln('GGT ist: ',i);
        if i>0 then begin
          if b mod a=0 then writeln('KGV ist: ',b) else  writeln('KGV ist: ', (b div i) * a);
        end;
end.