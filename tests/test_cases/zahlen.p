Program zahlen;
Uses math;
Type ZahlenArray = Array of Longint;

var
Positive,Negative,gerade,ungerade,a:ZahlenArray;
c,b,p,n,g,u:Longint;

Begin
writeln('Schreib wie viele Zahlen wollen sie eintippen, danach schreiben Sie die Zahlen.');
read(b);
if (b<=0) then begin writeln('Nur Zahlen > 0'); halt; end;
c:=b;
SetLength(a,b);
SetLength(Positive,b);
SetLength(Negative,b);
SetLength(gerade,b);
SetLength(ungerade,b);
while c>0 do begin 
c:=c-1;
read(a[c]);
if a[c]>=0 then p:=succ(p) else n:=succ(n);
if a[c] mod 2=0 then g:=succ(g) else u:=succ(u);
 end;
p:=0;g:=0;n:=0;u:=0;
for c:=0 to b-1 do
begin
if a[c]>=0 then begin Positive[p]:=a[c];p:=succ(p); end else begin Negative[n]:=a[c];n:=succ(n) end;
if a[c] mod 2=0 then begin gerade[g]:=a[c];g:=succ(g) end else begin ungerade[u]:=a[c];u:=succ(u) end;
end;
writeln('gerade':15,'ungerade':15,'Positive':15,'Negative':15);
for c:=0 to max(p,max(u,max(n,g)))-1 do
begin
if (c<g) then write(gerade[c]:15) else write('':15);
if (c<u) then write(ungerade[c]:15) else write('':15);
if (c<p) then write(positive[c]:15) else write('':15);
if (c<n) then write(negative[c]:15) else write('':15);
writeln;
end;
writeln('Gerade Zahlen');
for c:=0 to g-1 do write(gerade[c],' '); writeln;
writeln('Ungerade Zahlen');
for c:=0 to u-1 do write(ungerade[c],' '); writeln;
writeln('Positive Zahlen');
for c:=0 to p-1 do write(positive[c],' '); writeln;
writeln('Negative Zahlen');
for c:=0 to n-1 do write(negative[c],' '); writeln;



//writeln(p,n,g,u,'help');

end.
