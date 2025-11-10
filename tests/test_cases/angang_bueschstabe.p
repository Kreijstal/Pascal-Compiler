program AngangBueschstabe;

uses
    SysUtils;

var
    Name, Vorname: String;

begin
    Write('Vorname, bitte:');
    readln(vorname);
    WriteLn();
    Write('Name, bitte:');
    readln(name);
    WriteLn;
    WriteLn('Der Name beginnt mit:', UpperCase(Name[1]));
    WriteLn('Der Vorname beginnt mit:', UpperCase(Vorname[1]));
end.
