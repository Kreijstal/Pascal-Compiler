program dlg;

uses crt;

procedure talk(speaker:integer; dialogue:string);
begin;
  textcolor(speaker);
  if (speaker = 1) then
      write('Shopkeeper')
  else if (speaker = 2) then
      write('Joe Black ');
  write(': ');
  textcolor(7);
  writeln(dialogue);
  writeln;
end;

begin;
  clrscr;
  writeln('Fictional dialogue.');
  writeln;
  writeln('-----');
  writeln;
  talk(1,'Good morning, sir. May I help you?');
  talk(2,'I''m looking for a girl.');
  talk(1,'Aren''t we all?');
  talk(2,'No, I mean she works here. Do you know Sandra?');
  talk(1,'Of course. Let me call her.');
  readln;
end.
