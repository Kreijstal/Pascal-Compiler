program unix_gethostname_demo;

{$ifdef MSWINDOWS}
uses Windows;
{$else}
uses Unix;
{$endif}

begin
    writeln(GetHostName);
end.
