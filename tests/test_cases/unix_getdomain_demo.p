program unix_getdomain_demo;

{$ifdef MSWINDOWS}
uses Windows;
{$else}
uses Unix;
{$endif}

begin
    writeln(GetDomainName);
end.
