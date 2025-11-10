unit Windows;

interface

uses
    ctypes;

function GetHostName: string;
function GetDomainName: string;

implementation

function gpc_windows_get_hostname_string: string; external;
function gpc_windows_get_domainname_string: string; external;

function GetHostName: string;
begin
    GetHostName := gpc_windows_get_hostname_string;
end;

function GetDomainName: string;
begin
    GetDomainName := gpc_windows_get_domainname_string;
end;

end.
