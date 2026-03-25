unit charset;

interface

type
    tunicodechar = word;
    tunicodestring = ^tunicodechar;

    tunicodecharmappingflag = (umf_noinfo, umf_leadbyte, umf_undefined, umf_unused);

    punicodecharmapping = ^tunicodecharmapping;
    tunicodecharmapping = packed record
        unicode: tunicodechar;
        flag: tunicodecharmappingflag;
        reserved: byte;
    end;

    preversecharmapping = ^treversecharmapping;
    treversecharmapping = packed record
        unicode: tunicodechar;
        char1: Byte;
        char2: Byte;
    end;

    punicodemap = ^tunicodemap;
    tunicodemap = record
        cpname: string[20];
        cp: word;
        map: punicodecharmapping;
        lastchar: longint;
        reversemap: preversecharmapping;
        reversemaplength: longint;
        next: punicodemap;
        internalmap: boolean;
    end;

function loadunicodemapping(const cpname, f: string; cp: word): punicodemap;
procedure registermapping(p: punicodemap);
function registerbinarymapping(const directory, cpname: string): Boolean;
function getmap(const s: string): punicodemap;
function getmap(cp: word): punicodemap;
function mappingavailable(const s: string): boolean;
function mappingavailable(cp: word): boolean;
function getunicode(c: Char; p: punicodemap): tunicodechar;
function getascii(c: tunicodechar; p: punicodemap): string;

implementation

function loadunicodemapping(const cpname, f: string; cp: word): punicodemap;
begin
    loadunicodemapping := nil;
end;

procedure registermapping(p: punicodemap);
begin
end;

function registerbinarymapping(const directory, cpname: string): Boolean;
begin
    registerbinarymapping := False;
end;

function getmap(const s: string): punicodemap;
begin
    getmap := nil;
end;

function getmap(cp: word): punicodemap;
begin
    getmap := nil;
end;

function mappingavailable(const s: string): boolean;
begin
    mappingavailable := False;
end;

function mappingavailable(cp: word): boolean;
begin
    mappingavailable := False;
end;

function getunicode(c: Char; p: punicodemap): tunicodechar;
begin
    getunicode := Ord(c);
end;

function getascii(c: tunicodechar; p: punicodemap): string;
begin
    getascii := Chr(c and $FF);
end;

end.
