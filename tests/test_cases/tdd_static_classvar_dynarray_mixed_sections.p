{ Regression: a `var Foo: array of T; static;` field is a class-wide (static)
  class variable, exactly like `class var Foo: array of T;`.  KGPC only marked
  fields declared under a `class var` section as class vars; a `var ... static`
  field was never flagged, and was only reachable by bare name inside a static
  method through an "expose every field" fallback that applied when the class
  had NO real `class var` section.  As soon as the class ALSO had a `class var`
  section (FPC's TEncoding in rtl/objpas/sysutils/sysencodingh.inc declares
  both: a `var FSystemEncodings: array of TEncoding; static;` and a
  `class var FLock`), the fallback switched off and the dynamic-array static
  field vanished from scope — SetLength/Length/High/Low on it then failed with
  "first argument to SetLength must be a dynamic array variable", which broke
  compiling the FPC 3.2.2 RTL.  This mirrors that exact shape. }
program tdd_static_classvar_dynarray_mixed_sections;
{$mode objfpc}{$H+}
type
  TEnc = class
  strict private
    var
      FSys: array of TEnc; static;   { static class var via `static` directive }
    class var
      FCount: longint;               { a real `class var` section as well }
  public
    class procedure Grow; static;
    class function Count: longint; static;
  end;

class procedure TEnc.Grow;
begin
  SetLength(FSys, Length(FSys) + 1);
  if High(FSys) <> Low(FSys) then
    FSys[High(FSys)] := FSys[Low(FSys)];
  FSys[Low(FSys)] := nil;
  Inc(FCount);
end;

class function TEnc.Count: longint;
begin
  Count := Length(FSys);
end;

begin
  TEnc.Grow;
  TEnc.Grow;
  TEnc.Grow;
  writeln(TEnc.Count);
end.
