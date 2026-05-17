{ Regression: VMT must emit signature-mangled symbols for ALL overloaded   }
{ virtual methods sharing the same Pascal name.  Previously KGPC bound     }
{ both overload slots to the FIRST candidate during the template-refresh   }
{ pass, leaving the second slot's resolved_mangled_id == NULL.  Codegen    }
{ then fell back to the bare ClassName__Method form, which the linker      }
{ never saw a `.globl` for — undefined reference at link time.             }
{$mode objfpc}
program vmt_overloaded_virtual_dispatch;

type
  TBase = class
    procedure Do_(x: longint); overload; virtual;
    procedure Do_(x: ansistring); overload; virtual;
  end;

procedure TBase.Do_(x: longint);
begin
  writeln('int=', x);
end;

procedure TBase.Do_(x: ansistring);
begin
  writeln('str=', x);
end;

var
  b: TBase;
begin
  b := TBase.Create;
  b.Do_(42);
  b.Do_('hi');
  b.Free;
end.
