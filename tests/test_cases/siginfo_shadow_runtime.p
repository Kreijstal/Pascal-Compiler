program siginfo_shadow_runtime;
{$mode objfpc}{$H+}

uses BaseUnix, Unix;

type
  tsiginfo = record
    oldsiginfo: sigactionrec;
    hooked: Boolean;
  end;

var
  NativeInfo: BaseUnix.tsiginfo;
  InfoPtr: psiginfo;

begin
  FillChar(NativeInfo, SizeOf(NativeInfo), 0);
  NativeInfo.si_code := 1;
  InfoPtr := @NativeInfo;

  if InfoPtr^.si_code <> 1 then
    Writeln('unexpected')
  else
    Writeln('ok');
end.
