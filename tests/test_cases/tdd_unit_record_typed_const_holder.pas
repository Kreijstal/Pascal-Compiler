{ Holder unit that declares the typed const inside its IMPLEMENTATION section.

  TFoo lives in tdd_unit_record_typed_const_unit, which this unit imports
  privately (implementation `uses`).  A consumer program that imports only
  this holder will not have a direct view of TFoo, exercising the
  cross-unit-impl typed-const-init bug. }

unit tdd_unit_record_typed_const_holder;

interface

procedure dump;

implementation

uses tdd_unit_record_typed_const_unit;

function fa_impl(arg: byte): byte;
begin
  fa_impl := arg + 1;
end;

procedure fb_impl(arg: pointer);
begin
end;

const
  defaults: TFoo = (
    page_size:  $11111111;
    image_base: $22222222;
    code:       333;
    flag:       true;
    bytes:      (1, 2, 3, 4, 5);
    fa:         @fa_impl;
    fb:         @fb_impl;
    p1:         nil;
    p2:         nil;
  );

procedure dump;
begin
  writeln('page_size=', defaults.page_size);
  writeln('image_base=', defaults.image_base);
  writeln('code=', defaults.code);
  writeln('flag=', defaults.flag);
  writeln('byte0=', defaults.bytes[kind_a]);
  writeln('byte4=', defaults.bytes[kind_e]);
  writeln('fa_nil=', defaults.fa = nil);
  writeln('fb_nil=', defaults.fb = nil);
  writeln('p1_nil=', defaults.p1 = nil);
end;

end.
