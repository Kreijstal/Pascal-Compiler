{ Test ALUUInt/ALUSInt types - required for FPC compiler units }
program fpc_aluuint_sizeof;

{$mode objfpc}

begin
  WriteLn('ALUUInt size = ', SizeOf(ALUUInt));
  WriteLn('ALUSInt size = ', SizeOf(ALUSInt));
end.
